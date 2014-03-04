{-# LANGUAGE OverloadedStrings #-}
module Urza.Shader  (
    makeShaderProgram,
    bindAndBufferVertsUVs,
    bindAndBufferVertsColors
) where

import           Urza.Error
import           Urza.Types
import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Foreign
import           System.Exit (exitFailure)
import           Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import qualified Data.ByteString as B


sizeOfList :: [GLfloat] -> GLsizeiptr
sizeOfList = fromIntegral . (* sizeOf (undefined :: GLfloat)) . length


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s


makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled


-- | Compiles, validates and returns a shader for rendering text with.
makeShaderProgram :: IO ShaderProgram
makeShaderProgram = do
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [("position", positionLocation), ("color", colorLocation), ("uv", uvLocation) ]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"
    sLoc <- get $ uniformLocation p "sampler"
    cLoc <- get $ uniformLocation p "rColor"
    tLoc <- get $ uniformLocation p "isTextured"
    rLoc <- get $ uniformLocation p "isColorReplaced"

    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr
        updateSampler = (uniform sLoc $=)
        updateColor   = (uniform cLoc $=)
        updateIsReplaced rpl = uniform rLoc $= (Index1 $ if rpl then 1 else 0 :: GLint)

    return ShaderProgram { _program = p
                         , _setProjection = updatePJ
                         , _setModelview = updateMV
                         , _setSampler = updateSampler
                         , _setIsTextured = updateIsTextured tLoc
                         , _setTextColor = updateColor
                         , _setColorIsReplaced = updateIsReplaced
                         }


-- | Updates the shader to accept either uv coords if textured or color
-- values if not. Assumes a shader program is set as the current
-- program.
updateIsTextured :: UniformLocation -> Bool -> IO ()
updateIsTextured uloc isTextured = do
    when isTextured $ do
        vertexAttribArray colorLocation $= Disabled
        vertexAttribArray uvLocation $= Enabled

    unless isTextured $ do
        vertexAttribArray colorLocation $= Enabled
        vertexAttribArray uvLocation $= Disabled

    uniform uloc $= (Index1 $ if isTextured then 1 else 0 :: GLint)


-- | GLSL Source code for a vertex shader that can draw simple colored and
-- textured geometry or text.
vertSrc :: B.ByteString
vertSrc = B.concat [ "attribute vec2 position;\n"
                   , "attribute vec4 color;\n"
                   , "attribute vec2 uv;\n"
                   , "\n"
                   , "varying vec2 vTex;\n"
                   , "varying vec4 vColor;\n"
                   , "\n"
                   , "uniform mat4 modelview;\n"
                   , "uniform mat4 projection;\n"
                   , "\n"
                   , "void main () {\n"
                   , "    vTex = uv;\n"
                   , "    vColor = color;\n"
                   , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);\n"
                   , "}\n"
                   ]


-- | GLSL Source code for a fragment shader that can draw simple colored and
-- textured geometry or text.
fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec2 vTex;"
    , "varying vec4 vColor;"
    , ""
    , "uniform sampler2D sampler;"
    , "uniform vec4 rColor;"
    , "uniform bool isTextured;"
    , "uniform bool isColorReplaced;"
    , ""
    , "void main() {"
    , "    if (isTextured) {"
    , "        vec4 tc = texture2D(sampler, vec2(vTex.s, vTex.t));"
    , "        if (isColorReplaced) {"
    , "            gl_FragColor = vec4(rColor.r, rColor.g, rColor.b, tc.r);"
    , "        } else {"
    , "            gl_FragColor = tc;"
    , "        }"
    , "    } else {"
    , "        if (isColorReplaced) {"
    , "            gl_FragColor = rColor;\n"
    , "        } else {"
    , "            gl_FragColor = vColor;"
    , "        }"
    , "    }"
    , "}"
    ]


positionLocation :: AttribLocation
positionLocation = AttribLocation 0


colorLocation :: AttribLocation
colorLocation = AttribLocation 1


uvLocation :: AttribLocation
uvLocation = AttribLocation 2


-- | Vertex descriptor for the vertex shader.
vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


-- | UV descriptor for a tex vertex shader.
uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


-- | Color descriptor for a shape vertex shader.
colorDescriptor :: VertexArrayDescriptor [Float]
colorDescriptor = VertexArrayDescriptor 4 Float 0 nullPtr


-- | Binds and buffers vertices and uv coords to be used with a text
-- shader. This assumes that a text shader program is the current program.
bindAndBufferVertsUVs :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsUVs vts uvs = do
    [i,j] <- genObjectNames 2

    bindVBO i vertDescriptor positionLocation
    withArray vts $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList vts, ptr, StaticDraw)

    bindVBO j uvDescriptor uvLocation
    withArray uvs $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList uvs, ptr, StaticDraw)

    return (i,j)


bindAndBufferVertsColors :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsColors vts cs = do
    [i,j] <- genObjectNames 2

    bindVBO i vertDescriptor positionLocation
    withArray vts $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList vts, ptr, StaticDraw)

    bindVBO j colorDescriptor colorLocation
    withArray cs $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList cs, ptr, StaticDraw)

    return (i,j)
