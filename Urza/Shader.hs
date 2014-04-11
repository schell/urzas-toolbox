{-# LANGUAGE OverloadedStrings #-}
module Urza.Shader  (
    makeShaderProgram,
    withVertsAndColors,
    withVerts3AndColors,
    bindAndBufferVerts3UVs,
    bindAndBufferVerts3Colors,
    bindAndBufferVertsUVs,
    bindAndBufferVertsColors,
    bindAndBufferMany,
    bindAndBuffer,
    sizeOfFloatList,
    sizeOfIntList,
    sizeOfUByteList,
    withArray
) where

import           Urza.Error
import           Urza.Types
import           Urza.Math
import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Foreign
import           System.Exit (exitFailure)
import           Graphics.Rendering.OpenGL.Raw (glUniformMatrix4fv)
import qualified Data.ByteString as B


withVertsAndColors :: [GLfloat] -> [GLfloat] -> IO () -> IO ()
withVertsAndColors vs cs f = do
    (i, j) <- bindAndBufferVertsColors vs cs
    f
    deleteObjectNames [i,j]


withVerts3AndColors :: [GLfloat] -> [GLfloat] -> IO () -> IO ()
withVerts3AndColors vs cs f = do
    (i, j) <- bindAndBufferVerts3Colors vs cs
    f
    deleteObjectNames [i,j]


sizeOfFloatList = sizeOfList


sizeOfList :: [GLfloat] -> GLsizeiptr
sizeOfList = fromIntegral . (* sizeOf (undefined :: GLfloat)) . length


sizeOfIntList :: [GLint] -> GLsizeiptr
sizeOfIntList = fromIntegral . (* sizeOf (undefined :: GLint)) . length


sizeOfUByteList :: [GLubyte] -> GLsizeiptr
sizeOfUByteList = fromIntegral . (* sizeOf (undefined :: GLubyte)) . length


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


-- | Compiles, validates and returns a shader for rendering text with.
makeShaderProgram :: IO ShaderProgram
makeShaderProgram = do
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [ ("position", positionLocation)
                           , ("position3", position3Location)
                           , ("color", colorLocation)
                           , ("uv", uvLocation)
                           ]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"
    sLoc <- get $ uniformLocation p "sampler"
    cLoc <- get $ uniformLocation p "rColor"
    tLoc <- get $ uniformLocation p "isTextured"
    rLoc <- get $ uniformLocation p "isColorReplaced"
    i3Loc <- get $ uniformLocation p "is3d"

    let updateMV mat = withArray (toList mat) $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray (toList mat) $ \ptr ->
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
                         , _setIs3d = updateIs3d i3Loc
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


-- | Updates the shader to accept either 2d or 3d vertex coords
-- Assumes a shader program is set as the current
-- program.
updateIs3d :: UniformLocation -> Bool -> IO ()
updateIs3d uloc is3d = do
    when is3d $ do
        vertexAttribArray positionLocation $= Disabled
        vertexAttribArray position3Location $= Enabled

    unless is3d $ do
        vertexAttribArray positionLocation $= Enabled
        vertexAttribArray position3Location $= Disabled

    uniform uloc $= (Index1 $ if is3d then 1 else 0 :: GLint)


-- | GLSL Source code for a vertex shader that can draw simple colored and
-- textured geometry or text.
vertSrc :: B.ByteString
vertSrc = B.concat [ "attribute vec2 position;\n"
                   , "attribute vec3 position3;\n"
                   , "attribute vec4 color;\n"
                   , "attribute vec2 uv;\n"
                   , "\n"
                   , "varying vec2 vTex;\n"
                   , "varying vec4 vColor;\n"
                   , "\n"
                   , "uniform mat4 modelview;\n"
                   , "uniform mat4 projection;\n"
                   , "uniform bool is3d;\n"
                   , "\n"
                   , "void main () {\n"
                   , "    vTex = uv;\n"
                   , "    vColor = color;\n"
                   , "    vec4 pos;\n"
                   , "    if (is3d) {\n"
                   , "        pos = vec4(position3, 1.0);\n"
                   , "    } else {\n"
                   , "        pos = vec4(position, 0.0, 1.0);\n"
                   , "    }\n"
                   , "    gl_Position = projection * modelview * pos;\n"
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

position3Location :: AttribLocation
position3Location = AttribLocation 1


colorLocation :: AttribLocation
colorLocation = AttribLocation 2


uvLocation :: AttribLocation
uvLocation = AttribLocation 3


-- | Vertex descriptor for the vertex shader.
vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


vert3Descriptor :: VertexArrayDescriptor [Float]
vert3Descriptor = VertexArrayDescriptor 3 Float 0 nullPtr


-- | UV descriptor for a tex vertex shader.
uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


-- | Color descriptor for a shape vertex shader.
colorDescriptor :: VertexArrayDescriptor [Float]
colorDescriptor = VertexArrayDescriptor 4 Float 0 nullPtr


bindAndBufferVerts3UVs :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVerts3UVs vts uvs = do
    [i,j] <- bindAndBufferMany [ (vts, vert3Descriptor, position3Location)
                               , (uvs, uvDescriptor, uvLocation)
                               ]
    return (i,j)


bindAndBufferVerts3Colors :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVerts3Colors vts cs = do
    [i, j] <- bindAndBufferMany [ (vts, vert3Descriptor, position3Location)
                                , (cs, colorDescriptor, colorLocation)
                                ]
    return (i,j)


-- | Binds and buffers vertices and uv coords to be used with a text
-- shader. This assumes that a text shader program is the current program.
bindAndBufferVertsUVs :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsUVs vts uvs = do
    [i,j] <- bindAndBufferMany [ (vts, vertDescriptor, positionLocation)
                               , (uvs, uvDescriptor, uvLocation)
                               ]
    return (i,j)


bindAndBufferVertsColors :: [GLfloat] -> [GLfloat] -> IO (BufferObject, BufferObject)
bindAndBufferVertsColors vts cs = do
    [i, j] <- bindAndBufferMany [ (vts, vertDescriptor, positionLocation)
                                , (cs, colorDescriptor, colorLocation)
                                ]
    return (i,j)


bindAndBufferMany :: [([GLfloat], VertexArrayDescriptor [Float], AttribLocation)] -> IO [BufferObject]
bindAndBufferMany = mapM (\(dats, vd, loc) -> bindAndBuffer dats vd loc)


bindAndBuffer :: [GLfloat] -> VertexArrayDescriptor [Float] -> AttribLocation -> IO BufferObject
bindAndBuffer dats vd loc = do
    i <- genObjectName
    bindVBO i vd loc
    withArray dats $ \ptr -> do
        bufferData ArrayBuffer $= (sizeOfList dats, ptr, StaticDraw)
    return i


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled

