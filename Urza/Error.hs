module Urza.Error where

import Graphics.Rendering.OpenGL
import System.IO (hPutStrLn, stderr)

-- | Prints any accumulated OpenGL errors.
printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)



