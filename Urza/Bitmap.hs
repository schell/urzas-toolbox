module Urza.Bitmap where

import Urza.Types
import Urza.Texture


loadBitmap :: FilePath -> IO (Maybe Bitmap)
loadBitmap file = do
    mTex <- loadTexture file
    case mTex of
        Nothing -> return Nothing
        Just t  -> do
           sizeOfTexture t >>= return . Just . Bitmap t


