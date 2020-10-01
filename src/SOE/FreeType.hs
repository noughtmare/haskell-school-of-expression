{-# LANGUAGE LambdaCase #-}
module SOE.FreeType where

-- Based on https://learnopengl.com/In-Practice/Text-Rendering

import qualified Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL      ( ($=) )
import           FreeType
import           Concurrent
import           Paths_SOE
import           Foreign
import           GHC.Stack
import           System.IO.Unsafe
import           Data.Maybe
import           Data.Traversable
import Data.Int

data Character = Character
  { charTexture :: GL.TextureObject
  , charSize    :: (Int32, Int32)
  , charBearing :: (Int32, Int32)
  , charAdvance :: Int32
  }

fontRef :: MVar (Maybe [(Char, Character)])
fontRef = unsafePerformIO (newMVar Nothing)

getFont :: IO [(Char, Character)]
getFont = do
  readMVar fontRef >>= \case
    Just font -> return font
    Nothing   -> do
      font <- loadFont
      modifyMVar_ fontRef (\_ -> return (Just font))
      return font

loadFont :: IO [(Char, Character)]
loadFont = do
  ft       <- ft_Init_FreeType
  fontFile <- getDataFileName "fonts/DejaVuSans.ttf"
  face     <- ft_New_Face ft fontFile 0

  ft_Set_Pixel_Sizes face 0 16

  GL.rowAlignment GL.Unpack $= 1

  for [0 .. 127] $ \c -> do
    ft_Load_Char face c FT_LOAD_RENDER
    texture <- GL.genObjectName
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just texture
    glyph <- peek . frGlyph =<< peek face
    let bitmap = gsrBitmap glyph
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      GL.Luminance'
      (GL.TextureSize2D (fromIntegral (bWidth bitmap))
                        (fromIntegral (bRows bitmap))
      )
      0
      (GL.PixelData GL.Luminance GL.UnsignedByte (bBuffer bitmap))
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    return
      ( toEnum (fromIntegral c)
      , Character
        texture
        (fromIntegral (bWidth bitmap), fromIntegral (bRows bitmap))
        ( fromIntegral (gsrBitmap_left glyph)
        , fromIntegral (gsrBitmap_top glyph)
        )
        (fromIntegral (vX (gsrAdvance glyph)))
      )

renderString :: HasCallStack => String -> IO ()
renderString str = do
  font <- getFont
  GL.texture GL.Texture2D $= GL.Enabled
  GL.preservingMatrix $ mapM_ (renderChar . fromJust . (`lookup` font)) str
  GL.texture GL.Texture2D $= GL.Disabled
 where
  renderChar :: Character -> IO ()
  renderChar (Character font (w, h) _ adv)
    = do
      GL.textureBinding GL.Texture2D $= Just font
      GL.preservingMatrix $ GL.renderPrimitive GL.Quads $ do
        GL.texCoord (GL.TexCoord2 0 (0 :: Int32))
        GL.vertex (GL.Vertex3 0 h 0)
        GL.texCoord (GL.TexCoord2 0 (1 :: Int32))
        GL.vertex (GL.Vertex3 0 0 (0 :: Int32))
        GL.texCoord (GL.TexCoord2 1 (1 :: Int32))
        GL.vertex (GL.Vertex3 w 0 0)
        GL.texCoord (GL.TexCoord2 1 (0 :: Int32))
        GL.vertex (GL.Vertex3 w h 0)
      GL.translate (GL.Vector3 ((fromIntegral adv / 64) :: Float) 0 0)
