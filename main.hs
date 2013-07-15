import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.GLUtil
import Graphics.GLUtil.JuicyTextures

import System.Exit

texname = "sprite.png"
winSize = Size 800 800

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Particle Test"
  windowSize $= winSize
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  spritetex <- loadTex
  mainLoop
 
reshape s@(Size w h) = do
	viewport $= (Position 0 0, s)
	postRedisplay Nothing

keyboardMouse (Char '\ESC') Down _ _ = exitSuccess
keyboardMouse key state mods pos = putStrLn $ show key 

extract :: (IO (Either String a)) -> IO a
extract act = do
				e <- act
				case e of
					Left err -> error err
					Right val -> return val

buildBox :: GLfloat -> IO ()
buildBox w = do
	color $ Color3 o z z
	texCoord $ TexCoord2 z z
	vertex $ Vertex2 n n
	texCoord $ TexCoord2 z o
	vertex $ Vertex2 n w
	texCoord $ TexCoord2 o o
	vertex $ Vertex2 w w
	texCoord $ TexCoord2 o z
	vertex $ Vertex2 w n
	where
	o = 1 :: GLfloat
	z = 0 :: GLfloat
	n = -w

loadTex = do
		imgresult <- readTexture texname
		finaltexture <- extract $ readTexInfo texname loadTexture
		texture Texture2D $= Enabled
		activeTexture $= TextureUnit 0
		textureBinding Texture2D $= Just finaltexture
		textureFilter   Texture2D   $= ((Linear', Just Nearest), Linear')
		textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
		textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
		blend $= Enabled
		blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
		generateMipmap' Texture2D
		return finaltexture

display :: IO ()
display = do
  clear [ ColorBuffer ]
  -- withTexture2D [spritetex] renderPrimitive Quads $ buildBox 0.2
  renderPrimitive Quads $ buildBox 0.2
  flush
