import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Graphics.GLUtil
import Graphics.GLUtil.JuicyTextures

import Data.Colour.SRGB.Linear hiding (blend)
import Data.Colour hiding (blend)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

import Data.IORef

import System.Random
import System.Exit

texname = "sprite.png"
winSize = Size 800 800

type ParticleList = [Particle]
data Particle = Particle { particleHue :: GLfloat,
							particleSat :: GLfloat,
							particleVal :: GLfloat,
							particleSize :: GLfloat,
							particlePosx :: GLfloat,
							particlePosy :: GLfloat,
							particlePosz :: GLfloat,
							particleVelx :: GLfloat,
							particleVely :: GLfloat,
							particleVelz :: GLfloat,
							particleTTL :: Int} deriving (Eq, Show, Read)

gravity = 0.0098 :: GLfloat

main :: IO ()
main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "Particle Test"
  plist <- newIORef ([] :: ParticleList)
  windowSize $= winSize
  displayCallback $= (display plist)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  idleCallback $= Just (idle plist)
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

drawParticle :: Particle -> IO ()
drawParticle p = do
				color $ getGlColour (particleHue p) (particleSat p) (particleVal p)
				texCoord $ TexCoord2 z z
				vertex $ Vertex3 (particlePosx p - particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				texCoord $ TexCoord2 z o
				vertex $ Vertex3 (particlePosx p - particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o o
				vertex $ Vertex3 (particlePosx p + particleSize p) (particlePosy p + particleSize p) (particlePosz p)
				texCoord $ TexCoord2 o z
				vertex $ Vertex3 (particlePosx p + particleSize p) (particlePosy p - particleSize p) (particlePosz p)
				where
				o = 1 :: GLfloat
				z = 0 :: GLfloat

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

display :: IORef ParticleList -> IO ()
display plist = do
  clear [ ColorBuffer ]
  -- withTexture2D [spritetex] renderPrimitive Quads $ buildBox 0.2
  particles <- get plist
  mapM_ (renderPrimitive Quads . drawParticle) particles
  --renderPrimitive Quads $ drawParticle (Particle { particleColor = Color3 1.0 1.0 1.0, particleSize = 0.2, particlePosx = 0, particlePosy = 0 }) 
  flush
  postRedisplay Nothing -- Force GLUT to refresh screen

idle :: IORef ParticleList -> IO ()
idle plist = do
			particles <- get plist
			let updateparticles = map updateParticle particles
			newparticles <- sequence (replicate 3 newParticle)
			plist $= filter (\p -> particleTTL p >= 0)  (newparticles ++ updateparticles)


updateParticle :: Particle -> Particle
updateParticle p =
				p { particleVely = particleVely p - gravity,
					particlePosx = particlePosx p + particleVelx p,
					particlePosy = particlePosy p + particleVely p,
					particlePosz = particlePosz p + particleVelz p,
					particleTTL = particleTTL p - 1,
					particleSize = particleSize p * 0.9}

randomList :: Random a => (a, a) -> Int -> IO [a]
randomList bounds n = do
		sequence $ replicate n (randomRIO bounds)

newParticle :: IO Particle
newParticle = do
			hue <- randomRIO (0,60)
			psize <- randomRIO (0.01, 0.1) 
			pvx <- randomRIO (-0.02, -0.08)
			pvy <- randomRIO (0.1, 0.2)
			pvz <- randomRIO (-0.2,-0.2)
			ptime <- randomRIO (0, 5000)
			return $ Particle { particlePosx = 1,
							particlePosy = -1,
							particlePosz = 1,
							particleHue = hue,
							particleSat = 1,
							particleVal = 1,
							particleSize = psize,
							particleVelx = pvx,
							particleVely = pvy,
							particleVelz = pvz,
							particleTTL = ptime}

getGlColour :: GLfloat -> GLfloat -> GLfloat  -> Color3 GLfloat
getGlColour h s v = Color3 (channelRed rgbcol) (channelGreen rgbcol) (channelBlue rgbcol)
			where
			rgbcol = hsv h s v
