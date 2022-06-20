module Main where
import qualified SDL
import qualified SDL.Font as SDLF
import Linear (V2(..), V4 (V4))
import Data.Text (pack)
import SDL (defaultRenderer, ($=))
import Control.Concurrent (threadDelay)
import Foreign.C (CInt)
import Control.Monad (unless)
import Event ( EventData(quit, mouseCords), fetchEvents, regulateFPS )
import Draw(drawChess)
import Types(GameState, Color (White))
import Eval (initGameState, updateGameState)

windowSize :: CInt
windowSize =  600

tileAmount :: CInt
tileAmount = 8

tileSize :: CInt
tileSize = div 600 8

main :: IO()
main = do
  initGraphic
  sur <- createSurface
  win <- createWindow "chess" (SDL.V2 windowSize windowSize)
  ren <- createRenderer sur
  loop win ren sur (initGameState White)
  cleanUp ren win

loop :: SDL.Window -> SDL.Renderer -> SDL.Surface -> GameState -> IO()
loop window renderer surface currGameState = do
  start <- SDL.ticks

  events <- fetchEvents tileSize

  let mousePos  = mouseCords events
      gameState = maybe currGameState (updateGameState currGameState) mousePos

  draw window renderer surface gameState

  end <- SDL.ticks
  regulateFPS 60 (fromIntegral start) (fromIntegral end)
  unless (quit events) (loop window renderer surface gameState)


cleanUp :: SDL.Renderer -> SDL.Window -> IO()
cleanUp renderer window = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


createWindow :: String -> SDL.V2 CInt -> IO SDL.Window
createWindow title size = do
  window <- SDL.createWindow
    (pack title)
    SDL.defaultWindow {SDL.windowInitialSize = size}
  SDL.showWindow window
  return window

createRenderer :: SDL.Surface -> IO SDL.Renderer
createRenderer = SDL.createSoftwareRenderer

createSurface :: IO SDL.Surface
createSurface = SDL.createRGBSurface (SDL.V2 windowSize windowSize) SDL.RGB24

draw :: SDL.Window -> SDL.Renderer -> SDL.Surface -> GameState-> IO()
draw window renderer surface gameState = do
  screen <- SDL.getWindowSurface window

  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0
  SDL.clear renderer

  drawChess window renderer tileAmount windowSize gameState

  SDL.present renderer

  SDL.updateWindowSurface window

  _ <- SDL.surfaceBlit surface Nothing screen Nothing

  SDL.freeSurface screen

initGraphic :: IO()
initGraphic = do
  SDL.initialize [SDL.InitVideo]
  SDLF.initialize
