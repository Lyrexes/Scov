module Main where
import System.Environment (getArgs)
import qualified SDL
import qualified SDL.Font as SDLF
import Linear (V2(..), V4 (V4))
import Data.Text (pack)
import SDL (defaultRenderer, ($=), initialize)
import Control.Concurrent (threadDelay)
import Foreign.C (CInt)
import Control.Monad (unless)
import Args (parseArgs,Arg(..))
import Event (fetchEvents, ChessEvent(..), Command(..))
import Draw(drawChess,getPieceSurface)
import Types(GameState, Color (White), PieceType (..))
import Openings (readOpenings)

------------------------------
-- Window and board constants
windowSize :: Int
windowSize =  800

tileAmount :: CInt
tileAmount = 8
------------------------------

main :: IO()
main = do
    args <- getArgs
    openings <- readOpenings
    case parseArgs args openings of
        Msg x -> putStr x
        O (name,opening) -> initOpening name opening windowSize
        OWS (name,opening) s -> initOpening name opening s

--------------------
--initialize Opening
initOpening :: String -> [GameState] -> Int -> IO()
initOpening name opening s = do
            let size = toEnum s
            putStrLn ("now viewing, best match: " ++ name)
            initGraphic
            icon <- getIconSurface
            sur <- createSurface size
            win <- createWindow name (SDL.V2 size size)
            ren <- createRenderer sur
            SDL.setWindowIcon win icon
            loop win size ren sur 0 opening
            cleanUp ren win
--------------------

--------------------
getIconSurface :: IO SDL.Surface
getIconSurface = do
            sur <- createSurface 400
            ren <- createRenderer sur
            getPieceSurface ren 20 KingB
-------------------

--------------------------------
--initialize and create Grahpics
initGraphic :: IO()
initGraphic = do
  SDL.initialize [SDL.InitVideo]
  SDLF.initialize

createWindow :: String -> SDL.V2 CInt -> IO SDL.Window
createWindow title size = do
  window <- SDL.createWindow
    (pack title)
    SDL.defaultWindow {SDL.windowInitialSize = size,
                       SDL.windowResizable = False}
  SDL.showWindow window
  return window

createRenderer :: SDL.Surface -> IO SDL.Renderer
createRenderer = SDL.createSoftwareRenderer

createSurface :: CInt -> IO SDL.Surface
createSurface size = SDL.createRGBSurface (SDL.V2 size size) SDL.RGB24
--------------------------------

------------------------------------------
-- Game loop drawing and processing events
loop :: SDL.Window -> CInt -> SDL.Renderer -> SDL.Surface -> Int -> [GameState] -> IO()
loop window size renderer surface index gameStates = do
    event <- fetchEvents

    let newIndex  = case cmd event of
            PrevMove -> if index > 0 then index-1 else index
            NextMove -> if index < length gameStates-1 then index+1 else index
            None     -> index
        gameState = gameStates !! newIndex

    draw window size renderer surface  gameState

    unless (quit event) (loop window size renderer surface newIndex gameStates)
------------------------------------------

-----------------
-- Draws the game
draw :: SDL.Window -> CInt -> SDL.Renderer -> SDL.Surface -> GameState-> IO()
draw window size renderer surface gameState = do
    screen <- SDL.getWindowSurface window
    SDL.updateWindowSurface window

    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 0
    SDL.clear renderer

    drawChess window renderer tileAmount size gameState

    SDL.present renderer


    SDL.updateWindowSurface window

    _ <- SDL.surfaceBlit surface Nothing screen Nothing

    SDL.freeSurface screen
-----------------

---------------------------
-- Clean up all SDl grahics
cleanUp :: SDL.Renderer -> SDL.Window -> IO()
cleanUp renderer window = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
---------------------------
