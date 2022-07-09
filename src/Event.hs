module Event (ChessEvent(..), fetchEvents, regulateFPS) where
import qualified SDL
import Linear (V2)
import Foreign.C (CInt)
import Control.Concurrent (threadDelay)
import SDL (Event(eventPayload))
import Data.Int (Int8)
import Types (Position(..), pointToPosition)

data ChessEvent = ChessEvent {
    quit :: Bool,
    leftArrow :: Bool,
    rightArrow :: Bool
}
    deriving Show


fetchEvents :: IO ChessEvent
fetchEvents = do
    event <- SDL.waitEvent
    case eventPayload event of
        SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) -> return $ ChessEvent {
                quit = False,
                leftArrow = SDL.keysymKeycode keysym == SDL.KeycodeLeft,
                rightArrow = SDL.keysymKeycode keysym == SDL.KeycodeRight
            }
        x -> return $ ChessEvent {
                quit = SDL.QuitEvent == x,
                leftArrow = False,
                rightArrow = False
            }


regulateFPS :: Int -> Int -> Int -> IO()
regulateFPS 0 start end = return ()
regulateFPS fps start end =
  let
    ticksPerFrame = div 1000 fps
    done = end - start
    delta = ticksPerFrame - done
    in
      threadDelay (max 0 delta * 1000)
