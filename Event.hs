module Event (EventData(..), fetchEvents, regulateFPS) where
import qualified SDL
import Linear (V2)
import Foreign.C (CInt)
import Control.Concurrent (threadDelay)
import SDL (Event(eventPayload))
import Data.Int (Int8)
import Types (Position(..), pointToPosition)

data EventData = EventData {
  quit :: Bool,
  mouseCords :: Maybe Position
}

fetchEvents :: CInt -> IO EventData
fetchEvents tileAmount = do
  event <- SDL.waitEvent
  mCords <- SDL.getAbsoluteMouseLocation
  mouseButtons <- SDL.getMouseButtons 
  if mouseButtons SDL.ButtonLeft 
    then return $ EventData {
      quit = SDL.QuitEvent == eventPayload event,
      mouseCords = Just (pointToPosition ((`div` tileAmount) <$> mCords))
    }
    else return $ EventData {
      quit = SDL.QuitEvent == eventPayload event,
      mouseCords = Nothing
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
