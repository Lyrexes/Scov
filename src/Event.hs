module Event (ChessEvent(..), Command(..),
              fetchEvents) where
import qualified SDL
import SDL (Event(eventPayload))

data Command = PrevMove | NextMove | None
    deriving (Show,Enum,Eq,Ord)

data ChessEvent = ChessEvent {
    quit :: Bool,
    cmd :: Command
}
    deriving Show

-------------------------------------
-- fetch events
-- fetchEvents :: () -> ChessEvent
fetchEvents :: IO ChessEvent
fetchEvents = do
    event <- SDL.waitEvent
    case eventPayload event of
        SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ keysym)
            -> return $ ChessEvent {
                quit = False,
                cmd  = fetchKeys keysym
            }
        x -> return $ ChessEvent {
                quit = SDL.QuitEvent == x,
                cmd  = None
            }
-------------------------------------

-------------------------------------
-- fetch keys
-- fetchKeys :: KeySym -> Command
fetchKeys :: SDL.Keysym -> Command
fetchKeys keysym = case (SDL.keysymKeycode keysym == SDL.KeycodeLeft
                      || SDL.keysymKeycode keysym == SDL.KeycodeH
                     ,SDL.keysymKeycode keysym == SDL.KeycodeRight
                      || SDL.keysymKeycode keysym == SDL.KeycodeL) of
                    (True, False)  -> PrevMove
                    (False, True)  -> NextMove
                    _              -> None
