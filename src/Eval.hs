module Eval (updateGameState, initGameState) where
import Types (GameState (GameState),Position (..),
              Color(..),PieceType(..))
import Fen (parseFEN)
import Event (EventData)
import Data.Int (Int8)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Data.Bits

initGameState :: Color -> GameState
initGameState color = GameState (pieces color) 0 color

pieces :: Color -> [Word64]
pieces White = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
pieces Black = parseFEN "PPPPPPPP/RNBQKBNR/8/8/8/8/rnbqkbnr/pppppppp"

updateGameState :: GameState -> Position -> GameState
updateGameState gs@(GameState ps ms col) pos = gs
{-
tryMove :: Piece -> Position -> GameState -> GameState
tryMove pi pos gs@(GameState _ pMvs _ _) = if pos `elem` pMvs
                                            then changePosition pi pos gs
                                          else gs
-}
