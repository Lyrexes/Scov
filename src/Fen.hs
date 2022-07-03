module Fen (parseFEN) where
import Data.Binary (Word64)
import Data.Bits
import Data.Char(isDigit)
import Debug.Trace (trace)
import Types(PieceType(..), GameState(..))

-----------------------------------------------
-- parses FEN String
-- parseFEN :: FENString -> [PieceTypePostions]
parseFEN :: String -> [Word64]
parseFEN str = [figure x fen | x <- [minBound..maxBound]] where
    fen = withOutDelim str

withOutDelim :: String -> String
withOutDelim = filter ('/'/=)
-----------------------------------------------

-----------------------------------------------
-- parses FEN of one PieceType figure
-- PieceType -> FENString -> PieceTypePositions
figure :: PieceType -> [Char] -> Word64
figure pT fen = figH pT 0 fen 0 where
    figH _ _ [] bb = bb
    figH pT i (x:xs) bb = figH pT newIndex xs newBB where
        newIndex = i + advanceIndex x
        newBB  = bb .|. oneTileFigure pT i x
-----------------------------------------------

--------------------------------------------
-- parses a digit and returns the digit or 1
-- advanceIndex :: ['0'..'9'] -> Int
advanceIndex :: Char -> Int
advanceIndex c = if isDigit c then read [c] else 1
--------------------------------------------

----------------------------------------------------------
-- checks if on a specific square is a specific PieceType
-- and returns either the postion or zero
-- oneTileFigure :: Piece -> square -> AN -> PiecePosition
oneTileFigure :: PieceType -> Int -> Char -> Word64
oneTileFigure pT square f = case parsePieceType f of
                    Just pT2 -> if pT2 == pT then setBit 0 square else 0
                    Nothing -> 0
----------------------------------------------------------

----------------------------------------------------------
-- a table parsing Algebraic Noatation Char to PiercesType
parsePieceType :: Char -> Maybe PieceType
parsePieceType 'r' = Just RookB
parsePieceType 'n' = Just KnightB
parsePieceType 'b' = Just BishopB
parsePieceType 'q' = Just QueenB
parsePieceType 'k' = Just KingB
parsePieceType 'p' = Just PawnB
parsePieceType 'P' = Just PawnW
parsePieceType 'R' = Just RookW
parsePieceType 'N' = Just KnightW
parsePieceType 'B' = Just BishopW
parsePieceType 'Q' = Just QueenW
parsePieceType 'K' = Just KingW
parsePieceType _   = Nothing
----------------------------------------------------------
