module Fen (parseFEN) where
import Data.Binary (Word64)
import Data.Bits
import Data.Char(isDigit)
import Debug.Trace (trace)
import Types(PieceType(..), GameState(..))

parseFEN :: String -> [Word64]
parseFEN str = [figure x fen | x <- [minBound..maxBound]] where
    fen = withOutDelim str

withOutDelim :: String -> String
withOutDelim = filter ('/'/=)

figure :: PieceType -> [Char] -> Word64
figure pT fen = figH pT 0 fen 0 where
    figH _ _ [] bb = bb
    figH pT i (x:xs) bb = figH pT newIndex xs newBB where
        newIndex = i + advanceIndex x
        newBB  = bb .|. oneTileFigure pT i x

advanceIndex :: Char -> Int
advanceIndex c = if isDigit c then read [c]
                            else 1

oneTileFigure :: PieceType -> Int -> Char -> Word64
oneTileFigure pT index f = case pieceType f of
                    Just pT2 -> if pT2 == pT then setBit 0 index else 0
                    Nothing -> 0

pieceType :: Char -> Maybe PieceType
pieceType 'r' = Just RookB
pieceType 'n' = Just KnightB
pieceType 'b' = Just BishopB
pieceType 'q' = Just QueenB
pieceType 'k' = Just KingB
pieceType 'p' = Just PawnB
pieceType 'P' = Just PawnW
pieceType 'R' = Just RookW
pieceType 'N' = Just KnightW
pieceType 'B' = Just BishopW
pieceType 'Q' = Just QueenW
pieceType 'K' = Just KingW
pieceType _   = Nothing
