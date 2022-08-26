module Types(PieceType(..), Color(..), GameState(..), U64,
             RGB, isPieceWhite, getBitboard, setBitboard) where

import Data.Int (Int8)
import qualified SDL
import Foreign.C (CInt)
import Data.List (elemIndex)
import Data.Word (Word64,Word8)

-- Bitboard type
type U64 = Word64

-- Color Type
type RGB = (Word8,Word8,Word8)

-- piece types
data PieceType = PawnW | KnightW | BishopW | RookW |
                 QueenW | KingW | PawnB | KnightB |
                 BishopB | RookB | QueenB | KingB
    deriving (Show,Eq,Enum,Bounded)

-- check if piece is white
-- isPieceWhite :: Piece -> Bool
isPieceWhite :: PieceType -> Bool
isPieceWhite p = p `elem` [KingW ,QueenW,RookW,BishopW, KnightW, PawnW]

-- Sides
data Color = Black | White
    deriving (Show,Eq,Enum)

-- bitboard representation
type Pieces = [Word64]

newtype GameState  = GameState Pieces
    deriving (Show,Eq)

getBitboard :: Pieces -> PieceType -> Word64
getBitboard bs pT = bs !! fromEnum pT

setBitboard :: [Word64] -> PieceType -> Word64 -> [Word64]
setBitboard ps pT p =  ls ++ [p] ++ tail rs
    where
        (ls,rs) = splitAt (fromEnum pT) ps
