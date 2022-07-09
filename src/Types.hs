module Types(PieceType(..), Color(..), Position(..), GameState(..), U64,
             CEvent(..), RGB,
             positionToPoint, isPieceWhite, pointToPosition,
             getBitboard, setBitboard,cIntToInt8, int8ToCInt) where

import Data.Int (Int8)
import qualified SDL
import Foreign.C (CInt)
import Data.List (elemIndex)
import Data.Word (Word64,Word8)

type U64 = Word64
type RGB = (Word8,Word8,Word8)

data CEvent = CEvent {
  quit :: Bool,
  mouseCords :: Maybe Position
}

data PieceType = PawnW | KnightW | BishopW | RookW |
                 QueenW | KingW | PawnB | KnightB |
                 BishopB | RookB | QueenB | KingB
    deriving (Show,Eq,Enum,Bounded)

isPieceWhite :: PieceType -> Bool
isPieceWhite p = p `elem` [KingW ,QueenW,RookW,BishopW, KnightW, PawnW]

data Color = Black | White
    deriving (Show,Eq,Enum)

data Position = Position Int8 Int8
    deriving(Show,Eq,Ord)

positionToPoint :: Position -> SDL.Point SDL.V2 CInt
positionToPoint (Position x y) = SDL.P(SDL.V2 newX newY)
    where
        newX = int8ToCInt x
        newY = int8ToCInt y

pointToPosition :: SDL.Point SDL.V2 CInt -> Position
pointToPosition (SDL.P (SDL.V2 x y)) = Position newX newY
    where
        newX = cIntToInt8 x
        newY = cIntToInt8 y

cIntToInt8 :: CInt-> Int8
cIntToInt8 c =  toEnum(fromEnum c)

int8ToCInt :: Int8 -> CInt
int8ToCInt c =  toEnum(fromEnum c)

type Pieces = [Word64]

newtype GameState  = GameState Pieces
    deriving (Show,Eq)

getBitboard :: Pieces -> PieceType -> Word64
getBitboard bs pT = bs !! fromEnum pT

setBitboard :: [Word64] -> PieceType -> Word64 -> [Word64]
setBitboard ps pT p =  ls ++ [p] ++ tail rs
    where
        (ls,rs) = splitAt (fromEnum pT) ps
