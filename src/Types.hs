
{-# LANGUAGE DeriveGeneric #-}

module Types(PieceType(..), Color(..), Position(..), GameState(..), U64,
             Opening(..), positionToPoint, isPieceWhite, pointToPosition,
             setBitboards,getBitboard, replaceBitboardAt,cIntToInt8,
             int8ToCInt) where

import Data.Int (Int8)
import qualified SDL
import Foreign.C (CInt)
import Data.List (elemIndex)
import Data.Word (Word64)
import Data.Aeson as A
import Data.Text (Text)
import GHC.Generics

type U64 = Word64

data PieceType =    PawnW | KnightW | BishopW | RookW |
                    QueenW | KingW | PawnB | KnightB |
                    BishopB | RookB | QueenB | KingB
    deriving (Show,Eq,Enum,Bounded)

isPieceWhite :: PieceType -> Bool
isPieceWhite p = p `elem` [KingW ,QueenW,RookW,BishopW, KnightW, PawnW]

data Opening = Opening {
        name  :: Text,
        eco   :: Text,
        fen   :: Text,
        moves :: Text
    }
    deriving(Show,Generic)

instance A.ToJSON Opening where
    toEncoding = genericToEncoding defaultOptions
instance A.FromJSON Opening

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
type PossibleMoves = Word64

data GameState  = GameState Pieces PossibleMoves Color
    deriving (Show,Eq)

getBitboard :: [Word64] -> PieceType -> Word64
getBitboard bs pT = bs !! fromEnum pT

setBitboards :: GameState -> [Word64] -> GameState
setBitboards (GameState _  psMvs c) ps = GameState ps psMvs c

replaceBitboardAt :: [Word64] -> Int ->  Word64 -> [Word64]
replaceBitboardAt ps i p =  ls ++ [p] ++ tail rs
    where
        (ls,rs) = splitAt i ps
