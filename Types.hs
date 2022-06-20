module Types(PieceType(..), Color(..), Position(..), GameState(..),
            positionToPoint, pieceToUnicode, isPieceWhite, pointToPosition,
            setBitboards,getBitboard, replaceBitboardAt,cIntToInt8,
            int8ToCInt) where
import Data.Int (Int8)
import qualified SDL
import Foreign.C (CInt)
import Data.List (elemIndex)
import Data.Word (Word64)

data PieceType =    PawnW | KnightW | BishopW | RookW |
                    QueenW | KingW | PawnB | KnightB |
                    BishopB | RookB | QueenB | KingB
    deriving (Show,Eq,Enum,Bounded)

pieceToUnicode :: PieceType -> String
pieceToUnicode KingW   = "\x2654"
pieceToUnicode QueenW  = "\x2655"
pieceToUnicode RookW   = "\x2656"
pieceToUnicode BishopW = "\x2657"
pieceToUnicode KnightW = "\x2658"
pieceToUnicode PawnW   = "\x2659"
pieceToUnicode KingB   = "\x2654"
pieceToUnicode QueenB  = "\x2655"
pieceToUnicode RookB   = "\x2656"
pieceToUnicode BishopB = "\x2657"
pieceToUnicode KnightB = "\x2658"
pieceToUnicode PawnB   = "\x2659"

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
