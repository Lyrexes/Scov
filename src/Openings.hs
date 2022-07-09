{-# LANGUAGE DeriveGeneric #-}

module Openings (Move(..),DisAmb(..),testParseMoves) where
import Types (U64,PieceType(..),Color(..))
import Data.Aeson as A
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Char (isDigit,ord)
import GHC.Generics

------------------------------------------------
-- Disambiguate moves with specific Rank or File
data DisAmb = Rank Int | File Int | None
    deriving Show
------------------------------------------------

------------------------------
-- Move    Piece DisAmb Square
-- Capture Piece DisAmb Square
data Move = Mov PieceType DisAmb Int
          | Cap PieceType DisAmb Int
          | CastleKingW
          | CastleQueenW
          | CastleKingB
          | CastleQueenB
    deriving Show
------------------------------

-------------------------------------------
-- Tokens to parse Standard Chess Noatation
data Token = CastleK | CastleQ | Capture | Square Int |  R Int
                | F Int | Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show,Eq)
-------------------------------------------

-------------------------------------------------------
--Data stucture storing the opening info from json File
data OpeningMeta = OpeningMeta {
        name  :: String,
        eco   :: String,
        fen   :: String,
        moves :: String
    }
 deriving(Generic,Show)

instance A.ToJSON OpeningMeta where
    toEncoding = genericToEncoding defaultOptions
instance A.FromJSON OpeningMeta
-------------------------------------------------------

------------------------------------------------------------------------
-- reads and parses eco.json to a list [Opening {name, eco, fen, moves}]
readOpenings :: IO [OpeningMeta]
readOpenings = do
    json <- Prelude.readFile "eco.json"
    case A.decode (fromString json)::Maybe [OpeningMeta] of
        Just openings -> return openings
        Nothing -> error "could not read eco.json"
------------------------------------------------------------------------

--------------------------------------------------
-- splits moves and remvoes move number indication
splitMoves :: String -> [String]
splitMoves = filter isMoveNumber . words where
    isMoveNumber x = not (isDigit (head x) || isDigit (x!!1) && x!!1 == '.')
--------------------------------------------------

------------------------------------------------
-- Tokenize Standard Chess Notation move strings
tokenize :: String -> [[Token]]
tokenize x = [tokenizeMove m | m <- splitMoves x]

tokenizeMove :: String -> [Token]
tokenizeMove (x:xs) = if parsePiece x == Pawn
                  then case parseForms (x:xs) of
                    [CastleK] -> [CastleK]
                    [CastleQ] -> [CastleQ]
                    ts        -> Pawn : ts
                  else parsePiece x : parseForms xs
tokenizeMove _ = error "tokenize error: invalid Move String!"

parseForms :: String -> [Token]
parseForms [] = []
parseForms ('x':xs)  = Capture : parseForms xs
parseForms ('O':'-':'O':'-':'O':xs) = CastleQ : parseForms xs
parseForms ('O':'-':'O':xs) = CastleK : parseForms xs
parseForms [x] | x `elem` ['+','#'] = []
parseForms (x:y:xs) | isSquare = Square square : parseForms xs
                    | isRank  = R (8-read [x]::Int) : parseForms (y:xs)
                    | isFile = F (ord x-97) : parseForms (y:xs)
                    | otherwise = error ("invalid move form: " ++ (x:y:xs) ++ "!")
    where
        isFile = x `elem` ['a'..'h']
        isRank = isDigit x
        isSquare = elem x ['a'..'h'] && isDigit y
        square = (8-read [y]::Int) * 8 + (ord x - 97)
parseForms x = error ("invalid move form: " ++ x  ++ "!")

parsePiece :: Char -> Token
parsePiece 'K' = King
parsePiece 'Q' = Queen
parsePiece 'R' = Rook
parsePiece 'B' = Bishop
parsePiece 'N' = Knight
parsePiece _   = Pawn
------------------------------------------------

testParseMoves :: String -> [Move]
testParseMoves = tokensToMoves . tokenize

----------------
-- Parse opening
parseMoves :: OpeningMeta -> [Move]
parseMoves = tokensToMoves . tokenize . moves

tokensToMoves :: [[Token]] -> [Move]
tokensToMoves xs = pMH xs White [] where
    pMH :: [[Token]] -> Color -> [Move] -> [Move]
    pMH [] _ acc = acc
    pMH (x:xs) White acc = pMH xs Black (acc++[tokensToMove White x])
    pMH (x:xs) Black acc = pMH xs White (acc++[tokensToMove Black x])

tokensToMove :: Color -> [Token] -> Move
tokensToMove c [p, R x, Square s] = Mov (tokenToPiece c p) (Rank x) s
tokensToMove c [p, F x, Square s] = Mov (tokenToPiece c p) (File x) s
tokensToMove c [p, R x, Capture, Square s] = Cap (tokenToPiece c p) (File x) s
tokensToMove c [p, F x, Capture, Square s] = Cap (tokenToPiece c p) (File x) s
tokensToMove c [p, Square s] = Mov (tokenToPiece c p) None s
tokensToMove c [p, Capture, Square s] = Cap (tokenToPiece c p) None s
tokensToMove White [CastleK] = CastleKingW
tokensToMove White [CastleQ] = CastleQueenW
tokensToMove Black [CastleK] = CastleKingB
tokensToMove Black [CastleQ] = CastleQueenB
tokensToMove _ x = error ("invalid Tokens, got: " ++ show x)

tokenToPiece :: Color -> Token -> PieceType
tokenToPiece White Pawn   = PawnW
tokenToPiece Black Pawn   = PawnB
tokenToPiece White Knight = KnightW
tokenToPiece Black Knight = KnightB
tokenToPiece White Bishop = BishopW
tokenToPiece Black Bishop = BishopB
tokenToPiece White Rook   = RookW
tokenToPiece Black Rook   = RookB
tokenToPiece White Queen  = QueenW
tokenToPiece Black Queen  = QueenB
tokenToPiece White King   = KingW
tokenToPiece Black King   = KingB
tokenToPiece _ x = error ("tokenToPiece takes only Pieces, got: " ++ show x)
----------------

{-
main :: IO()
main = do
    o <- readOpenings
    --print (zip [0..length o](map splitMoves [x |x <- moveStrings o ]))
    let i = 3000
    print (map moves o !! i)
  print (zip [1..16](parseMoves (o !! i)))
-}
