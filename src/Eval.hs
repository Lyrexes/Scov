module Eval (initGameState) where
import Types (GameState (GameState),Position (..),
              Color(..),PieceType(..), U64, getBitboard, setBitboard)
import Bitboard (sqrOf, orBB)
import Openings (Move(..),DisAmb(..),testParseMoves)
import Fen (parseFEN)
import Moves (possibleMoves)
import Debug.Trace (trace)
import Data.Int (Int8)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Data.Bits

initGameState :: Color -> [GameState]
initGameState color = test "1. e4 Nf6 2. e5 Nd5 3. Na3" (GameState (initPieces color))

test :: String -> GameState -> [GameState]
test x  = openingGameStates (testParseMoves x)

initPieces :: Color -> [U64]
initPieces White = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
initPieces Black = parseFEN "PPPPPPPP/RNBQKBNR/8/8/8/8/rnbqkbnr/pppppppp"

openingGameStates :: [Move] -> GameState -> [GameState]
openingGameStates [] _                  = []
openingGameStates (CastleQueenW:ms) cur = let gs = castleQueenSideW cur in gs : openingGameStates ms gs
openingGameStates (CastleQueenB:ms) cur = let gs = castleQueenSideB cur in gs : openingGameStates ms gs
openingGameStates (CastleKingW:ms)  cur = let gs = castleKingSideW  cur in gs : openingGameStates ms gs
openingGameStates (CastleKingB:ms)  cur = let gs = castleKingSideW  cur in gs : openingGameStates ms gs
openingGameStates (Mov pT dA s:ms)  cur = let gs = move pT dA s cur in gs : openingGameStates ms gs
openingGameStates (Cap pT dA s:ms)  cur = error "unimplemented"

move :: PieceType -> DisAmb -> Int -> GameState -> GameState
move pT None s g = movePiece (currSquare pT s g) s pT g
move pT (Rank r) s g = error "unimplemented!"
move pT (File f) s g = error "unimplemented!"

currSquare :: PieceType -> Int -> GameState -> Int
currSquare pT s g = if length square == 1 then (fst . head) square else error ("ambigouse Square: " ++ show (length square)) where
    square =  filter (\(_,x) -> bit s .&. x >= 1) (allPossMoves pT g)

friendlyBlock :: PieceType -> GameState -> U64
friendlyBlock pT (GameState ps) | pT `elem` [PawnW .. KingW] = orBB (take 6 ps)
                                | otherwise = orBB (drop 6 ps)

enemyBlock :: PieceType -> GameState -> U64
enemyBlock pT (GameState ps) | pT `elem` [PawnW .. KingW]  = orBB (drop 6 ps)
                             | otherwise = orBB (take 6 ps)

allPossMoves :: PieceType -> GameState  ->  [(Int, U64)]
allPossMoves pT g@(GameState ps) = zip squares [ possibleMoves pT s fB eB | s <- squares] where
    squares = pieceSquares (getBitboard ps pT)
    eB = enemyBlock pT g
    fB = friendlyBlock pT g

castleQueenSideW :: GameState ->  GameState
castleQueenSideW = moveKing . moveRook
    where
        moveKing x = movePiece (sqrOf 7 4) (sqrOf 7 2) KingW x
        moveRook x = movePiece (sqrOf 7 0) (sqrOf 7 3) RookW x

castleKingSideW :: GameState ->  GameState
castleKingSideW = moveKing . moveRook
    where
        moveKing x = movePiece (sqrOf 7 4) (sqrOf 7 6) KingW x
        moveRook x = movePiece (sqrOf 7 7) (sqrOf 7 5) RookW x

castleQueenSideB :: GameState ->  GameState
castleQueenSideB = moveKing . moveRook
    where
        moveKing x = movePiece (sqrOf 0 4) (sqrOf 0 2) KingB x
        moveRook x = movePiece (sqrOf 0 0) (sqrOf 0 3) RookB x

castleKingSideB :: GameState ->  GameState
castleKingSideB = moveKing . moveRook
    where
        moveKing x = movePiece (sqrOf 0 4) (sqrOf 0 6) KingB x
        moveRook x = movePiece (sqrOf 0 7) (sqrOf 0 5) RookB x

movePiece :: Int -> Int -> PieceType ->  GameState -> GameState
movePiece cs ns pT (GameState ps) = GameState (setBitboard ps pT (remSet (getBitboard ps pT))) where
    remSet = (`clearBit` cs) . (`setBit` ns)

pieceSquares :: U64 -> [Int]
pieceSquares x = [i | i <- [0..63], bit i .&. x >= 1]
