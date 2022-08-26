module Eval (openingGameStates, initPieces) where
import Types (GameState (GameState), Color(..),
        PieceType(..), U64, getBitboard, setBitboard)
import Bitboard (sqrOf, orBB, showBitboard, colOf)
import Openings (Move(..),DisAmb(..))
import Fen (parseFEN)
import Moves (possibleMoves, movePiece, toSquares,
              friendlyOcc, enemyOcc, capturePiece,
              enemyPT)
import Data.Word (Word64)
import Data.Bits

---------------------------------
-- starting position of each side
initPosW :: String
initPosW = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

initPieces :: GameState
initPieces = GameState (parseFEN initPosW)
---------------------------------

----------------------------------------------------------------
-- converting Moves to corresponding GameStates
-- openingGameStates :: Moves -> StartingGameState -> GameStates
openingGameStates :: [Move] -> GameState -> [GameState]
openingGameStates [] _                  = []
openingGameStates (CastleQueenW:ms) cur = let gs = castleQueenSideW cur in gs : openingGameStates ms gs
openingGameStates (CastleQueenB:ms) cur = let gs = castleQueenSideB cur in gs : openingGameStates ms gs
openingGameStates (CastleKingW:ms)  cur = let gs = castleKingSideW  cur in gs : openingGameStates ms gs
openingGameStates (CastleKingB:ms)  cur = let gs = castleKingSideB  cur in gs : openingGameStates ms gs
openingGameStates (Mov pT dA s:ms)  cur = let gs = move pT dA s cur in gs : openingGameStates ms gs
openingGameStates (Cap pT dA s:ms)  cur
    | isEnPassant cur pT s = let gs = enPassant pT dA s cur in gs : openingGameStates ms gs
    | otherwise            = let gs = capture pT dA s cur in gs : openingGameStates ms gs
----------------------------------------------------------------

--------------------------------------------------------------------
-- execute en passent
-- enPassant :: PieceType -> File -> square -> GameState -> GameState
enPassant :: PieceType -> DisAmb -> Int -> GameState -> GameState
enPassant pT (File f) s gs@(GameState ps) = GameState afterEnPa
    where
    afterEnPa = setBitboard afterMove (enemyPawn pT) capPawn
    capPawn = clearBit (getBitboard ps (enemyPawn pT)) defenderSqr
    (GameState afterMove) = movePiece attackerSqr s pT gs
    attackerSqr = head (attFilter sqrsFriendly)
    attFilter = filter (\x -> bit x .&. file f >= 1)
    sqrsFriendly = toSquares (getBitboard ps pT)
    defenderSqr = head (defFilter sqrsEnemy)
    defFilter = filter (\x -> bit x .&. file (colOf s) >= 1)
    sqrsEnemy = toSquares (getBitboard ps (enemyPawn pT))
enPassant _ _ _ _ = error "enPassant: expected file DisAmbiguation"

-- enemyPawn :: PawnType -> PawnType
enemyPawn :: PieceType -> PieceType
enemyPawn PawnW = PawnB
enemyPawn PawnB = PawnW
enemyPawn _ = error "enemyPawn: expected pawn"

-- isEnPassant :: GameState -> Piece -> AtackedSquare -> Bool
isEnPassant :: GameState -> PieceType -> Int -> Bool
isEnPassant gs pT s | isPawn = bit s .&. enemyOcc pT gs == 0
                    | otherwise = False
    where
    isPawn = pT `elem` [PawnW, PawnB]
-------------------------------------------------------------

--------------------------------------------------------------------------------------
-- move piece
-- move :: Piece -> DisAmbiguation -> NewSquare -> CurrentGameState -> NewGameState
move :: PieceType -> DisAmb -> Int -> GameState -> GameState
move pT None s g = movePiece (currSquare noneAmb pT s g) s pT g
move pT (Rank r) s g = movePiece (currSquare (rankAmb r) pT s g) s pT g
move pT (File f) s g = movePiece (currSquare (fileAmb f) pT s g) s pT g
--------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------
-- capture piece
-- capture :: Piece -> DisAmbiguation -> NewSquare -> CurrGameState -> NewGameState
capture :: PieceType -> DisAmb -> Int -> GameState -> GameState
capture pT None s g = capturePiece (currSquare noneAmb pT s g)  s  pT (enemyPT pT s g) g
capture pT (Rank r) s g = capturePiece (currSquare (rankAmb r) pT s g) s pT (enemyPT pT s g) g
capture pT (File f) s g = capturePiece (currSquare (fileAmb f) pT s g) s pT (enemyPT pT s g) g
--------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------
-- get start square from new square and piece type
-- currSquare :: MoveFilter -> Piece -> NewSquare -> CurrGameState -> startSquare
currSquare :: ((Int,U64)->Bool) -> PieceType -> Int -> GameState -> Int
currSquare f pT s g = if length square == 1 then (fst . head) square  else err where
    square =  filter f (filter (\(_,x) -> bit s .&. x >= 1) (allPossMoves pT g))
    err = error ("ambigouse Square: " ++ show (length square))

-- no ambiguity filter
noneAmb :: (Int,U64) -> Bool
noneAmb = const True

-- filters moves where the starting square is on a specific file
fileAmb :: Int ->  (Int,U64) -> Bool
fileAmb f (s,_) = bit s .&. file f >= 1

-- filters moves where the starting square is on a specific rank
rankAmb :: Int -> (Int,U64)->Bool
rankAmb r (s,_) = bit s .&. rank r >= 1
---------------------------------------------------------------------------------

-----------------------------------
-- rank and file bit board constant
{-
rank 4:
8   00000000
7   00000000
6   00000000
5   00000000
4   11111111
3   00000000
2   00000000
1   00000000

    abcdefgh

file 4:
8   00001000
7   00001000
6   00001000
5   00001000
4   00001000
3   00001000
2   00001000
1   00001000

    abcdefgh
-}
-- rank :: Rank -> Bitboard
rank :: Int -> U64
rank r = orBB [bit (sqrOf r c)| c <- [0..7]]

-- file :: File -> Bitboard
file :: Int -> U64
file c = orBB [bit (sqrOf r c)| r <- [0..7]]
-----------------------------------

----------------------------------------------------------------
-- all valid moves of a specific type
-- allPossMoves :: Piece -> CurrGameState -> [(Square,Bitboard)]
allPossMoves :: PieceType -> GameState  ->  [(Int, U64)]
allPossMoves pT g@(GameState ps) = zip squares [ possibleMoves g pT s fB eB | s <- squares] where
    squares = toSquares (getBitboard ps pT)
    eB = enemyOcc pT g
    fB = friendlyOcc pT g
----------------------------------------------------------------

-------------------------------------------
-- castle king and queen side for each side
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
-------------------------------------------
