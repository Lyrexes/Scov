module Moves (pawnMoves, bishopMoves, knightMoves,
              rookMoves, queenMoves, kingMoves,
              possibleMoves, movePiece, toSquares,
              friendlyOcc, enemyOcc, capturePiece,
              enemyPT) where

import Data.Bits
import Types (U64,Color(..),PieceType(..),GameState(..),
              setBitboard, getBitboard, isPieceWhite    )
import Bitboard (sqrOf, showBitboard, orBB)
import Attacks (bishopAttacks, knightAttacks, pawnAttacks,
                kingAttacks, rookAttacks, queenAttacks)

----------------------
{- rank seven constant
8   00000000
7   11111111
6   00000000
5   00000000
4   00000000
3   00000000
2   00000000
1   00000000

    abcdefgh
-}
rankSeven :: U64
rankSeven = 65280
---------------------

------------------
{- rank 2 constant
8   00000000
7   00000000
6   00000000
5   00000000
4   00000000
3   00000000
2   11111111
1   00000000

    abcdefgh
-}
rankTwo :: U64
rankTwo = 71776119061217280
------------------

---------------------------------------------------------------
-- all possible queen moves on given square
-- queenMoves :: Square -> FriendlyBlock -> EnemyBlock -> Moves
queenMoves :: Int -> U64 -> U64 -> U64
queenMoves = queenAttacks
---------------------------------------------------------------

---------------------------------------------------------------
-- all possible bishop moves on given square
-- bishopMoves :: Square -> FriendlyBlock -> EnemyBlock -> Moves
bishopMoves :: Int -> U64 -> U64 -> U64
bishopMoves = bishopAttacks
---------------------------------------------------------------

---------------------------------------------------------------
-- all possible rook moves on given square
-- rookAttacks :: Square -> FriendlyBlock -> EnemyBlock -> Moves
rookMoves :: Int -> U64 -> U64 -> U64
rookMoves = rookAttacks
---------------------------------------------------------------

-------------------------------------------------
-- all possible knight moves on given square
-- knight :: Square -> FriendlyBlock -> Moves
knightMoves :: Int -> U64 -> U64
knightMoves s bF = complement bF .&. knightAttacks s
-------------------------------------------------

-------------------------------------------------
-- all possible king moves on given square
-- kingMoves :: Square -> FriendlyBlock -> Moves
kingMoves :: Int -> U64 -> U64
kingMoves s bF = complement bF .&. kingAttacks s
-------------------------------------------------

-------------------------------------------------------------------
-- all possible pawn moves on given square
-- knight :: Side -> Square -> FriendlyBlock -> EnemyBlock -> Moves
pawnMoves :: Color -> Int -> U64 -> U64 -> U64
pawnMoves White s bF bE = (pawnAttacks White s .&. bE) .|. adv
    where
        advSqr = setBit 0 (s-8)
        adv = if advSqr .&. (bF .|. bE) == 0 then advSqr .|. doubleAdv else 0
        doubleAdv = if rankTwo .&. setBit 0 s > 0 then setBit 0 (s-16) else 0
pawnMoves Black s bF bE = (pawnAttacks Black s .&. bE) .|. adv
    where
        advSqr = setBit 0 (s+8)
        adv = if advSqr .&. (bF .|. bE) == 0 then advSqr .|. doubleAdv else 0
        doubleAdv = if rankSeven .&. setBit 0 s > 0 then setBit 0 (s+16) else 0
-------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- movePiece :: CurrentSquare -> NewSquare -> Piece -> CurrentGameState-> NewGameState
movePiece :: Int -> Int -> PieceType ->  GameState -> GameState
movePiece cs ns pT (GameState ps) = GameState (setBitboard ps pT (remSet (getBitboard ps pT))) where
    remSet = (`clearBit` cs) . (`setBit` ns)
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
-- capturePiece :: CurrSquare -> NewSquare -> Attacker -> Attacked -> CurrGameState -> NewGameState
capturePiece :: Int -> Int -> PieceType -> PieceType -> GameState -> GameState
capturePiece cs ns tA tD gs =  GameState (setBitboard afterMove tD (rem (getBitboard afterMove tD))) where
    (GameState afterMove) = movePiece cs ns tA gs
    rem = (`clearBit` ns)
--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------
-- all possible  moves on given square and given PieceType
-- getMoves :: PieceType -> Square -> FriendlyBlock -> EnemyBlock -> Moves
getMoves :: PieceType -> Int -> U64 -> U64 -> U64
getMoves PawnB s bF bE = pawnMoves Black s bF bE
getMoves PawnW s bF bE = pawnMoves White s bF bE
getMoves pT s bF bE | pT `elem` [KnightB,KnightW] = knightMoves s bF
                         | pT `elem` [BishopB,BishopW] = bishopMoves s bF bE
                         | pT `elem` [RookB, RookW]    = rookMoves s bF bE
                         | pT `elem` [QueenB,QueenW]   = queenMoves s bF bE
                         | otherwise = kingMoves s bF
--------------------------------------------------------------------------

getAttacks :: PieceType -> Int -> U64 -> U64 -> U64
getAttacks PawnB s bF bE = pawnAttacks Black s
getAttacks PawnW s bF bE = pawnAttacks White s
getAttacks pT s bF bE | pT `elem` [KnightB,KnightW] = knightAttacks s
                      | pT `elem` [BishopB,BishopW] = bishopAttacks s bF bE
                      | pT `elem` [RookB, RookW]    = rookAttacks s bF bE
                      | pT `elem` [QueenB,QueenW]   = queenAttacks s bF bE
                      | otherwise = kingAttacks s


getAllAttacks :: GameState -> PieceType -> U64 -> U64 -> U64
getAllAttacks (GameState ps) pT bF bE = orBB [ getAttacks pT s bF bE | s <- sqs ]
    where
    sqs = toSquares (ps!! fromEnum pT)

------------------------------------------
-- convert bitboard of position to squares
-- toSquares :: Bitboard -> Squares
toSquares :: U64 -> [Int]
toSquares x = [i | i <- [0..63], bit i .&. x >= 1]
----------------------------------------------------------------

------------------------------------------
-- convert bitboard of position to squares
-- toBitboard :: Squares -> Bitboard
toBitboard :: [Int] -> U64
toBitboard xs = orBB [bit x | x <- xs]
----------------------------------------------------------------

-------------------------------------------------------------------------------
-- all possible  moves on given square and given PieceType
-- possibleMoves :: PieceType -> Square -> FriendlyBlock -> EnemyBlock -> Moves
possibleMoves :: GameState -> PieceType -> Int -> U64 -> U64 -> U64
possibleMoves gs@(GameState ps) pT s bF bE = toBitboard (noCheckFilter mvs)
    where
    mvs = toSquares (getMoves pT s bF bE)
    noCheckFilter = filter (noCheck gs pT s bF bE)
    --todo check if it is check after move
-------------------------------------------------------------------------------

-----------------------------------------------------------------------
-- get attacked piece type
-- enemyPT :: Piece -> AttackedSquare -> CurrGameState -> AttackedPiece
enemyPT :: PieceType -> Int -> GameState -> PieceType
enemyPT pT s (GameState gs)
    | pT `elem` [PawnW .. KingW] = if null lsB then error "empty List!!!" else head lsB
    | otherwise = if null lsW then error "empty List!!!" else head lsW
    where
    lsB = filter (\x -> getBitboard gs x .&. bit s >= 1) [PawnB .. KingB]
    lsW = filter (\x -> getBitboard gs x .&. bit s >= 1) [PawnW .. KingW]
-----------------------------------------------------------------------

noCheck :: GameState -> PieceType -> Int -> U64 -> U64 -> Int -> Bool
noCheck gs pT cs bF bE ns = not (isCheck pT newbF newbE newGS)
    where
    newbF = enemyOcc pT newGS
    newbE = friendlyOcc pT newGS
    enPt  = enemyPT pT ns gs
    newGS = if bE .&. bit ns == 0
            then movePiece cs ns pT gs
            else capturePiece cs ns pT enPt gs


isCheck :: PieceType -> U64 -> U64 -> GameState -> Bool
isCheck pT bF bE gs@(GameState ps) | isPieceWhite pT = attacksB .&. kingW > 0
                                   | otherwise = attacksW .&. kingB > 0
    where
    kingW = ps !! fromEnum KingW
    kingB = ps !! fromEnum KingB
    attacksW = orBB [getAllAttacks gs x bF bE| x <- [PawnW .. KingW]]
    attacksB = orBB [getAllAttacks gs x bF bE| x <- [PawnB .. KingB]]


-------------------------------
--all friendly piece occupation
-- friendlyBlock :: Piece -> CurrGameState -> Bitboard
friendlyOcc :: PieceType -> GameState -> U64
friendlyOcc pT (GameState ps) | pT `elem` [PawnW .. KingW] = orBB (take 6 ps)
                                | otherwise = orBB (drop 6 ps)
--all enemy piece occupation
-- enemyBlock :: Piece -> CurrGameState -> Bitboard
enemyOcc :: PieceType -> GameState -> U64
enemyOcc pT (GameState ps) | pT `elem` [PawnW .. KingW]  = orBB (drop 6 ps)
                             | otherwise = orBB (take 6 ps)
-------------------------------
