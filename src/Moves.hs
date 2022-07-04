module Moves (pawnMoves, bishopMoves, knightMoves, rookMoves, queenMoves, kingMoves) where
import Data.Bits
import Types (U64,Color(..))
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
