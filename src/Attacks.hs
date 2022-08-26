module Attacks (pawnAttacks, kingAttacks, knightAttacks, rookAttacks, bishopAttacks, queenAttacks) where

import Bitboard(showBitboard, orBB, sqrOf, rowOf, colOf)
import Types (Color(..), U64, PieceType(..))
import Debug.Trace
import Data.Bits
import Data.Word
import Fen (parseFEN)


{-------------------------------
    Not A File
8   01111111
7   01111111
6   01111111
5   01111111
4   01111111
3   01111111
2   01111111
1   01111111

    abcdefgh
-}
notAFile :: U64
notAFile = 18374403900871474942
--------------------------------

{-------------------------------
    Not AB File
8   00111111
7   00111111
6   00111111
5   00111111
4   00111111
3   00111111
2   00111111
1   00111111

    abcdefgh
-}
notABFile :: U64
notABFile = 18229723555195321596
--------------------------------

{------------------------------
    Not H File
8   11111110
7   11111110
6   11111110
5   11111110
4   11111110
3   11111110
2   11111110
1   11111110

    abcdefgh
-}
notHFile :: U64
notHFile = 9187201950435737471
------------------------------

{-----------------------------

Not GH File
8   11111100
7   11111100
6   11111100
5   11111100
4   11111100
3   11111100
2   11111100
1   11111100

    abcdefgh
-}
notGHFile :: U64
notGHFile = 4557430888798830399
-------------------------------

-------------------------------------------------------------------
-- calculate Pawn Attack : pawnAttacks :: Side -> Square -> Bitboard
pawnAttacks :: Color -> Int -> U64
pawnAttacks White s = orBB [if bb .&. notAFile > 0 then shiftR bb 9 else 0,
                             if bb .&. notHFile > 0 then shiftR bb 7 else 0]
    where bb = setBit 0 s
pawnAttacks Black s = orBB [if bb .&. notHFile > 0 then shiftL bb 9 else 0,
                             if bb .&. notAFile > 0 then shiftL bb 7 else 0]
    where bb = setBit 0 s
-------------------------------------------------------------------

----------------------------------------------------------------
-- calculate Knight Attacks: knightAttacks :: square -> Bitboard
knightAttacks ::Int -> U64
knightAttacks s = orBB [if bb .&. notAFile > 0 then shiftR bb 17 else 0,
                         if bb .&. notHFile > 0 then shiftR bb 15 else 0,
                         if bb .&. notABFile > 0 then shiftR bb 10 else 0,
                         if bb .&. notGHFile > 0 then shiftR bb 6 else 0,
                         if bb .&. notHFile > 0 then shiftL bb 17 else 0,
                         if bb .&. notAFile > 0 then shiftL bb 15 else 0,
                         if bb .&. notGHFile > 0 then shiftL bb 10 else 0,
                         if bb .&. notABFile > 0 then shiftL bb 6 else 0
                    ]
    where bb = setBit 0 s
----------------------------------------------------------------

----------------------------------------------------------
-- calulate King Attacks: kingAttacks ::  Square -> Bitboard
kingAttacks :: Int -> U64
kingAttacks s = orBB [shiftR bb 8,
                       if bb .&. notAFile > 0 then shiftR bb 9 else 0,
                       if bb .&. notAFile > 0 then shiftR bb 1 else 0,
                       if bb .&. notHFile > 0 then shiftR bb 7 else 0,
                       shiftL bb 8,
                       if bb .&. notHFile > 0 then shiftL bb 9 else 0,
                       if bb .&. notHFile > 0 then shiftL bb 1 else 0,
                       if bb .&. notAFile > 0 then shiftL bb 7 else 0
                    ]
    where bb = setBit 0 s
----------------------------------------------------------


----------------------------------------------------------------------------------------
--calculate rook attack: rookAttacks :: Square -> FriendlyBlock -> EnemyBlock -> Bitboard
rookAttacks :: Int -> U64 -> U64 -> U64
rookAttacks s bF bE = orBB [
                      orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF uR)),
                      orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF dR)),
                      orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF rR)),
                      orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF lR))
                ]
    where
        isNotBlockedF x = x .&. bF == 0
        isNotBlockedE x = x .&. bE == 0
        dR = map (bit . (`sqrOf` colOf s)) [rowOf s+1..7]
        uR = map (bit . (`sqrOf` colOf s)) (reverse [0..rowOf s-1])
        rR = map (bit . sqrOf (rowOf s))  [colOf s+1..7]
        lR = map (bit . sqrOf (rowOf s)) (reverse [0..colOf s-1])
----------------------------------------------------------------------------------------

---------------------------------------------------------------------
-- calculate bishop attacks with block:
-- bishopAttacks :: Square -> FriendlyBlock -> EnemyBlock -> BitBoard
bishopAttacks :: Int -> U64 -> U64 -> U64
bishopAttacks s bF bE = orBB [
        orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF ruB)),
        orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF rdB)),
        orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF luB)),
        orBB (takeWhileP1 isNotBlockedE (takeWhile isNotBlockedF ldB))
        ]
    where
    isNotBlockedF x = x .&. bF == 0
    isNotBlockedE x = x .&. bE == 0
    ruB  = zipWith (\r c -> bit (sqrOf r c)) (reverse [0..rowOf s -1]) [colOf s+1..7]
    rdB  = zipWith (\r c -> bit (sqrOf r c)) [rowOf s+1..7] [colOf s+1..7]
    luB  = zipWith (\r c -> bit (sqrOf r c)) (reverse [0..rowOf s-1]) (reverse [0..colOf s-1])
    ldB  = zipWith (\r c -> bit (sqrOf r c)) [rowOf s+1..7] (reverse [0..colOf s-1])
---------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------
--calulate queen attacks with block: queenAttacks :: Square -> FrriendlBlock -> EnemyBlock -> Bitboard
queenAttacks :: Int -> U64 -> U64 -> U64
queenAttacks s bF bE = bishopAttacks s bF bE .|. rookAttacks s bF bE
-----------------------------------------------------------------------------------------------------

--------------------------------------------------------
--takeWhile plus the element where the predicate is true
takeWhileP1 :: (a->Bool) -> [a] -> [a]
takeWhileP1 f = foldr (\x acc -> if f x then x:acc else [x]) []
--------------------------------------------------------
