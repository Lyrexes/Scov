module Attacks (pawnAttack, kingAttack, knightAttack, rookAttack, bishopAttack) where

import Bitboard(showBitboard, orBB, sqrOf, rowOf, colOf)
import Types (Color(..), U64)
import Debug.Trace
import Data.Bits
import Data.Word


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

---------------------------------------------------------------------
-- calculate Pawn Attack : PawnAttack :: Side -> Square -> PawnAttack

pawnAttack :: Color -> Int -> U64
pawnAttack White s = orBB [if bb .&. notAFile > 0 then shiftR bb 9 else 0,
                             if bb .&. notHFile > 0 then shiftR bb 7 else 0]
    where bb = setBit 0 s
pawnAttack Black s = orBB [if bb .&. notHFile > 0 then shiftL bb 9 else 0,
                             if bb .&. notAFile > 0 then shiftL bb 7 else 0]
    where bb = setBit 0 s
---------------------------------------------------------------------

----------------------------------------------------------------
-- A table of all possible Knight Attacks: knightAttacks[square]
knightAttack ::Int -> U64
knightAttack s = orBB [if bb .&. notAFile > 0 then shiftR bb 17 else 0,
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

------------------------------------------------------------
-- calulate King Attacks: kingAtack ::  Square -> KingAttack
kingAttack :: Int -> U64
kingAttack s = orBB [shiftR bb 8,
                       if bb .&. notAFile > 0 then shiftR bb 9 else 0,
                       if bb .&. notAFile > 0 then shiftR bb 1 else 0,
                       if bb .&. notHFile > 0 then shiftR bb 7 else 0,
                       shiftL bb 8,
                       if bb .&. notHFile > 0 then shiftL bb 9 else 0,
                       if bb .&. notHFile > 0 then shiftL bb 1 else 0,
                       if bb .&. notAFile > 0 then shiftL bb 7 else 0
                    ]
    where bb = setBit 0 s
------------------------------------------------------------


------------------------------------------------------------------------------------------
--calculate rook attack: RookAttack :: Square -> FriendlyBlock -> EnemyBlock -> RookAttack
rookAttack :: Int -> U64 -> U64 -> U64
rookAttack s bF bE = orBB [
                      orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF uR)),
                      orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF dR)),
                      orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF rR)),
                      orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF lR))
                ]
    where
        isNotBlockedF x = x .&. bF == 0
        isNotBlockedE x = x .&. bE == 0
        uR = reverse (map (setBit 0 . (`sqrOf` colOf s)) [rowOf s..7])
        dR = reverse (map (setBit 0 . (`sqrOf` colOf s)) [0..rowOf s])
        rR = reverse (map (setBit 0 . sqrOf (rowOf s))  [colOf s..7])
        lR = reverse (map (setBit 0 . sqrOf (rowOf s)) [0..colOf s])
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------
--calculate bishop attacks with block: bishopAttack :: Square -> FriendlyBlock -> EnemyBlock -> BishopAttack
bishopAttack :: Int -> U64 -> U64 -> U64
bishopAttack s bF bE = orBB [
                        orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF ruB)),
                        orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF rdB)),
                        orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF luB)),
                        orBB ((takeWhileP1 isNotBlockedE . reverse) (takeWhile isNotBlockedF ldB))
                        ]
    where
        isNotBlockedF x = x .&. bF == 0
        isNotBlockedE x = x .&. bE == 0
        ruB  = zipWith (\r c -> setBit 0 (sqrOf r c)) [rowOf s..7] [colOf s..7]
        rdB  = zipWith (\r c -> setBit 0 (sqrOf r c)) [0..rowOf s] [0..colOf s]
        luB  = zipWith (\r c -> setBit 0 (sqrOf r c)) (reverse [0..rowOf s]) [colOf s..7]
        ldB  = zipWith (\r c -> setBit 0 (sqrOf r c)) [rowOf s..7] (reverse [0..colOf s])

-------------------------------------------------------------------------------------------------------------

--------------------------------------------------------
--takeWhile plus the element where the predicate is true
takeWhileP1 :: (a->Bool) -> [a] -> [a]
takeWhileP1 f = foldr (\x acc -> if f x then x:acc else [x]) []
--------------------------------------------------------
