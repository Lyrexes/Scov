import Bitboard(showBitboard)
import Types (Color(..))
import Data.Bits
import Data.Word

{- Not A File
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
notAFile :: Word64
notAFile = 18374403900871474942

{- Not AB File
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
notABFile :: Word64
notABFile = 18229723555195321596

{- Not H File
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
notHFile :: Word64
notHFile = 9187201950435737471

{- Not GH File
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
notGHFile :: Word64
notGHFile = 4557430888798830399

-- A table of all possible Pawn Attacks: PawnAttacks[side][square]
pawnAttacks :: [[Word64]]
pawnAttacks = [[pawnAttack White r c | r<-[0..7], c<-[0..7]],
               [pawnAttack Black r c | r<-[0..7], c<-[0..7]]]

-- A table of all possible Knight Attacks: knightAttacks[square]
knightAttacks :: [Word64]
knightAttacks = [knightAttack r c | r<-[0..7], c<-[0..7]]

knightAttack ::Int -> Int -> Word64
knightAttack r c = bb
    where bb = setBit 0 (r*8+c)

orBB :: [Word64] -> Word64
orBB = foldl (.|.) 0

pawnAttack :: Color -> Int -> Int -> Word64
pawnAttack White r c = orBB [if bb .&. notAFile > 0 then shiftR bb 9 else 0,
                             if bb .&. notHFile > 0 then shiftR bb 7 else 0]
    where bb = setBit 0 (r*8+c)
pawnAttack Black r c = orBB [if bb .&. notHFile > 0 then shiftL bb 9 else 0,
                             if bb .&. notAFile > 0 then shiftL bb 7 else 0]
    where bb = setBit 0 (r*8+c)

main :: IO()
main = putStrLn (showBitboard (knightAttack 1 2))
