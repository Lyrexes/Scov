module Bitboard (showBitboard) where

import Data.Word
import Data.Bits
import Fen (parseFEN)
import Types (PieceType)

showBitboard :: Word64 -> String
showBitboard bb = concat [tileFrameToStr bb r c | r <-[0..7], c<- [0..8]]

tileFrameToStr :: Word64 -> Int -> Int -> String
tileFrameToStr bb row 0 = show (8-row)++ spaces 3 ++ tileToStr bb row 0
tileFrameToStr bb 7 8 = "\n\n" ++ spaces 4 ++ ['a'..'h'] ++ "\n"++ show bb
tileFrameToStr _ _ 8 = "\n"
tileFrameToStr bb row col = tileToStr bb row col

tileToStr :: Word64 -> Int -> Int -> String
tileToStr bb row col = if testBit bb (row*8+col) then "1" else "0"

spaces :: Int -> String
spaces i = [' '| _<-[1..i]]
