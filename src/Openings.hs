--module Openings () where

import Data.Aeson as A
import Data.Text
import Data.ByteString.Lazy.UTF8 as BLU
import Types (Opening (Opening))

------------------------------------------------------------------------
-- reads and parses eco.json to a list [Opening {name, eco, fen, moves}]
readOpenings :: IO [Opening]
readOpenings = do
    json <- Prelude.readFile "eco.json"
    case A.decode (BLU.fromString json)::Maybe [Opening] of
        Just openings -> return openings
        Nothing -> error "could not read eco.json"
------------------------------------------------------------------------

main :: IO()
main = readOpenings >>= print
