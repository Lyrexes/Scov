module Args (parseArgs,getOpening,Arg(..)) where

import Data.List (intersect,intercalate)
import Data.Char (isDigit)
import Types (GameState(GameState))
import Eval (openingGameStates, initPieces)
import System.Environment (getArgs)
import Openings (OpeningMeta (..),parseMoves, Move(..))

data Arg = Msg String
           | OWS (String,[GameState]) Int
           | O (String,[GameState])

helpStr :: String
helpStr = "scov [Option] [Opening] \n \
          \--search, -s [Opening name] \n \
          \--search, -s [Number of seach results] [Opening] \n \
          \--view, -v [Opening name] \n \
          \--view, -v [Opening ID] \n \
          \--view-with-size, -vws [Opening name] [window size] \n \
          \--view-with-size, -vws [Opening ID] [window size] \n \
          \--help, -h \n \
          \press LeftArrow for previous move \n \
          \press RightArrow for next move \n \
          \press H for previous move \n \
          \press L for next move \n"

errorStr :: String
errorStr = " not found or invalid amount of arguments, type --help or -h for help!\n"

-------------------------------------------------------------------------
-- ParseArgs :: Args -> [Openings] -> ArgumentResponse
parseArgs :: [String] -> [(Int,OpeningMeta)] -> Arg
parseArgs (x:n:s:r) o | x `elem` ["--search", "-s"] && isNum n = search s o $read n
parseArgs (x:s:_) o   | x `elem` ["--search", "-s"] = search s o 10
parseArgs (x:s:_) o   | x `elem` ["--view", "-v"] && isNum s = viewID (read s) o
parseArgs (x:s:_) o   | x `elem` ["--view", "-v"] = view s o
parseArgs (x:i:s:_) o | isArg = viewIDS (read i) s o
    where isArg = x `elem` ["--view-with-size", "-vws"] && isNum s && isNum i
parseArgs (x:n:s:_) o | x `elem` ["--view-with-size", "-vws"] && isNum s = viewS n s o
parseArgs (x:_) _     | x `elem` ["--help", "-h"] = Msg helpStr
parseArgs (x:_) _ = Msg ("command: " ++ x ++ errorStr)
parseArgs _ _ = Msg "no commands were given, type --help or -h to see all commands!\n"

isNum :: String -> Bool
isNum = all isDigit
-------------------------------------------------------------------------

---------------------------------------------------------------------------
-- view :: OpeningName -> [Openings] -> Opening
view :: String -> [(Int,OpeningMeta)] -> Arg
view nam os = if null searched
              then Msg ("No matches for Opening: " ++ nam)
              else O ( (name . snd . head) searched,
                           (getOpening . snd . head) searched)
    where
        searched = filter (\(_,x) -> intersect nam (name x) == nam) os

-- viewID :: ID -> [Openings] -> OpeningArg
viewID :: Int -> [(Int,OpeningMeta)] -> Arg
viewID id xs = case lookup id xs of
            Just x -> O (name x,getOpening x)
            Nothing -> Msg ("No opening with ID: " ++ show id ++ " \n")

-- viewS :: OpeningName -> [Openings] -> Opening
viewS :: String -> String -> [(Int,OpeningMeta)] -> Arg
viewS nam s os = if null searched
                 then Msg ("No matches for Opening: " ++ nam)
                 else OWS (oName, opening) (read s)
    where
        searched = filter (\(_,x) -> intersect nam (name x) == nam) os
        oName = (name . snd . head) searched
        opening = (getOpening . snd . head) searched

-- viewIDS :: ID -> [Openings] -> OpeningArg
viewIDS :: Int -> String -> [(Int,OpeningMeta)] -> Arg
viewIDS id s xs = case lookup id xs of
            Just x -> OWS (name x,getOpening x) (read s)
            Nothing -> Msg ("No opening with ID: " ++ show id ++ " \n")

-- getOpening :: OpeningSAN -> Opening
getOpening :: OpeningMeta -> [GameState]
getOpening x = initPieces : ((`openingGameStates`  initPieces) . parseMoves) x
---------------------------------------------------------------------------

-------------------------------------------------------------
-- search :: UserOpeningName -> [Openings] -> NumberOfResults
--           -> SearchMessage
search :: String -> [(Int,OpeningMeta)] -> Int -> Arg
search n os num = Msg $getResultStr searched
    where
        searched = take num (filter (\(_,x) -> intersect n (name x) == n) os)

-- getResultStr :: [ResultOpenings] -> ResultOpeningString
getResultStr :: [(Int,OpeningMeta)] -> String
getResultStr xs = intercalate ['\n']  (map (uncurry str) xs) ++ ['\n']
    where
        str i o = "ID: " ++ show i ++ " Name: " ++ name o
-------------------------------------------------------------
