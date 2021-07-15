module Lib
  ( someFunc,
  )
where

import Data.Either ()
import Data.Functor.Identity (Identity)
import Data.List (filter, foldl', intercalate)
import Data.Set (Set, empty, fromList, insert, toList)
import System.IO ()
import Text.Parsec (ParsecT)
import Text.ParserCombinators.Parsec (ParseError, anyChar, manyTill, parse, string, try)

parser :: ParsecT [Char] u Identity [Char]
parser = manyTill anyChar (try $ string "static/") *> manyTill anyChar (try $ string ".json")

accString :: Either ParseError [Char] -> [[Char]] -> [[Char]]
accString res acc = case res of
  Right s -> s : acc
  Left _ -> acc

someFunc :: IO ()
someFunc = do
  txt <- readFile "data/jsons.txt"
  let nameList = toList . fromList $ foldl' (\acc s -> accString (parse parser "" s) acc) [] (lines txt)
  (writeFile "data/results.txt" . intercalate "\n") nameList
