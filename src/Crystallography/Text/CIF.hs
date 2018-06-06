module Crystallography.Text.CIF (
  cif,
  -- Contents(..),
  Body(..),
  DataItem(..),
  Tag(..),
  Value(..),
  Numeric(..),
  Number(..),
  getStrings,

  query,
  loopToAssocList,
  toDouble
  ) where

import Control.Monad
import Data.List.Split
import Numeric
import Crystallography.Text.CIF.Parser
import Crystallography.Text.CIF.Contents

loopToAssocList :: DataItem -> [[(Tag,Value)]]
loopToAssocList (Loop tags vals) = map (zip tags) . chunksOf (length tags) $ vals
loopToAssocList _ = []


hasTag :: Tag -> DataItem -> Bool
hasTag t0 (Item t1 _) = t0 == t1
hasTag t0 (Loop tags _) = t0 `elem` tags


query :: Tag -> [DataItem] -> [DataItem]
query t0 = filter (hasTag t0)


floatString :: Value -> String
floatString = getFloatString . getNumber . getNumeric


toDouble :: Value -> Double
toDouble = read' (readSigned' readFloat') . floatString


read' :: ReadS t -> String -> t
read' r d = case [ x | (x,"") <- r d ] of
    [] -> error "read' : no parse"
    [x] -> x
    _ -> error "read' : ambigious"


readFloat' :: RealFrac a => ReadS a
readFloat' d = readFloat d ++ f d
  where
    f d = do
      guard ((not .null) d)
      guard (head d == '.')
      readFloat $ "0" ++ d


readSigned' :: (Real a) => ReadS a -> ReadS a
readSigned' readPos = readParen False read'
                     where read' r  = read'' r ++
                                      (do
                                        ("-",s) <- lex' r
                                        (x,t)   <- read'' s
                                        return (-x,t))
                           read'' r = do
                               (str,s) <- lex' r
                               (n,"")  <- readPos str
                               return (n,s)


lex' :: ReadS String
lex' s = do
  (str,s) <- lex s
  case str of
    "-." -> return ("-","." ++ s)
    "." -> do
      (d,ss) <- lexDigits s
      return ("."++d,ss)
    _ -> return (str,s)
