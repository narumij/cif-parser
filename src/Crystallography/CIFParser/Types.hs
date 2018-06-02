module Crystallography.CIFParser.Types (
  CIF(..),
  CIFBody(..),
  DataItem(..),
  Tag(..),
  Value(..),
  Numeric(..),
  Number(..),
  loopToAssocList,
  fromBody,
  -- getBlocks,
  toDouble,
  query,
  getStrings,
) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split
import Numeric

newtype CIF = CIF { getBodies :: [CIFBody] } deriving Show

data CIFBody
  = DataBlock { getHeading :: String, getBlock :: [DataItem] }
  | SaveFrame { getHeading :: String, getSaves :: [()] }
  deriving Show

data DataItem
  = Item { getTag :: Tag, getValue :: Value }
  | Loop { getTags :: [Tag], getValues :: [Value] }
  deriving Show

newtype Tag = Tag String deriving (Eq,Ord)

data Value
  = Inapplicable
  | Unknown
  | Numeric { getNumeric :: Numeric }
  | String { getString :: String }
  | Field { getField :: [String] }
  deriving Eq

data Numeric
  = Numeric_ { getNumber :: Number }
  | NumericSU { getNumber :: Number, uncert :: String }
  deriving Eq

getUncert (Numeric_ _) = Nothing
getUncert (NumericSU _ su) = Just su

data Number
  = Integer { getIntegerString :: String }
  | Float { getFloatString :: String }
  deriving Eq

instance Show Tag where
  show (Tag s) = "_" ++ s

instance Show Value where
  show Inapplicable = "<.>"
  show Unknown = "<?>"
  show (Numeric n) = show n
  show (String s) = show s
  show (Field ss) = show ss

instance Show Numeric where
  show (Numeric_ n) = show n
  show (NumericSU n u) = show n ++ "(" ++ u ++ ")"

instance Show Number where
  show (Integer n) = n
  show (Float n) = n

loopToAssocList :: DataItem -> [[(Tag,Value)]]
loopToAssocList (Loop tags vals) = map (zip tags) . chunksOf (length tags) $ vals
loopToAssocList _ = []

fromBody :: CIFBody -> [(Tag, [Value])]
fromBody = toAssocList . getBlock

toAssocList :: [DataItem] -> [(Tag,[Value])]
toAssocList = concatMap f
  where
    f :: DataItem -> [(Tag, [Value])]
    f (Item tag val) = [(tag,[val])]
    f (Loop tags vals) = zip tags . transpose . chunksOf (length tags) $ vals

-- getBlocks :: [CIFBody] -> [[DataItem]]
-- getBlocks cif = cif >>= \item -> getBlock' item
--   where
--     getBlock' (DataBlock _ b) = [b]
--     getBlock' _ = []

--------------

hasTag t0 (Item t1 _) = t0 == t1
hasTag t0 (Loop tags _) = t0 `elem` tags

query :: Tag -> [DataItem] -> [DataItem]
query t0 = filter (hasTag t0)

getString' (String s) = Just s
getString' _ = Nothing

getStrings (Item _ val) = (maybeToList . getString') val
getStrings (Loop _ vals) = vals >>= \n -> (maybeToList . getString') n

--------------

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
