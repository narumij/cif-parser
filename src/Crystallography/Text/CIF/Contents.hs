module Crystallography.Text.CIF.Contents (
  Body(..),
  DataItem(..),
  Tag(..),
  Value(..),
  Numeric(..),
  Number(..),
  getStrings,
  getUncert,
) where

import Data.Maybe

data Body
  = DataBlock {
    getHeading :: String,
    getBlock :: [DataItem]
    }
  | SaveFrame {
    getHeading :: String,
    getSaves :: [()]
    }
  deriving Show

data DataItem
  = Item {
    getTag :: Tag,
    getValue :: Value
    }
  | Loop {
    getTags :: [Tag],
    getValues :: [Value]
    }
  deriving Show

newtype Tag
  = Tag String
  deriving (Eq,Ord)

data Value
  = Inapplicable
  | Unknown
  | Numeric {
    getNumeric :: Numeric
    }
  | String {
    getString :: String
    }
  | Field {
    getField :: [String]
    }
  deriving Eq

data Numeric
  = Numeric_ {
    getNumber :: Number
    }
  | NumericSU {
    getNumber :: Number,
    uncert :: String
    }
  deriving Eq

getUncert :: Numeric -> Maybe String
getUncert (Numeric_ _) = Nothing
getUncert (NumericSU _ su) = Just su

data Number
  = Integer {
    getIntegerString :: String
    }
  | Float {
    getFloatString :: String
    }
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

getString' :: Value -> Maybe String
getString' (String s) = Just s
getString' _ = Nothing

getStrings :: DataItem -> [String]
getStrings (Item _ val) = (maybeToList . getString') val
getStrings (Loop _ vals) = vals >>= \n -> (maybeToList . getString') n
