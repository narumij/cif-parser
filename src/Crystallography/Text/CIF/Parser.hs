module Crystallography.Text.CIF.Parser (
  cif,
) where

import Control.Monad
import Data.Maybe
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (GenParser)

import Crystallography.Text.CIF.Contents

sp :: GenParser Char st Char
sp = char ' ' <?> "<SP>"

ht :: GenParser Char st Char
ht = char '\t' <?> "<HT>"

eol :: GenParser Char st String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "<eol>"

--noteol = noneOf "\n\r"

ordinaryChar :: GenParser Char st Char
ordinaryChar = oneOf [
  '!','%','&','(',')','*','+',',','-','.','/',
  '0','1','2','3','4','5','6','7','8','9',
  ':','<','=','>','?','@',
  'A','B','C','D','E','F','G',
  'H','I','J','K','L','M','N',
  'O','P','Q','R','S','T','U',
  'V','W','X','Y','Z',
  '\\','^','`',
  'a','b','c','d','e','f','g',
  'h','i','j','k','l','m','n',
  'o','p','q','r','s','t','u',
  'v','w','x','y','z',
  '{','|','}','~'] <?> "<OridinaryChar>"

singleQuote :: GenParser Char st Char
singleQuote = char '\''

doubleQuote :: GenParser Char st Char
doubleQuote = char '"'

nonBlankChar :: GenParser Char st Char
nonBlankChar = ordinaryChar <|> doubleQuote <|> char '#' <|> char '$'
  <|> singleQuote <|> char '_' <|> char ';' <|> char '[' <|> char ']'

anyPrintChar :: GenParser Char st Char
anyPrintChar = ordinaryChar <|> doubleQuote <|> char '#' <|> char '$'
  <|> singleQuote <|> char '_' <|> sp <|> ht <|> char ';' <|> char '[' <|> char ']'

textLeadChar :: GenParser Char st Char
textLeadChar = ordinaryChar <|> doubleQuote <|> char '#' <|> char '$'
  <|> singleQuote <|> char '_' <|> sp <|> ht <|> char '[' <|> char ']'
-- anyChar a = a <|> a

comment = do
  string "#"
--  many anyPrintChar
  many $ noneOf "\r\n"
  eol
  return "<Comments>"

comments = many1 comment

sp' :: GenParser Char st String
sp' = string " " <?> "<SP>"

ht' :: GenParser Char st String
ht' = string "\t" <?> "<HT>"

tokenizedComments = do
  many (sp' <|> ht' <|> eol)
  comments
  return "<TokenizedComments>" <?> "<TokenizedComments>"

whiteSpace = do
  many1 (sp' <|> ht' <|> eol <|> tokenizedComments)
  return "<WhiteSpace>" <?> "<WhiteSpace>"

eof' :: GenParser Char st String
eof' = do
  eof
  return "<EOF>"

data_ :: GenParser Char st String
data_ = do
  oneOf "dD"
  oneOf "aA"
  oneOf "tT"
  oneOf "aA"
  char '_'
  return "<DATA_>"

loop_ :: GenParser Char st String
loop_ = do
  oneOf "lL"
  oneOf "oO"
  oneOf "oO"
  oneOf "pP"
  char '_'
  return "<LOOP_>"

save_ :: GenParser Char st String
save_ = do
  oneOf "sS"
  oneOf "aA"
  oneOf "vV"
  oneOf "eE"
  char '_'
  return "<SAVE_>"

stop_ :: GenParser Char st String
stop_ = do
  oneOf "sS"
  oneOf "tT"
  oneOf "oO"
  oneOf "pP"
  char '_'
  return "<STOP_>"

global_ :: GenParser Char st String
global_ = do
  oneOf "gG"
  oneOf "lL"
  oneOf "oO"
  oneOf "bB"
  oneOf "aA"
  oneOf "lL"
  char '_'
  return "<GLOBAL_>"

singleQuoteString :: GenParser Char st String
singleQuoteString = do
  singleQuote
  s <- many anyPrintChar'
  singleQuote <?> "single quote at end of string"
  --return $ "<SingleQuoteString " ++ s ++ ">"
  return s
  where
    anyPrintChar' = noneOf "'"
      <|> try (string "''" >> return '\'')

doubleQuoteString :: GenParser Char st String
doubleQuoteString = do
  doubleQuote
  s <- many anyPrintChar'
  doubleQuote <?> "single quote at end of string"
  --return $ "<DoubleQuoteString " ++ s ++ ">"
  return s
  where
    anyPrintChar' = noneOf "\""
      <|> try (string "\"\"" >> return '\"')

-- textField :: GenParser Char st String
textField = semiColonTextField

semiColonTextField :: GenParser Char st [String]
semiColonTextField = do
  delim
  t <- manyTill anyChar (try delim)
  return ["<SemiColonTextField>",t]
  where
    delim = sol >> string ";"

bracketTextField = error "not implemented yet"

sol :: GenParser Char st ()
sol = do
  c <- sourceColumn <$> getPosition
  -- guard (c == 1) >> return "<HeadOfLine>"
  unless (c == 1) parserZero

notsol :: GenParser Char st ()
notsol = do
  c <- sourceColumn <$> getPosition
  unless (c /= 1) parserZero

--tag :: GenParser Char st Tag
tag = do
  char '_'
  s <- many nonBlankChar
  return $ Tag s



--value :: GenParser Char st String
value = try numeric' <|> try textField' <|> try charString' <|> try inapp <|> try unknown
  where
    inapp = do
      string "."
      return Inapplicable
    unknown = do
      string "?"
      return Unknown
    numeric' = do
      n <- numeric
      lookAhead whiteSpace
      return $ Numeric n

charString' = do
  s <- charString
  return $ String s

textField' = do
  ss <- textField
  return $ Field ss

value' = textField'

value'' = charString'

--numeric :: GenParser Char st String
numeric = do
   n <- try number' <|> number''
   return $ n
   where
     number' = do
       nn <- number
       char '('
       su <- unsignedInteger
       char ')'
       -- return $ nn ++ "<s.u>"
       return $ NumericSU nn su
     number'' = do
      nn <- number
      return $ Numeric_ nn

--number :: GenParser Char st String
number = try float <|> integer

unsignedInteger :: GenParser Char st String
unsignedInteger = many1 digit

exponent' :: GenParser Char st String
exponent' = do
  e <- try ee <|> e
  i <- unsignedInteger
  return $ e ++ i
  where
    e = do
      c <- oneOf "eE"
      return [c]
    ee = do
      c0 <- oneOf "eE"
      c1 <- oneOf "+-"
      return [c0,c1]

sign (Just n) = [n]
sign _ = ""

integer = do
  s <- optionMaybe $ oneOf "+-"
  n <- unsignedInteger
  return $ Integer (sign s ++ n)

floatA = do
      i <- integer
      e <- exponent'
      return $ getIntegerString i ++ e

floatB = do
  s <- optionMaybe $ oneOf "+-"
  n <- try floatB' <|> floatB''
  e <- optionMaybe exponent'
  return $ sign s ++ n ++ fromMaybe "" e

floatB' = do
      n <- many digit
      char '.'
      ns <- unsignedInteger
      return (n++"."++ns)

floatB'' = do
      n <- many1 digit
      char '.'
      return (n++".")

float = do
  s <- try floatA <|> floatB
  return $ Float s

charString = singleQuoteString <|> doubleQuoteString <|> unquoteString
-- charString = singleQuoteString <|> doubleQuoteString

unquoteString = try unquoteStringA <|> try unquoteStringB

--unquoteStringA :: GenParser Char st String
unquoteStringA = sol >> do
  --eol
  c <- ordinaryChar
  cs <- many nonBlankChar
  --return "<UnquoteStringA>"
  let st = c:cs
  -- guard (st /= "loop_") >> return ("<A>"++st)
  guard (st /= "loop_") >> guard (st /= ".") >> guard (st /= "?") >> return st

unquoteStringB = notsol >> do
  --noteol
  c <- ordinaryChar <|> char ';'
  cs <- many nonBlankChar
  --return "<UnquoteStringB>"
  let st = if c == ';' then cs else c:cs
  -- return $ if c == ';' then cs else c:cs
  -- guard (st /= "loop_") >> return ("<B>"++st)
  guard (st /= "loop_") >> guard (st /= ".") >> guard (st /= "?") >> return st

---------------------

-- cif = do
--   c <- cif'
--   return c

cif = do
  optional comments
  optional whiteSpace
  c <- optionMaybe $ do
    b <- dataBlock
    -- bs <- many $ do
    --   whiteSpace
    --   bb <- dataBlock
    --   optional whiteSpace
    --   return bb
    -- return $ b:bs
    return [b]
  return (fromMaybe [] c)

dataBlock = do
  h <- dataBlockHeading
  -- d <- many $ do
  --   whiteSpace
  --   dataItem
  d <- many (try a <|> try b)
  return (DataBlock h (catMaybes d))
  where
    a = do
      whiteSpace
      eof
      return Nothing
    b = do
      whiteSpace
      d <- dataItem
      return (Just d)

dataBlockHeading = do
  data_
  many1 nonBlankChar

-- saveFrame = do
--   saveFrameHeading
--
-- saveFrameHeading = do
--   save_
--   many1 nonBlankChar

dataItem = dataItemA <|> dataItemB <?> "<DataItem>"

dataItemA = do
  t <- tag <?> "<DataItemA-Tag>"
  v <- try b <|> a <?> "<DataItemA-Value>"
  return $ Item t v
  where
    a = do
      whiteSpace <?> "<DataItemA-WhiteSpace>"
      value <?> "<DataItemA-Value>"
    b = value' <?> "<DataItemA-Value'>"

dataItemB = do
  header <- loopHeader <?> "<DataItemB-LoopHeader>"
  body <- loopBody <?> "<DataItemB-LoopBody>"
  return $ Loop header body

loopHeader = do
  loop_
  many1 $ try $ do
    whiteSpace
    tag

loopBody = do
  x <- try b <|> a
  xs <- many $ do
    try b <|> try a
  return (x:xs)
  where
    a = do
      whiteSpace
      value
    b = do
      value'
