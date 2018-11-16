{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Data.List
import qualified Numeric as N

type Chars = String
type Input = Chars

data Parser a =
  Parser { runParser :: Input -> ParseResult a }

data ParseResult a = Err ParseError | Ok Input a

instance Show a => Show (ParseResult a) where
  show (Err e) = show e
  show (Ok i a) = concat ["Result >", i, "< ", show a]

data ParseError =
    UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | Failed
  deriving Eq

instance Show ParseError where
  show UnexpectedEof =
    "Unexpected end of stream"
  show (ExpectedEof i) =
    concat ["Expected end of stream, but got >", show i, "<"]
  show (UnexpectedChar c) =
    concat ["Unexpected character: ", show [c]]
  show Failed = "Parse failed"

anyChar :: Parser Char
anyChar = Parser $ \s ->
  case s of
    [] -> Err UnexpectedEof
    x : xs -> Ok xs x

unexpectedChar :: Char -> Parser a
unexpectedChar c = Parser $ \_ -> Err (UnexpectedChar c)

failed :: Parser a
failed = Parser $ \_ -> Err Failed

pureParser :: a -> Parser a
pureParser x = Parser $ \s -> Ok s x

valueParser = pureParser

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser fab pa = Parser $ \s -> 
  case runParser pa s of
    Ok s' x -> Ok s' (fab x)
    e @ (Err err) -> Err err

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser pa fab = Parser $ \s ->
  case runParser pa s of
    Ok s' x -> runParser (fab x) s'
    e @ (Err err) -> Err err

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = mapParser

instance Applicative Parser where
  pure :: a -> Parser a
  pure = pureParser
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = bindParser pf $ \f ->
              bindParser pa $ \x ->
              pureParser $ f x

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = bindParser

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser $ \s ->
  case runParser p s of
    Err err -> runParser q s
    result -> result

many :: Parser a -> Parser [a]
many pa = some pa <|> pure []

some :: Parser a -> Parser [a]
some pa = pa >>= \x ->
  many pa >>= \xs ->
    pure $ x : xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = anyChar >>= \c ->
  if p c then pure c else unexpectedChar c

is :: Char -> Parser Char
is c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

seqParser :: [Parser a] -> Parser [a]
seqParser [] = pure []
seqParser (p : ps) = p >>= \x ->
  seqParser ps >>= \xs ->
    pure $ x : xs

list :: Parser a -> Parser [a]
list k = list1 k <|> valueParser []

list1 :: Parser a -> Parser [a]
list1 k =
  k >>= (\k' ->
    (list k) >>= (\kk' ->
      valueParser (k' : kk')))

noneof :: Chars -> Parser Char
noneof s = satisfy (`notElem` s)

replicateA :: (Applicative f, Monad f) => Int -> f a -> f [a]
replicateA n = sequence . replicate n

hex :: Parser Char
hex =
  let hInt s = case readHex s of
                 Nothing -> 0
                 Just n -> n
  in chr . hInt <$> replicateA 4 (satisfy isHexDigit) 

hexu :: Parser Char
hexu = do
  is 'u'
  hex

string :: Chars -> Parser Chars
string = traverse is

between :: Parser b -> Parser c -> Parser a -> Parser a
between pb pc pa = pb *> pa <* pc

seqBy :: Parser b -> Parser a -> Parser [a]
seqBy pb pa = (pa >>= \h ->
                many (pb *> pa) >>= \t ->
                  pure $ h : t
              ) <|> pure []

readHexs :: (Eq a, Num a) => Chars -> Maybe (a, Chars)
readHexs s =
  case N.readHex s of
    [] -> Nothing
    ((a, q): _) -> Just (a, q)

readHex :: (Eq a, Num a) => Chars -> Maybe a
readHex = fmap fst . readHexs

readFloats :: (RealFrac a) => Chars -> Maybe (a, Chars)
readFloats s =
  case N.readSigned N.readFloat s of
    [] -> Nothing
    ((a, q):_) -> Just (a, q)

readFloat :: (RealFrac a) => Chars -> Maybe a
readFloat = fmap fst . readFloats

character :: Parser Char
character =
  Parser $ \s ->
    case s of
      [] -> Err UnexpectedEof
      (c : r) -> Ok r c

data SpecialCharacter =
  BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

fromSpecialCharacter :: SpecialCharacter -> Char
fromSpecialCharacter BackSpace = chr 0x08
fromSpecialCharacter FormFeed = chr 0x0C
fromSpecialCharacter NewLine = '\n'
fromSpecialCharacter CarriageReturn = '\r'
fromSpecialCharacter Tab = '\t'
fromSpecialCharacter VerticalTab = '\v'
fromSpecialCharacter SingleQuote = '\''
fromSpecialCharacter DoubleQuote = '"'
fromSpecialCharacter Backslash = '\\'

toSpecialCharacter :: Char -> Maybe SpecialCharacter
toSpecialCharacter c =
  let table = ('b', BackSpace) :
              ('f', FormFeed) :
              ('n', NewLine) :
              ('r', CarriageReturn) :
              ('t', Tab) :
              ('v', VerticalTab) :
              ('\'', SingleQuote) :
              ('"' , DoubleQuote) :
              ('\\', Backslash) :
              []
  in snd <$> find ((==) c . fst) table

stringTok :: Chars -> Parser Chars
stringTok = tok . string

tok p = do
  v <- p
  spaces
  pure v

charTok :: Char -> Parser Char
charTok = tok . is

commaTok :: Parser Char
commaTok = charTok ','

quote :: Parser Char
quote = is '"' <|> is '\''

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p s = do
  v <- p
  w <- list (s *> p)
  pure (v : w)

sepby :: Parser a -> Parser s -> Parser [a]
sepby p s = sepby1 p s <|> pure []

betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok a b = between (charTok a) (charTok b)

betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
betweenSepbyComma a b g = betweenCharTok a b $ g `sepby` charTok ','

space :: Parser Char
space = satisfy isSpace

spaces :: Parser Chars
spaces = list space

type Assoc = [(Chars, JsonValue)]

data JsonValue =
    JsonString Chars
  | JsonRational  Bool !Rational
  | JsonObject Assoc
  | JsonArray [JsonValue]
  | JsonTrue
  | JsonFalse
  | JsonNull
  deriving (Show, Eq)

jsonString :: Parser Chars
jsonString =
  let str =  do
      c1 <- character
      if c1 == '\\'
        then do
          c2 <- character
          if c2 == 'u'
            then hex
            else case toSpecialCharacter c2 of
              Nothing ->
                unexpectedChar c2
              Just d ->
                return (fromSpecialCharacter d)
      else
        if c1 == '"'
        then unexpectedChar '"'
        else return c1
  in between (is '"') (charTok '"') (list str)

jsonNumber :: Parser Rational
jsonNumber =
  Parser $ \i ->
    case readFloats i of
      Nothing -> Err Failed
      Just (n, z) -> Ok z n

jsonTrue :: Parser Chars
jsonTrue = stringTok "true"

jsonFalse :: Parser Chars
jsonFalse = stringTok "false"

jsonNull :: Parser Chars
jsonNull = stringTok "null"

jsonArray :: Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' jsonValue

jsonObject :: Parser Assoc
jsonObject =
  let field = (,) <$> (jsonString <* charTok ':') <*> jsonValue
  in betweenSepbyComma '{' '}' field

jsonValue :: Parser JsonValue
jsonValue = spaces *> (
  (pure JsonNull <* jsonNull) <|>
  (pure JsonTrue <* jsonTrue) <|>
  (pure JsonFalse <* jsonFalse) <|>
  (JsonArray <$> jsonArray) <|>
  (JsonString <$> jsonString) <|>
  (JsonObject <$> jsonObject) <|>
  (JsonRational False <$> jsonNumber)
  ) <* spaces

type Filename = String

readJsonValue :: Filename -> IO (ParseResult JsonValue)
readJsonValue filename =
  do
    s <- readFile filename
    return (runParser jsonValue inp)

main :: IO ()
main = readJsonValue "./sample.json" >>= print

{-
https://zhuanlan.zhihu.com/p/25867784

https://github.com/tonymorris/fp-course
https://github.com/fpinscala/fpinscala

sample.json
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}

JsonObject [
  ("Company name", JsonString "Microsoft Corporation"),
  ("Ticker", JsonString "MSFT"),
  ("Active", JsonTrue),
  ("Price", JsonRational False (1533 % 50)),
  ("Shares outstanding", JsonRational False (8380000000 % 1)),
  ("Related companies", JsonArray [
    JsonString "HPQ",
    JsonString "IBM",
    JsonString "YHOO",
    JsonString "DELL",
    JsonString "GOOG"]
  )
]

-}
