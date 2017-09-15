-- A JSON parser similar to that found in config, but
-- using Text.Parser rather than lightyear.

-- For now, I'm only implementing as much as I need to
-- parse things for todaybot.

module Todaybot.JSON

-- reuse datatypes from here.
import Config.JSON

-- import Text.Parser
import Todaybot.XXPARSER

%default total

public export specificChar : Char -> Grammar Char True Char
specificChar ch = terminal $ \c => case (c == ch) of
  True => Just ch
  False => Nothing

public export notChar : Char -> Grammar Char True Char
notChar ch = terminal $ \c => case (c /= ch) of
  True => Just c
  False => Nothing

public export anyChar : Grammar Char True Char
anyChar = terminal Just

public export wsChar : Grammar Char True Char
wsChar = terminal $ \c => case c of
  ' ' => Just c
  _ => Nothing

public export digit : Grammar Char True Char
digit = terminal $ \c => case
    c `elem`  (the (List Char) ['0','1','2','3','4','5','6','7','8','9'])
  of
    True => Just c
    False => Nothing

public export ws : Grammar Char False ()
ws = do
  Text.Parser.many wsChar
  pure ()

public export specificCharWS : Char -> Grammar Char True Char
specificCharWS ch = do
  c <- specificChar ch
  ws
  pure c

public export escapedChar : Grammar Char True Char
escapedChar = do
  specificChar '\\'
  commit
  terminal $ \c => Just c -- TODO - we don't need actual escapes to be parsed at the moment... its fine to skip them.
  pure 'X'
 

-- any character except closing quotes,
-- supporting (some) string escapes.
public export jsonStringCharacter : Grammar Char True Char
jsonStringCharacter = 
  escapedChar <|> notChar '"'

public export jsonStringAsList : Grammar Char True (List Char)
jsonStringAsList = do
  specificChar '"'
  commit
  s <- many jsonStringCharacter
  specificCharWS '"'
  pure s

public export jsonString : Grammar Char True Config.JSON.JsonValue
jsonString = do
  s <- jsonStringAsList
  pure $ JsonString (pack s)

-- this will parse "numbers" with any number of decimal points in
-- them, and then not work at the cast stage, rather than giving
-- a decent parser error. Lazy me. TODO
public export jsonNumber : Grammar Char True Config.JSON.JsonValue
jsonNumber = do
  ds <- some (digit <|> specificChar '.')
  pure $ JsonNumber (cast (pack ds))

public export jsonNull : Grammar Char True Config.JSON.JsonValue
jsonNull = do
  specificChar 'n'
  commit
  specificChar 'u'
  specificChar 'l'
  specificChar 'l'
  pure JsonNull

public export jsonTrue : Grammar Char True Config.JSON.JsonValue
jsonTrue = do
  specificChar 't'
  commit
  specificChar 'r'
  specificChar 'u'
  specificChar 'e'
  pure (JsonBool True)

public export jsonFalse : Grammar Char True Config.JSON.JsonValue
jsonFalse = do
  specificChar 'f'
  commit
  specificChar 'a'
  specificChar 'l'
  specificChar 's'
  specificChar 'e'
  pure (JsonBool False)

public export jsonBool : Grammar Char True Config.JSON.JsonValue
jsonBool = do
  jsonTrue <|> jsonFalse

public export objectValueSeparator : Grammar Char True ()
objectValueSeparator = do
  specificCharWS ','
  pure ()

{- QUESTION/DISCUSSION more generally, I keep forgetting that
   the type constructor for lists is List not [...] - unlike
   Haskell - because [...] for a list of types is valid at the
   type level
-}
public export packOnFst : List (List Char,rtype) -> List (String,rtype)
packOnFst [] = []
packOnFst ((a,b)::rest) = (pack a, b)::(packOnFst rest)

mutual 

  public export objectValuePair : Grammar Char True (List Char, Config.JSON.JsonValue)
  objectValuePair = do
    k <- jsonStringAsList
    specificCharWS ':'
    v <- jsonValue
    pure (k,v)

  public export jsonObject : Grammar Char True Config.JSON.JsonValue
  jsonObject = do
    specificChar '{'
    commit
    ws

    {- QUESTION/DISCUSSION if (k,v) is bound, with idris 1.1.1 I get
       this type error. If (k,v) is not bound (and the result discarded)
       this doesn't happen!

       It is unclear to me why this is getting problems apparently
       unifying on the 'consumes' parameter which shouldn't have
       an effect on the return type...

./Todaybot/JSON.idr:74:14:
When checking right hand side of Todaybot.JSON.case block in jsonObject at ./Todaybot/JSON.idr:74:14 with expected type
        Grammar Char c2 JsonValue

Type mismatch between
        Grammar Char
                (True || Delay False)
                JsonValue (Type of specificCharWS '}' >>=
                                   Delay (\__bindx => pure (JsonArray [])))
and
        Grammar Char c2 JsonValue (Expected type)

Specifically:
        Type mismatch between
                True || Delay False
        and
                c2

     -}
    {- replacing (k,v) with llll causes the binding to work out ok
       here... and then using fst llll and snd llll...
       This is against my intuition... worthy of QUESTION/DISCUSSION

       I think I've encountered this before?
    -}
    llll <- sepBy objectValueSeparator objectValuePair
    specificCharWS '}'
    pure $ JsonObject $ fromList $ packOnFst llll

  public export arrayVals : Grammar Char False (List Config.JSON.JsonValue)
  arrayVals = sepBy objectValueSeparator jsonValue

  public export jsonArray : Grammar Char True Config.JSON.JsonValue
  jsonArray = do
    specificChar '['
    commit
    llll <- arrayVals
    specificChar ']'
    pure $ JsonArray llll

  public export jsonValue : Grammar Char True Config.JSON.JsonValue
  jsonValue = 
        jsonObject
    <|> jsonString
    <|> jsonArray
    <|> jsonNumber
    <|> jsonNull
    <|> jsonBool

-- TODO: some way of getting a string representation would be nice
-- in Text.Parser itself
errMsg : ParseError Char -> String
errMsg (Error msg tok) = msg ++ ", remaining: " ++ show tok
errMsg _ = "ParseError, and furthermore, an unrecognised ParseError case"

public export fromString : String -> Either String Config.JSON.JsonValue
fromString sss = let
  toks : List Char = unpack sss
  resOrFail = parse toks jsonValue
  in case resOrFail
       of Left err => Left $ "JSON parser error: " ++ errMsg err
          Right (v, []) => Right v
          Right (v, rest) => Left $ "Parser did not consume everything, with " ++ show rest ++ " remaining"

