module Todaybot.TitleParser

-- this parser is based on the one used in Haskell lsc-todaybot

import Data.String

import Effects
import Effect.Exception

import Text.Parser

import Todaybot.Date
import Todaybot.Morph

public export oneOf : Eq x => List x -> Grammar x True x
oneOf syms = terminal (\c => if c `elem` syms then Just c else Nothing)

public export noneOf : Eq x => List x -> Grammar x True x
noneOf syms = terminal (\c => if c `elem` syms then Nothing else Just c)

public export char : Eq x => x -> Grammar x True x
char sym = terminal (\c => if c == sym then Just c else Nothing)

public export anyChar : Grammar x True x
anyChar = terminal Just

public export pureOrFail : Maybe a -> Grammar x False a
pureOrFail (Just a) = pure a
pureOrFail (Nothing) = fail "could not parse digit sequence (impossible?)"

partial public export dateComponent : Grammar Char True Integer
dateComponent = do
  syms <- (some . oneOf . unpack) "0123456789"
  let nM = (parsePositive . cast) syms -- cast from List Char to String
  pureOrFail nM

{- QUESTION/DISCUSSION In the lightyear based parser which
precedes the current implementation in version control,
could say this; but cases can't be used in Text.Parser do blocks.
The case can be moved into a separate function, though, (pureOrFail)
because the type can be more explicitly specified there.

See:
http://benctechnicalblog.blogspot.co.uk/2017/09/pattern-matching-in-idris-do-notation.html

  let nM = (parsePositive . cast) syms -- cast from List Char to String
  case nM of 
    Just n => pure n
    Nothing => fail "couldn't parse digit sequence (impossible?)"
-}

-- turn a 2-or-4 digit year into a 4-digit year with
-- assumptions about when we're actually running.

public export normaliseYear : Integer -> Integer
normaliseYear i = case i > 2000 of
  True => i
  False => 2000 + i -- hello, year 2100!

-- QUESTION/DISCUSSION: this has to be 'public (export)' to be used in the
-- public "titleToDate" function. so what is the point of
-- private functions if they can't be exposed outside the module?
-- go read about that...
public export titleDateParser : Grammar Char True Date
titleDateParser = do
  many $ noneOf (unpack "[")
  char '['
  d <- dateComponent
  sep <- oneOf ['/', '-', '.', '\\']
  m <- dateComponent
  char sep
  y <- dateComponent
  char ']'
  many anyChar
  pure (MkDate (normaliseYear y) m d)


public export showParseError : Show tok => ParseError tok -> String
showParseError (Error err toks) = err ++ "(tokens: " ++ show toks ++ ")"

partial public export titleToDate : Maybe String -> Eff Date [EXCEPTION String]
titleToDate m_title = 
  case m_title of
    Nothing => raise "Given no title to parse"
    Just title => 
    case parse (unpack title) titleDateParser of
      Right (v, []) => pure v
      Left err => raise $ "titleDateParser failed: " ++ showParseError err
      Right (v, _::_) => raise $ "titleDateParser had leftover content"


{- old lightyear based implementation:

-- QUESTION/DISCUSSION:
-- I just figured out where Lightyear comes from. Parsec. Ahaha.
import Lightyear
import Lightyear.Char
import Lightyear.Strings



-- QUESTION/DISCUSSION: 'many' and 'some' are not total here. why?
-- This also leads to titleDateParser being partial and so on. Which
-- is not what I want: it leads to todaybot as a whole being partial. Ugh.
-- Is there a way I can have a total 'some' combinator, that doesn't
-- recurse infinitely which I guess is what is making it potentially
-- non-terminating (on infinite strings). In the date case, yes, I think,
-- because I should have a finite number of digits (usually a 4 digit
-- year for example, but to deal with the 10k problem, the finite
-- number could also be the length of the string to be parsed, which
-- is finite and known ahead of time in todaybot)
-- Converting the parsed string to an integer is also partial because
-- it might fail (even though informally we know it will work because
-- we've only taken digits) so maybe there's a combined parser that
-- will deal better with both at once?


partial public export dateComponent : Parser Integer
dateComponent = do
  syms <- some $ oneOf "0123456789"
  let nM = (parsePositive . cast) syms -- cast from List Char to String
  case nM of 
    Just n => pure n
    Nothing => fail "couldn't parse digit sequence (impossible?)"

-- turn a 2-or-4 digit year into a 4-digit year with
-- assumptions about when we're actually running.

public export normaliseYear : Integer -> Integer
normaliseYear i = case i > 2000 of
  True => i
  False => 2000 + i -- hello, year 2100!

-- QUESTION/DISCUSSION: this has to be 'public (export)' to be used in the
-- public "titleToDate" function. so what is the point of
-- private functions if they can't be exposed outside the module?
-- go read about that...
partial public export titleDateParser : Parser Date
titleDateParser = do
  many $ noneOf "["
  char '['
  d <- dateComponent
  char '/'
  m <- dateComponent
  char '/'
  y <- dateComponent
  char ']'
  pure (MkDate (normaliseYear y) m d)

-- 'partial' because of partial dateComponent - see notes there.
partial public export titleToDate : String -> Maybe Date
titleToDate title = eitherToMaybe $ parse titleDateParser title

-}
