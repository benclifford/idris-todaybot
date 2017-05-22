module Todaybot.TitleParser

import Data.String

-- QUESTION/DISCUSSION:
-- I just figured out where Lightyear comes from. Parsec. Ahaha.
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Todaybot.Date
import Todaybot.Morph

-- this parser is based on the one used in Haskell lsc-todaybot


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

{- haskell code:
datedSubjectLine = prefixDatedSubjectLine
               <|> postfixDatedSubjectLine

prefixDatedSubjectLine = dateBlock
  -- ignore the rest of the line...

postfixDatedSubjectLine = do
  many $ P.noneOf "["
  d <- dateBlock
  P.eof
  return d

dateBlock = do
  P.char '['
  day <- dateComponent
  dateSeparator
  month <- dateComponent
  dateSeparator
  year <- yearComponent
  P.char ']'
  return $ fromGregorian year month day

dateSeparator = P.oneOf "/-."

dateComponent = read <$> digits

yearComponent = (normaliseYear . read) <$> digits

digits = (P.many $ P.oneOf "0123456789")

normaliseYear year =
  case () of
    _ | year > 2000 -> year
    _ | year >= 0 && year < 100 -> 2000 + year -- hello, 2100!

-}
