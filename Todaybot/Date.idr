module Todaybot.Date

import Todaybot.Ptr

-- QUESTION/DISCUSSION: choice in way to represent date is because
-- mostly I want to compare for equality rather than
-- doing other calendar operations, and will
-- in the post title space at least, always be getting
-- stuff as separate year, month and day.

-- This date type is quite todaybot specific, compared to
-- 'struct tm' which has the fields we want and more.
-- So maybe it should be in a separate module distinct from
-- the system date structures?
public export record Date where
  constructor MkDate
  year : Integer
  month : Integer -- could be a Fin 12
  day : Integer -- if we were really craaazy this could be a dependent type depending on month, and on year for leap years. Possibly a massive hassle and nothing useful in the todaybot case coming from that typing? QUESTION/DISCUSSION

-- QUESTION/DISCUSSION: what is the best way to implement this
-- interface? In Haskell land it would be automatically derivable
-- and Generics would also be able to implement it.
-- There is some discussion of doing this with the elaborator
-- but I haven't dug into that - probably I should.
-- Manually feels so ickily error prone, even for a three field
-- type.
public export Eq Date where
 l == r = (year l == year r)
       && (month l == month r)
       && (day l == day r)

-- QUESTION/DISCUSSION:
-- compareBy and use of thenCompare can probably be implemented
-- as nice function combinators, haskell style...
public export compareBy : Ord b => (a -> b) -> a -> a -> Ordering
compareBy f l r = compare (f l) (f r)

public export Ord Date where
  compare l r = (thenCompare (compareBy year l r) (thenCompare (compareBy month l r) (compareBy day l r)))
-- QUESTION/DISCUSSION: infix notation for thenCompare? `infix`
-- haskell notation didn't seem to work

public export Show Date where
  show date = "Todaybot date: " ++ (show . year) date
           ++ "/" ++ (show . month) date
           ++ "/" ++ (show . day) date


-- represent a time_t (do it properly though, not this
-- manual way). like size_t should be done too in the
-- curl code.
-- **BUG** introduced in 68eb0f518fe19dfe90c676a01339a93124e14e14
-- TimeT isn't a C integer; it is a long. When there is
-- lots of zeroed memory around, this bug doesn't always
-- appear, but valgrind always finds it when we pass a
-- TimeT into other C library code that tries to inspect
-- the value. Bits64 will do for now, probably. But a type
-- provider should provide it dynamically so it matches up with
-- the C prototypes...
-- QUESTION/DISCUSSION: how was I supposed to learn about Bit64
-- from the (FFI?) documentation? It took me a bunch of poking
-- to understand what was going on. Maybe because FFI is an
-- "advanced" topic (that I used on my first project)
public export TimeT : Type
TimeT = Bits64

public export getTime : IO TimeT
getTime = foreign FFI_C "time" (Ptr -> (IO TimeT)) null_pointer

-- QUESTION/DISCUSSION: this should be/needs to be called to initialise
-- timezones in libc before calling timeTToDate (or anything
-- which calls localtime_r).
-- This requirement to initialise should be captured in idris somewhere
-- (a timezone conversion effect? which looks functional except that
-- tzset is called at the start? that might be a bit icky in threads
-- though? I don't know if tzset is thread safe... for current todaybot
-- purposes that doesn't really matter)
public export tzSet : IO ()
tzSet = foreign FFI_C "tzset" (IO ())

public export tzinfo : IO ()
tzinfo = foreign FFI_C "tzinfo_benc" (IO ())

-- TODO: something to turn a TimeT into a 'struct tm'
-- 'localtime' will do that but is not thread safe.
-- 'localtime_r' is thread safe but needs some memory to be
-- allocated for the result.

public export timeTToDate : TimeT -> IO Date
timeTToDate timet = do

  -- ok lets get a load of space into which we can put a tm
  -- we should size it correctly, but for now just allocate a load
  -- and hope that it is big enough. TODO: make this based on size
  -- of struct tm
  -- TODO: don't forget to release this
  tm_ptr <- alloc_bytes 256
  -- this memory will be populated by a later call to localtime_r
  -- so does not need initialising.
 
  -- need to pass in timet as a Ptr not as a raw value
  -- QUESTION/DISCUSSION: why does localtime_r take that as a
  -- pointer not a raw time_t? does it modify it? 

  -- TODO: don't forget to release this
  timet_ptr <- alloc_bytes 256 -- should be sizeof timet
  -- timet_ptr needs to be populated with the value of timet
  poke_long timet_ptr timet

  -- struct tm *localtime_r(const time_t *timep, struct tm *result);
  foreign FFI_C "localtime_r_benc" (Ptr -> Ptr -> IO ()) timet_ptr tm_ptr

  -- so now we have a populated tm_ptr
  -- I'd like to get at the three fields of that, day, month and
  -- year: tm_year, tm_mon tm_mday. NB that tm_mon is 0 based, and
  -- tm_year is 1900 based.
  -- QUESTION/DISCUSSION: Can type providers give me those accessors?

  y <- foreign FFI_C "get_tm_year" (Ptr -> IO Int) tm_ptr
  m <- foreign FFI_C "get_tm_mon" (Ptr -> IO Int) tm_ptr

  -- QUESTION/DISCUSSIOn:
  -- if the output FFI type is 'IO Integer' rather than 'IO Int',
  -- this error appears:

{-
When checking right hand side of timeTToDate with expected type
        IO Date

When checking argument fty to function foreign:
        Can't find a value of type 
                FTy FFI_C [] (Ptr -> IO Integer)

-}
  -- presumably this is because Integer can't be marshalled to the
  -- C side of things? But it isn't apparent immediately that
  -- the *integer* is the problem in the supplied error message.

  d <- foreign FFI_C "get_tm_mday" (Ptr -> IO Int) tm_ptr

  -- QUESTION/DISCUSSION: are these casts safe? I think so because
  -- we're moving to a "bigger" type.
  pure (MkDate (cast y+1900) (cast m+1) (cast d))
