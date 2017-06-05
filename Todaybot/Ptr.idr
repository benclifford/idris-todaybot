module Todaybot.Ptr

{- QUESTION/DISCUSSION: Naming(!)

Namng Effects vs Effect.* is ugh

-}
import Effects
import Effect.Exception

%include C "ptr.h"
%link C "ptr.o"

-- QUESTION/DISCUSSION: this poke stuff should be able to be
-- made into a single poke that knows what to poke based on
-- its parameter, like haskell's storable type classes?
-- Likewise, alloc should be able to do that too?


-- This should be safe because NULL is (I think?) a constant.
public export null_pointer : Ptr
null_pointer = unsafePerformIO $ foreign FFI_C "get_null_pointer" (IO Ptr)

public export checkPointerNotNull : Ptr -> Eff () [EXCEPTION String]
checkPointerNotNull ptr =
  if ptr == null_pointer 
  then raise "checkPointerNotNull: Null pointer"
  else pure ()

-- TODO: this should be captured at compile time
-- eg using type providers as in example
public export SizeT : Type
SizeT = Int

-- TODO: all this memory stuff should take account of CData
-- which I discovered after implementing this. Specifically
-- it looks like we can do garbage collection, if careful.

-- TODO: memory allocation should probably do some validity checking
-- and have some appropriate exception-effect or some such on
-- failure, rather than returning NULL?

public export alloc_bytes : SizeT -> IO Ptr
alloc_bytes count = foreign FFI_C "alloc_bytes" (SizeT -> IO Ptr) count

public export realloc_bytes : Ptr -> SizeT -> IO Ptr
realloc_bytes old count = foreign FFI_C "realloc" (Ptr -> SizeT -> IO Ptr) old count

public export free : Ptr -> IO ()
free ptr = foreign FFI_C "free" (Ptr -> IO ()) ptr

public export memcpy : Ptr -> Ptr -> SizeT -> IO Ptr
memcpy dest src count = foreign FFI_C "memcpy" (Ptr -> Ptr -> SizeT -> IO Ptr) dest src count


-- TODO: should be a byte not an int
public export poke_byte : (base : Ptr) -> (offset : Int) -> (value : Int) -> IO ()
poke_byte base offset value = foreign FFI_C "poke_byte" (Ptr -> Int -> Int -> IO ()) base offset value

public export poke_long : (base : Ptr) -> (value : Bits64) -> IO ()
poke_long base value = foreign FFI_C "poke_long" (Ptr -> Bits64 -> IO ()) base value

-- TODO: inconsistent offset UI wrt poke_byte
public export poke_ptr : (base : Ptr) -> (value : Ptr) -> IO ()
poke_ptr base value = foreign FFI_C "poke_ptr" (Ptr -> Ptr -> IO ()) base value

public export peek_ptr : (base : Ptr) -> IO Ptr
peek_ptr base = foreign FFI_C "peek_ptr" (Ptr -> IO Ptr) base

-- QUESTION/DISCUSSION: Really I would like some kind of
-- Eq on pointers. But I think maybe I don't have those?

public export is_null : Ptr -> Bool
is_null ptr = ptr == null_pointer

{-
is_null : Ptr -> Bool
is_null ptr = do
  v <- foreign FFI_C "is_null" (Ptr -> IO Int) ptr
  case v of
    501 => True
    502 => False
-}


-- QUESTION/DISCUSSION: really the top level todaybot code shouldn't be
-- exposing a MEMORY effect: that is an effect needed by the CURL
-- effect implementation, rather than by todaybot - because type signatures
-- need to contain (I think) the entire effects list.
namespace effect

  public export data Memory : Effect where
    AllocBytes : SizeT -> sig Memory Ptr
    PokePtr : Ptr -> Ptr -> sig Memory ()
    PeekPtr : Ptr -> sig Memory Ptr
    Free : Ptr -> sig Memory ()

  public export MEMORY : EFFECT
  MEMORY = MkEff () Memory

  public export alloc_bytes : SizeT -> Eff Ptr [MEMORY]
  alloc_bytes size = call $ AllocBytes size

  public export poke_ptr : (base : Ptr) -> (value : Ptr) -> Eff () [MEMORY]
  poke_ptr base value = call $ PokePtr base value

  public export peek_ptr : (base : Ptr) -> Eff Ptr [MEMORY]
  peek_ptr base = call $ PeekPtr base

  public export free : (base : Ptr) -> Eff () [MEMORY]
  free base = call $ Free base

  public export Handler Memory IO where
    handle () (AllocBytes size) k = do
      ptr <- alloc_bytes size
      k ptr ()

    handle () (PokePtr base value) k = do
      poke_ptr base value
      k () ()

    handle () (PeekPtr base) k = do
      v <- peek_ptr base
      k v ()

    handle () (Free base) k = do
      free base
      k () ()

