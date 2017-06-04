module Todaybot.Ptr

-- QUESTION/DISCUSSION: this poke stuff should be able to be
-- made into a single poke that knows what to poke based on
-- its parameter, like haskell's storable type classes?
-- Likewise, alloc should be able to do that too?


-- This should be safe because NULL is (I think?) a constant.
public export null_pointer : Ptr
null_pointer = unsafePerformIO $ foreign FFI_C "get_null_pointer" (IO Ptr)

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

