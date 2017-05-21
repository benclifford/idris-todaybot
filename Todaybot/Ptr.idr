module Todaybot.Ptr

--- This should be safe because NULL is (I think?) a constant.
public export null_pointer : Ptr
null_pointer = unsafePerformIO $ foreign FFI_C "get_null_pointer" (IO Ptr)

