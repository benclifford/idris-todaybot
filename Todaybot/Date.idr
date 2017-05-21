module Todaybot.Date

import Todaybot.Ptr

-- represent a time_t (do it properly though, not this
-- manual way). like size_t should be done too in the
-- curl code.
public export TimeT : Type
TimeT = Int


public export getTime : IO TimeT
getTime = foreign FFI_C "time" (Ptr -> (IO TimeT)) null_pointer

