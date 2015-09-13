module Timeout

private %inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

%access public

abstract
data Timeout : Type where
  MkTimeout : Ptr -> Timeout

setTimeout : (() -> JS_IO ()) -> Float -> JS_IO Timeout
setTimeout f t = do
  e <- jscall
    "setTimeout(%0,%1)" (JsFn (() -> JS_IO ()) -> Float -> JS_IO Ptr)
    (MkJsFn f) t
  return (MkTimeout e)

clearTimeout : Timeout -> JS_IO ()
clearTimeout (MkTimeout p) =
  jscall "clearTimeout(%0)" (Ptr -> JS_IO ()) p

