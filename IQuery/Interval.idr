module Interval

private %inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

%access public

abstract
data Interval : Type where
  MkInterval : Ptr -> Interval

setInterval : (() -> JS_IO ()) -> Float -> JS_IO Interval
setInterval f t = do
  e <- jscall
    "setInterval(%0,%1)" (JsFn (() -> JS_IO ()) -> Float -> JS_IO Ptr)
    (MkJsFn f) t
  return (MkInterval e)

clearInterval : Interval -> JS_IO ()
clearInterval (MkInterval p) =
  jscall "clearInterval(%0)" (Ptr -> JS_IO ()) p

