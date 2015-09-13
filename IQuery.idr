module IQuery

import IQuery.Timeout
import IQuery.Interval
import IQuery.Event
import IQuery.Elements
import IQuery.State

private %inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

%access public

alert : String -> JS_IO ()
alert msg =
  jscall "alert(%0)" (String -> JS_IO ()) msg

