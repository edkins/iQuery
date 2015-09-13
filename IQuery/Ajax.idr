module Ajax

private %inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

%access private

data XMLHttpRequest : Type where
  MkXHR : Ptr -> XMLHttpRequest

data ReadyState : Type where
  Unsent : ReadyState
  Opened : ReadyState
  HeadersReceived : ReadyState
  Loading : ReadyState
  Done : ReadyState

public
data Method : Type where
  GET : Method
  POST : Method

new : JS_IO XMLHttpRequest
new = [| MkXHR (jscall "new XMLHttpRequest" (JS_IO Ptr)) |]

open : XMLHttpRequest -> Method -> String -> Bool -> JS_IO ()
open (MkXHR xhr) method url async =
  jscall
    "%0.open(%1,%2,%3)" (Ptr -> String -> String -> Int -> JS_IO ())
    xhr (toMethod method) url (toAsync async)
  where toMethod : Method -> String
        toMethod GET = "GET"
        toMethod POST = "POST"

        toAsync : Bool -> Int
        toAsync True = 1
        toAsync False = 0

setRequestHeader : XMLHttpRequest -> String -> String -> JS_IO ()
setRequestHeader (MkXHR xhr) name value =
  jscall
    "%0.setRequestHeader(%1, %2)" (Ptr -> String -> String -> JS_IO ())
    xhr name value

readyState : XMLHttpRequest -> JS_IO ReadyState
readyState (MkXHR xhr) = do
  r <- jscall "%0.readyState" (Ptr -> JS_IO Int) xhr
  pure $ case r of
              1 => Opened
              2 => HeadersReceived
              3 => Loading
              4 => Done
              _ => Unsent

responseText : XMLHttpRequest -> JS_IO String
responseText (MkXHR xhr) = jscall "%0.responseText" (Ptr -> JS_IO String) xhr

status : XMLHttpRequest -> JS_IO Int
status (MkXHR xhr) = jscall "%0.status" (Ptr -> JS_IO Int) xhr

-- TODO: check we pass in f and not (const f)
onReadyStateChange : XMLHttpRequest -> JS_IO () -> JS_IO ()
onReadyStateChange (MkXHR x) f =
  jscall
    "%0.onreadystatechange=%1" (Ptr -> JsFn (JS_IO ()) -> JS_IO ())
    x (MkJsFn f)

send : XMLHttpRequest -> String -> JS_IO ()
send (MkXHR xhr) r = jscall "%0.send(%1)" (Ptr -> String -> JS_IO ()) xhr r

public
ajax : Method -> String -> Bool -> List (String, String) -> String ->
       (Either Int String -> JS_IO ()) -> JS_IO ()
ajax method url async headers dat callback = do
  xhr <- new
  open xhr method url async
  traverse (uncurry $ setRequestHeader xhr) headers
  onReadyStateChange xhr $ do rs <- readyState xhr
                              case rs of
                                   Done => do s <- status xhr
                                              case s of
                                                  200 => do t <- responseText xhr
                                                            callback $ Right t
                                                  _ => callback $ Left s
                                   _ => return ()
  send xhr dat

