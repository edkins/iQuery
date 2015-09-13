module Elements

import IQuery.Event

private %inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

%access public

abstract
data Element : Type where
  MkElem : Ptr -> Element

abstract
data NodeList : Type where
  MkNodeList : Ptr -> NodeList

newElement : String -> JS_IO Element
newElement t =
  map MkElem $ jscall "document.createElement(%0)" (String -> JS_IO Ptr) t

newElementNS : String -> String -> JS_IO Element
newElementNS ns t =
  map MkElem $ jscall 
    "document.createElementNS(%0, %1)" (String -> String -> JS_IO Ptr) ns t

setProperty : Element -> String -> String -> JS_IO ()
setProperty (MkElem e) name value =
  jscall
    "%0[%1]=%2" (Ptr -> String -> String -> JS_IO ())
    e name value

getProperty : Element -> String -> JS_IO String
getProperty (MkElem e) name = 
  jscall
    "%0[%1]" (Ptr -> String -> JS_IO String)
    e name

setValue : Element -> String -> JS_IO ()
setValue = flip setProperty "value"

getValue : Element -> JS_IO String
getValue = flip getProperty "value"

setAttribute : Element -> String -> String -> JS_IO ()
setAttribute (MkElem e) name value =
  jscall
    "%0.setAttribute(%1,%2)" (Ptr -> String -> String -> JS_IO ())
    e name value

setAttributeNS : Element -> String -> String -> String -> JS_IO ()
setAttributeNS (MkElem e) ns name value =
  jscall
    "%0.setAttributeNS(%1,%2,%3)" (Ptr -> String -> String -> String -> JS_IO ())
    e ns name value

getAttribute : Element -> String -> JS_IO String
getAttribute (MkElem e) name = 
  jscall
    "%0.getAttribute(%1)" (Ptr -> String -> JS_IO String)
    e name

appendChild : Element -> Element -> JS_IO ()
appendChild (MkElem p) (MkElem c) =
  jscall
    "%0.appendChild(%1)" (Ptr -> Ptr -> JS_IO ())
    p c

removeChild : Element -> Element -> JS_IO ()
removeChild (MkElem p) (MkElem c) =
  jscall
    "%0.removeChild(%1)" (Ptr -> Ptr -> JS_IO ())
    p c

tagName : Element -> JS_IO String
tagName (MkElem e) = jscall "%0.tagName" (Ptr -> JS_IO String) e

getText : Element -> JS_IO String
getText (MkElem e) =
  jscall "%0.textContent" (Ptr -> JS_IO String) e

setText : Element -> String -> JS_IO ()
setText (MkElem e) s =
  jscall "%0.textContent=%1" (Ptr -> String -> JS_IO ()) e s

onEvent : EventType -> Element -> (Event -> JS_IO Int) -> JS_IO ()
onEvent ty (MkElem e) cb =
  let ev = show ty in
      jscall
        "%0.addEventListener(%1, %2)" (Ptr -> String -> (JsFn (Ptr -> JS_IO Int)) -> JS_IO ())
        e ev (MkJsFn (cb . MkEvent))

onClick : Element -> (Event -> JS_IO Int) -> JS_IO ()
onClick = onEvent Click

length : NodeList -> JS_IO Int
length (MkNodeList l) =
  jscall "%0.length" (Ptr -> JS_IO Int) l

elemAt : NodeList -> Int -> JS_IO (Maybe Element)
elemAt (MkNodeList l) i =
  if !(length $ MkNodeList l) > i then
    map (Just . MkElem) $ jscall "%0.item(%1)" (Ptr -> Int -> JS_IO Ptr) l i
  else
    return Nothing

query : String -> JS_IO NodeList
query q =
  map MkNodeList $ jscall "document.querySelectorAll(%0)" (String -> JS_IO Ptr) q

childNodes : Element -> JS_IO NodeList
childNodes (MkElem e) =
  map MkNodeList $ jscall "%0.childNodes" (Ptr -> JS_IO Ptr) e


