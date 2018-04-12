module Main

import Control.Monad.State
import IdrisScript

data Attr = Text String
          | Listener String (JS_IO ())

record Node where
  constructor MkNode
  type : String
  attributes : List $ Attr
  children : List Node

div : List Attr -> List Node -> Node
div = MkNode "div"

text : String -> Node
text s = MkNode "text" [ Text s ] []

button : Node
button = MkNode "button" [] []

create : String -> IO' (MkFFI JS_Types String String) Ptr
create "div" = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "div"

createText : List Attr -> IO' (MkFFI JS_Types String String) Ptr
createText [] = do
  jscall "document.createTextNode(%0)"
    (String -> JS_IO Ptr)
    "oops"
createText [Text t] = do
  jscall "document.createTextNode(%0)"
    (String -> JS_IO Ptr)
    t

appendChild : Ptr -> Ptr -> JS_IO ()
appendChild = do
  jscall "(%0).appendChild(%1)"
    (Ptr -> Ptr -> JS_IO ())

helloWorld : IO' (MkFFI JS_Types String String) ()
helloWorld =
  log (toJS {from=String}{to=JSString} "hello world!")

addEventListener : Ptr -> JS_IO () -> JS_IO ()
addEventListener ptr f = do
  jscall "(%0).addEventListener(\"click\", %1)"
    (Ptr -> (JsFn (() -> JS_IO ()) -> JS_IO ()))
    ptr (MkJsFn (\() => f))

applyAttributes : Ptr -> Attr -> JS_IO ()
applyAttributes ptr attr = do
  case attr of
    Listener s fn =>
      jscall "(%0).addEventListener(\"click\", %1)"
        (Ptr -> (JsFn (() -> JS_IO ()) -> JS_IO ()))
        ptr (MkJsFn (\() => fn))

app : Ptr -> List Attr -> JS_IO ()
app ptr [] = pure ()
app ptr [a] = applyAttributes ptr a

render : Ptr -> Node -> IO' (MkFFI JS_Types String String) ()
render parent node = do
  -- get ptr to created el
  let type = type node
  let attributes = (attributes node)

  ptr <- case type of
    "div" => create type
    "text" => createText attributes

  -- append
  appendChild parent ptr

  -- apply attributes
  case type of
    "div" => app ptr attributes

  let children = children node
  sequence_ $ map (render ptr) children
  pure ()

getBody : JS_IO Ptr
getBody = do
  jscall "document.body"
    (JS_IO Ptr)

main : JS_IO ()
main = do
  log (toJS {from=String}{to=JSString} "hello")
  body <- getBody
  let mine : Node = div [ Listener "onClick" helloWorld ] [ text "sup" ]
  render body mine

  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
