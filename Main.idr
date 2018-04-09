module Main

import Control.Monad.State
import IdrisScript

record Node where
  constructor MkNode
  type : String
  attributes : List ((String, String))
  children : List Node

div : List (String, String) -> List Node -> Node
div = MkNode "div"

text : Node
text = MkNode "text" [] []

button : Node
button = MkNode "button" [] []

create : String -> IO' (MkFFI JS_Types String String) Ptr
create "div" = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "div"
create "text" = do
  jscall "document.createTextNode(%0)"
    (String -> JS_IO Ptr)
    "text"

appendChild : Ptr -> Ptr -> JS_IO ()
appendChild = do
  jscall "(%0).appendChild(%1)"
    (Ptr -> Ptr -> JS_IO ())

render : Ptr -> Node -> IO' (MkFFI JS_Types String String) ()
render parent node = do
  -- get ptr to created el
  let type = type node
  ptr <- create type

  -- append
  appendChild parent ptr

  -- apply attributes
  let attributes = (attributes node)

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
  let mine : Node = div [] [ text, button ]
  render body mine

  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
