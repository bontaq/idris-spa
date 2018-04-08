module Main

import IdrisScript
import IdrisScript.Timer

setTimeout : (() -> JS_IO ()) -> (millies: Int) -> JS_IO Timeout
setTimeout f millies = do
  timeout <- jscall "setTimeout(%0, %1)"
                    (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                    (MkJsFn f) millies
  pure $ MkTimeout timeout

data Attribute msg
data Html msg

record Node where
  constructor MkNode
  type : String
  attributes : List ((String, String))
  children : List Node

-- internal representation for virtual dom
-- node : String -> List ((String, String)) -> List Node -> Node
-- node = MkNode

-- should just call node
-- div : IO' (MkFFI JS_Types String String) Ptr
-- div = do
--   jscall "document.createElement(%0)"
--     (String -> JS_IO Ptr)
--     "div"

div : List (String, String) -> List Node -> Node
div = MkNode "div"

text : Node
text = MkNode "text" [] []

create : String -> IO' (MkFFI JS_Types String String) Ptr
create "div" = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "div"
create "text" = do
  jscall "document.createTextNode(%0)"
    (String -> JS_IO Ptr)
    "text"

render : Node -> IO' (MkFFI JS_Types String String) ()
render node = do
  let type = type node
  ptr <- create type
  let attributes = node
  -- apply attributes
  let children = children node
  sequence_ $ map render children
  pure ()

insertAfterBody : Ptr -> IO' (MkFFI JS_Types String String) ()
insertAfterBody = do
  jscall "document.body.appendChild(%0)"
    (Ptr -> JS_IO ())

main : JS_IO ()
main = do
  log (toJS {from=String}{to=JSString} "hello")
  let mine : Node = div [] []
  -- myDiv <- div
  -- insertAfterBody myDiv
  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
