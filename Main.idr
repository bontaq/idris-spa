module Main

import IdrisScript

data Attr = Text String
          | Listener String (JS_IO ())
          | Style String

record Node where
  constructor MkNode
  type : String
  attributes : List Attr
  children : List Node

div : List Attr -> List Node -> Node
div = MkNode "div"

text : String -> Node
text s = MkNode "text" [ Text s ] []

button : Node
button = MkNode "button" [] []

--
-- Javascript world
--

create : String -> IO' (MkFFI JS_Types String String) Ptr
create "div" = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "div"
create "text" = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "span"

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
    Text s =>
      jscall "(%0).innerText = %1"
      (Ptr -> String -> JS_IO ())
      ptr s
    Style s =>
      jscall "(%0).style.cssText = %1"
      (Ptr -> String -> JS_IO ())
      ptr s

app : Ptr -> List Attr -> JS_IO ()
app ptr [] = pure ()
app ptr (p::ps) = do
 applyAttributes ptr p
 app ptr ps

--
-- Core algorithm
--

render : Ptr -> Node -> IO' (MkFFI JS_Types String String) ()
render parent node = do
  let type = type node
  let attributes = (attributes node)

  -- get ptr to created el
  ptr <- create type

  -- append
  appendChild parent ptr

  -- apply attributes
  app ptr attributes

  sequence_ $ map (render ptr) (children node)

--
-- Entrance
--

getBody : JS_IO Ptr
getBody = do
  jscall "document.body"
    (JS_IO Ptr)

main : JS_IO ()
main = do
  log (toJS {from=String}{to=JSString} "hello")
  body <- getBody
  let mine : Node = div [
   Listener "onClick" helloWorld
   , Style "color: blue;"
   ]
   [ text "sup", text "another" ]
  render body mine

  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
