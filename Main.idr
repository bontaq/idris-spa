module Main

import IdrisScript
import IdrisScript.Timer

setTimeout : (() -> JS_IO ()) -> (millies: Int) -> JS_IO Timeout
setTimeout f millies = do
  timeout <- jscall "setTimeout(%0, %1)"
                    (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                    (MkJsFn f) millies
  pure $ MkTimeout timeout

div : IO' (MkFFI JS_Types String String) Ptr
div = do
  jscall "document.createElement(%0)"
    (String -> JS_IO Ptr)
    "div"

insertAfterBody : Ptr -> IO' (MkFFI JS_Types String String) ()
insertAfterBody = do
  jscall "document.body.appendChild(%0)"
    (Ptr -> JS_IO ())

main : JS_IO ()
main = do
  log (toJS {from=String}{to=JSString} "hello")
  myDiv <- div
  insertAfterBody myDiv
  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
