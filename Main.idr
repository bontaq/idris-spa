module Main

import IdrisScript
import IdrisScript.Timer

setTimeout : (() -> JS_IO ()) -> (millies: Int) -> JS_IO Timeout
setTimeout f millies = do
  timeout <- jscall "setTimeout(%0, %1)"
                    (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                    (MkJsFn f) millies
  pure $ MkTimeout timeout

main : JS_IO ()
main = do
  log (toJS {from=String}{to=JSString} "hello")
  pure ()

-- Local Variables:
-- idris-load-packages: ("idrisscript")
-- End:
