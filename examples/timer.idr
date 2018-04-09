setTimeout : (() -> JS_IO ()) -> (millies: Int) -> JS_IO Timeout
setTimeout f millies = do
  timeout <- jscall "setTimeout(%0, %1)"
                    (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                    (MkJsFn f) millies
  pure $ MkTimeout timeout
