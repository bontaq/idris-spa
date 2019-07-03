module Learning

import Control.Monad.State
import Control.Monad.Trans
-- import Control.ST

inc3 : String -> StateT String IO ()
inc3 str = do
  lift $ putStrLn "hello"
  a <- get
  -- a <- pure $ lift . get
  -- a <- execState $ get
  -- putStrLn $ a (Prelude.Strings.++) str
  pure ()

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
