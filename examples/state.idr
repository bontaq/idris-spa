Count : Type
Count = Integer

connectedButton : State Count Count
connectedButton = do
  count <- get
  pure (count + 1)
