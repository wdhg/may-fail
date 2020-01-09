data MayFail a
  = Ok a
  | Fail String
    deriving (Show)

instance Functor MayFail where
  fmap func (Ok result)
    = Ok $ func result
  fmap _ (Fail message)
    = Fail message

instance Applicative MayFail where
  pure result
    = Ok result
  Fail message1 <*> Fail message2
    = Fail $ message1 ++ " and " ++ message2
  Fail message <*> _
    = Fail message
  _ <*> Fail message
    = Fail message
  Ok func <*> Ok value
    = Ok $ func value

instance Monad MayFail where
  Fail message >>= func
    = Fail message
  Ok value >>= func
    = case result of
        ok@(Ok _) -> ok
        fail      -> fail
      where
        result = func value

safeDiv :: Integral a => a -> a -> MayFail a
safeDiv _ 0
  = Fail "cannot divide by 0"
safeDiv x y
  = Ok $ x `div` y
