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
