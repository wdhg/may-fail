module MF where

data MayFail a
  = Ok a
  | Fail String
    deriving (Show)

instance Functor MayFail where
  fmap func (Ok value)
    = Ok $ func value
  fmap _ (Fail message)
    = Fail message

instance Applicative MayFail where
  pure result
    = Ok result
  left <*> right
    = do
      func <- left
      func <$> right

instance Monad MayFail where
  Fail message >>= func
    = Fail message
  Ok value >>= func
    = func value
