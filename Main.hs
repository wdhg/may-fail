data MayFail a
  = Ok a
  | Fail String
    deriving (Show)

instance Functor MayFail where
  fmap func (Ok result)
    = Ok $ func result
  fmap _ (Fail message)
    = Fail message
