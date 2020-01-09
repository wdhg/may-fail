import qualified MF

safeDiv :: Integral a => a -> a -> MF.MayFail a
safeDiv _ 0
  = MF.Fail "cannot divide by 0"
safeDiv x y
  = MF.Ok $ x `div` y

-- performs division over a list by folding the dividend
listDiv :: Integral a => [a] -> MF.MayFail a
listDiv (x : xs)
  = foldl (\x y -> x >>= (`safeDiv` y)) (MF.Ok x) xs
