instance Monad Parser where
  (>>=) = bindParser
  return = value