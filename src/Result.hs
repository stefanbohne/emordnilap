module Result where

import Control.Monad.State.Strict

data Result a = 
        Success a
    |   Rejected String
    |   Error String
    |   TypeError String
    deriving (Functor, Show, Eq)
instance Applicative Result where
  pure = Success
  (Success f) <*> (Success a) = Success $ f a
  _ <*> TypeError msg = TypeError msg
  TypeError msg <*> _ = TypeError msg
  _ <*> Error msg = Error msg
  Error msg <*> _ = Error msg
  _ <*> Rejected msg = Rejected msg
  Rejected msg <*> _ = Rejected msg
instance Monad Result where
  (Success a) >>= f = f a
  TypeError msg >>= _ = TypeError msg
  Error msg >>= _ = Error msg
  Rejected msg >>= _ = Rejected msg
_ >>= Rejected msg = Rejected msg
  
errorHint :: String -> Result a -> Result a
errorHint hint (Success v) = Success v
errorHint hint (TypeError msg) = TypeError $ msg ++ "\n" ++ hint
errorHint hint (Error msg) = Error $ msg ++ "\n" ++ hint
errorHint hint (Rejected msg) = Rejected $ msg ++ "\n" ++ hint
