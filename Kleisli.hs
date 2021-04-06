{-#LANGUAGE InstanceSigs#-}
module Kleisli where
import Control.Category
import Control.Arrow (Arrow(..),ArrowChoice(..))

newtype Kleisli m a b = Kleisli {
  runKleisli :: a -> m b}

instance Monad m => Category (Kleisli m) where
  id      = Kleisli $ return
  k1 . k0 = Kleisli $ \a -> runKleisli k0 a >>=
                      \b -> runKleisli k1 b

instance Monad m => Arrow (Kleisli m) where
  arr f = Kleisli $ return Prelude.. f
  first m = Kleisli $ \ ~(x,y) -> runKleisli m x >>=
                            \r -> return (r,y)

instance Monad m => ArrowChoice (Kleisli m) where
  left :: Kleisli m b c -> Kleisli m (Either b d) (Either c d)
  left m = Kleisli $ whichCase
    where
      whichCase (Left b)  = Left <$> runKleisli m b
      whichCase (Right d) = return $ Right d

example :: Kleisli IO a ()
example = (Kleisli (const getLine)  >>>
           arr length >>>
           Kleisli print)
