module Arrow where
import Control.Category hiding (id, (.))

class Category a => Arrow a where
  arr    :: (b -> c) -> a b c
  first  :: a b c -> a (b,d) (c,d)
  second :: a b c -> a (d,b) (d,c)
  (***)  :: a b c -> a b' c' -> a (b,b') (c,c') -- both
  (&&&)  :: a b c -> a b c' -> a b (c,c') -- fanout

instance Arrow (->) where
  arr f            = f
  (***) f g ~(x,y) = (f x, g y)
  first            = (*** id)
  second           = (id ***)
  f &&& g          = arr (\b -> (b,b)) >>> f *** g

class Arrow a => ArrowChoice a where
  left  :: a b c -> a (Either b d) (Either c d)
  right :: a b c -> a (Either d b) (Either d c)
  (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c') -- split
  (|||) :: a b d -> a c d -> a (Either b c) d

instance ArrowChoice(->) where
  left f  = f +++ id
  right f = id +++ f
  f +++ g = (Left . f) ||| (Right . g)
  (|||)   = either

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr whatCase >>>
         arr (const [])
         |||
         (f *** mapA f >>> arr (uncurry (:)))
         where
           whatCase []     = Left ()
           whatCase (x:xs) = Right (x,xs)

factorialA :: ArrowChoice arr => arr Integer Integer
factorialA = arr whatCase >>>
             arr (const 1)
             |||
             (second factorialA >>> arr (uncurry (*)))
             where
               whatCase 0 = Left ()
               whatCase x = Right (x, (x-1))

fibA :: ArrowChoice arr => arr Integer Integer
fibA = arr whatCase >>>
       arr id
       |||
       (fibA *** fibA >>> arr (uncurry (+)))
       where
         whatCase 0 = Left 0
         whatCase 1 = Left 1
         whatCase x = Right (x-1, x-2)

filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA p = arr listCase >>>
            arr (const [])
            |||
            ((p &&& arr id) *** filterA p >>> arr boolJoin)
            where
              listCase [] = Left ()
              listCase (x:xs) = Right (x,xs)
              boolJoin ((True, x), xs) = x:xs
              boolJoin ((False, _), xs) = xs
