import Control.Applicative

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

-- type (just synonym) - make signatures look cleaner
-- newtype (create existing type and wrap them in new type) - make instance of a type class
-- data (create your own type, completely new and odd)
