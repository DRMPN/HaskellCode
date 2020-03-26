import Control.Monad.State
import Control.Monad.Writer

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

-- This explains a lot
{-

marySue :: Maybe Bool
marySue = do
x <- Just 9
Just (x>8)


Just 9 >>= \x -> (Just (x>8))

-}

{-
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

type KnigthPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
-}

applyLog :: (a,String) -> (a->(b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

bigSmalls :: Int -> Int -> Maybe Int
bigSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

-- use foldM bigSmalls 0 [1,2,3,4,5]
-- will fail with foldl
