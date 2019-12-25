module Practice2 where
   
import Data.List ((\\))

lastm :: Monad m => [a] -> m a
lastm [] = fail "Empty List"
lastm (x:xs) = do 
                if lengthD xs == 0 
                    then return x
                else do lastm xs

lengthD :: [a] -> Integer
lengthD [] = 0
lengthD (x:t) = 1 + lengthD t

concatD :: [a] -> [a] -> [a]
concatD [] ys = ys
concatD xs [] = xs
concatD (x:xs) ys = x:concatD xs ys   

reverseD :: [a] -> [a]
reverseD [] = []
reverseD ds = let i = (lengthD ds) - 1 
                  index = fromIntegral i      
              in reverseD2 ds index 

reverseD2 :: [a] -> Int -> [a]
reverseD2 _  (- 1) = []
reverseD2 ds ind = let x = (!!) ds ind  
                       y = ind - 1
                 in x : reverseD2 ds y

{- spanD:: Monad m => (a -> Bool) -> [a] ->m ([a],[a])
spanD p ds = spanD2 p ds ([],[])

spanD2 :: Monad m => (a -> Bool) -> [a] -> ([a],[a]) ->m ([a],[a])
spanD2 p ds (xs,ys) = do l <- length ds
                         if l == 0
                            then return (xs,ys)
                         else do hed <- head ds
                                 pred <- p hed
                                 tl <- tail ds
                                 if pred == True 
                                    then spanD2 p tl (hed:xs,ys)
                                 else spanD2 p tl (xs,hed:ys) 
-}

minimumD :: Ord a => [a] -> a
minimumD l = minimumD2 head l tail l

minimumD2 :: Ord a => [a] -> a
minimumD2 min [] = min
minimumD2 min l  = if min > head l 
                     then minimumD2 head l tail l 

{- mapD :: (a -> b) -> [a] -> [b]
mapD func [] = []
mapD func (x:xs) = func x:mapD func xs 
-}

data BinTree 
   = BNil
   | BOne Int String
   | BTwo BinTree Int String BinTree
   deriving (Eq, Show)

lookup :: BinTree -> Int -> String 
lookup (BOne i s) x 
 | x == i = s
lookup (BTwo left i s right) x
 | x < i = lookup left x
 | x > i = lookup right x 