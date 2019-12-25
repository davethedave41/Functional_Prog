module Ex17 where

taild :: [a] -> [a]
taild [] = error "Runtime error"
taild (x:xs) = xs

concatd :: [a] -> [a] -> [a]
concatd [] ys = ys
concatd (x:xs) ys = x:concatd xs ys 

initd :: [a] -> [a]
initd [x] = []
initd (x:xs) = x : init xs

lastd :: [a] -> a
lastd [x] = x
lastd (_:xs) = last xs


reversed :: [a] -> [a]
reversed [] = []
reversed xs = rev [] xs

rev sx [] = sx
rev sx (x:xs) = rev (x:sx) xs

{- breakd :: (a -> Bool) -> [a] -> ([a],[a])
breakd pred xs = breakdh pred xs ([],[]) 

breakdh :: (a -> Bool) -> [a] -> ([a],[a]) -> ([a],[a])
breakdh pred [] (xs,ys) = (xs,ys)
breakdh pred l (xs,ys) = if not(pred head xs)
                            then breakdh pred tail l (xs,head l:ys)  
                         else breakdh pred tail l (head l:xs, ys)
-}

adder :: a-> [a] -> [a]
adder x xs = (x:xs)

maximumd :: Ord a => [a] -> a 
maximumd xs = maximumdh xs head xs

maximumdh :: Ord a =>[a] -> a -> a 
maximumdh [] x = x 
maximumdh xs x = case (head xs > x) of
                  False -> maximumdh tail xs x
                  True  -> maximumdh tail xs head xs
