module Min where

minimumD :: Ord a => [a] -> a
minimumD l = minimumD2 (head l) (tail l)

minimumD2 :: Ord a => a -> [a] -> a
minimumD2 min [] = min
minimumD2 min (l:ls)  = if min > l 
                            then minimumD2 l ls 
                        else minimumD2 min ls 
