mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f []    = []
mapInt f (a:x) = f a : mapInt f x

times2, times3 :: Int -> Int
times2 n = 2*n
times3 n = 3*n