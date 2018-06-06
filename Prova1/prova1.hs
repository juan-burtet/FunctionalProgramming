-- 1)
paraCinza :: [Int] -> [Int] -> [Int]
paraCinza [] a  = []
paraCinza a  [] = []
paraCinza (x:xs) (y:ys)
  | x == y    = x : paraCinza xs ys
  | otherwise = 2 : paraCinza xs ys

-- 2)
applyIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyIf p f [] = []
applyIf p f (x:xs)
  | p x       = f x: applyIf p f xs
  | otherwise = x : applyIf p f xs

-- 3)
pegaEnquanto :: (a -> Bool) -> [a] -> [a]
pegaEnquanto p [] = []
pegaEnquanto p (x:xs)
  | p x       = x : pegaEnquanto p xs
  | otherwise = []

retiraEnquanto :: (a -> Bool) -> [a] -> [a]
retiraEnquanto p [] = []
retiraEnquanto p (x:xs)
  | p x       = retiraEnquanto p xs
  | otherwise = x:xs

-- 4)
cod2db :: [Codigo] -> Database -> Database
cod2db [] b     = []
cod2db (c:cs) b = pegaProduto c b : cod2db cs b

pegaProduto :: Codigo -> Database -> (Codigo, Produto, Preco)
pegaProduto x [] = error "Codigo não existe"
pegaProduto x ((a,b,c):xs)
  | x == a    = (a,b,c)
  | otherwise = pegaProduto x xs

total :: [Codigo] -> Database -> Int
total [] db     = 0
total (x:xs) db = preco x db + total xs db

preco :: Codigo -> Database -> Int
preco x []     = 0
preco x ((a,b,c):xs)
  | x == a    = c
  | otherwise = preco x xs