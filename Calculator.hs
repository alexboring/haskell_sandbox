data Expr =
  Add
  | Minus
  | Pending String
  | Solved Integer
  deriving (Show)

calculate =  foldl folder []  where
       folder xs '+'                                = (Add):xs
       folder xs '-'                                = (Minus):xs
       folder ((Pending y):(Solved x):(Add):xs) ' '   = (Solved ((read y :: Integer) + x)):xs
       folder ((Pending y):(Solved x):(Minus):xs) ' '   = (Solved ((read y :: Integer) - x)):xs
       folder ((Pending x):xs) ' '                  = (Solved (read x :: Integer)):xs
       folder xs ' '                                = xs
       folder ((Pending x):xs) num                  = (Pending (x ++ [num])):xs
       folder xs n                                  = (Pending [n]):xs

reduce  = foldl folder [] where
  folder xs (Pending x) = (Solved (read x :: Integer)):xs
  folder ((Solved x):(Solved y):xs) (Add) = (Solved (x + y)):xs
  folder ((Solved x):(Solved y):xs) (Minus) = (Solved (x - y)):xs
  folder xs x = x:xs

  solveRPN = head . foldl foldingFunction [] . words
      where  foldingFunction (x:y:ys) "+" = (x + y):ys
             foldingFunction (x:y:ys) "-" = (y - x):ys
             foldingFunction xs numberString = read numberString:xs


  evaluate xs = head $ calculate xs
  calculate ('+':xs) = [foldl (+) 0 $ (calculate xs)]
  calculate ('-':xs) = let (h:x:y:ys) = calculate xs in [ x - y]
  calculate (x:[]) = [(digitToInt x)]
  calculate  (x:xs) = (digitToInt x):(calculate xs)




  calculate xs = head (foldl compute [] (reverse xs))

  compute [] el = (read el :: Integer):[]
  compute (x:y:xs) el =
    if el == "+" then
      (x + y):xs
    else if el == "-" then
      (x - y):xs
    else
      (read el :: Integer):(x:y:xs)

  compute xs el = (read el :: Integer):xs


  solveRPN = head . foldl foldingFunction [] . words
       where  foldingFunction (x:y:ys) "+" = (x + y):ys
              foldingFunction (x:y:ys) "-" = (y - x):ys
              foldingFunction xs numberString = read numberString:xs

calculator xs = evalExpr xs xs

evalExpr ("+":xs) ("+":ys) = evalExpr ("+":xs) ys
evalExpr ("+":xs) (n:ys) = (read n :: Integer) + (evalExpr xs ys)
evalExpr n (x:[]) = (read x :: Integer)
