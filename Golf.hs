skips xs = map (\n -> everyNth n xs) [1..length xs]

everyNth n xs
 | n == 1    = xs
 | otherwise = headFunc (drop (n - 1) xs)
         where headFunc [] = []
               headFunc (x:[]) = (x:[])
               headFunc ys = (head ys):(headFunc (drop (n) ys))


localMaxima (x:y:z:xs)
  | (y > x && y > z) = y:(localMaxima (z:xs))
  | otherwise          = localMaxima (z:xs)
localMaxima _ = []
