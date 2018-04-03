module Hanoi where

  -- towers of hanoi
type Peg = String
type Move = (Peg, Peg)


hanoi 1 from to aux = [(from, to)]
hanoi n from to aux =
  (hanoi (n - 1) from aux to) ++ [(from, to)] ++ (hanoi (n - 1) aux to from)
