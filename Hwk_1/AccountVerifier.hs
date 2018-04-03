module AccounterVerifier where

  parse (x:y:xs) "+" = (x + y):ys
  parse (x:y:xs) "-" = (x - y):ys
  parse xs numString = read numString:xs


  -- Credit card validator

  digits num = digit num []

  digit num digits | num <= 10 = num:digits
  digit num digits = digit (div num 10) ((mod num 10):digits)

  doubleEveryOther xs = snd (foldr doubler (True, []) xs)

  doubler x (False, digits) = (True, (x * 2):digits)
  doubler x (True, digits) = (False, x:digits)

  sumDigits xs = sum (map (sum. digits) xs)

  validates num = (mod (sumDigits (doubleEveryOther (digits num))) 10) == 0
