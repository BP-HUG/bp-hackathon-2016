fix :: (a -> a) -> a
fix f = f (fix f) -- let x = f x in x

factorial_proto :: (Integer -> Integer) -> Integer -> Integer
factorial_proto self n = if n == 0 then 1 else n * self (n-1)

factorial = fix factorial_proto

main = print $ factorial 5