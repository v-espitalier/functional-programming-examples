
{-
**********************************************************************
*  https://github.com/v-espitalier/functional-programming-examples   *
*  Vincent Espitalier                                                *
*  Code under MIT license                                            *
*                                                                    *
*  Compilation and use:                                              *
*  $ ghc exemples.hs                                                 *
*  $ ./exemples                                                      *
*                                                                    *
*  Get script duration:                                              *
*  $ date +%s.%N  &&  ./exemples  &&  date +%s.%N                    *
**********************************************************************
-} 


----------------------------------------
-- Function Definitions

-- Examples for the factorial
factorial :: Integer -> Integer
factorial x
    | x <= 1 = 1
    | otherwise = x * factorial(x - 1)

-- Alternative implementations (factorial) 
factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 n = n * factorial (n - 1)

factorial3 :: Integer -> Integer
factorial3 n = product [1..n]

factorial4 :: Integer -> Integer
factorial4 n = go 1 n
  where
    go acc 1 = acc
    go acc k = go (acc * k) (k - 1)



-- Examples for Fibonacci
fibonacci :: Integer -> Integer
fibonacci n = fib_iter n 0 1
  where
    fib_iter 0 a b = a
    fib_iter k a b = fib_iter (k - 1) b (a + b)
    
-- Alternative implementation (Fibonacci)
fibonacci2 :: Integer -> Integer
fibonacci2 0 = 0
fibonacci2 1 = 1
fibonacci2 n = fibonacci (n - 1) + fibonacci (n - 2)


--Quick sort
onlySmaller i [] = []
onlySmaller i (x:xs)
  | x < i = x: (onlySmaller i xs)
  | otherwise = onlySmaller i xs

largerOrEqual i [] = []
largerOrEqual i (x:xs)
  | x >= i = x: (largerOrEqual i xs)
  | otherwise = largerOrEqual i xs


qsort [] = [] :: [Integer]
qsort (x:xs) = (qsort left) ++ [x] ++ (qsort right)
  where
    left = onlySmaller x xs
    right = largerOrEqual x xs
    

-- Bisection method for root finding (aka dichotomy)
bisection :: (Double -> Double) -> Double -> Double -> Int -> Double
bisection f a b n =
  let m = (a + b) / 2.0
  in if n <= 0
    then m
    else if f m > 0
      then bisection f a m (n - 1)
      else bisection f m b (n - 1)

-- Pi number computation by zeroing cosine
f_for_pi_bisection :: Double -> Double
f_for_pi_bisection x = -1.0 * cos (x / 2.0)  -- f_for_pi_bisection(Pi) = 0; increasing Function

pi_approx :: Int -> Double
pi_approx n = bisection f_for_pi_bisection 3.0 3.3 n


-- GCD computation
f_gcd :: Integral a => a -> a -> a
f_gcd a 0 = a
f_gcd a b = f_gcd b (mod a b)
  

-- Code for prime numbers
-- Compute int(sqrt(n))
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral
--isqrt n = floor (sqrt (fromIntegral n))

-- Primality test
is_not_div :: Integral a => a -> a -> Bool
is_not_div n d = (mod n d) /= 0

is_not_div_list :: Integral a => a -> [a] -> Bool
is_not_div_list n list = all (is_not_div n) list

is_prime :: Integral a => a -> Bool
is_prime n = n > 1 && is_not_div_list n [2..isqrt n]

-- Alternative implementation (Primality test)
is_prime2 :: Integral a => a -> Bool
is_prime2 n
  | n < 2     = False
  | otherwise = foldl (&&) True (map (is_not_div n) [2..isqrt n])


--Methods for prime number listing
--Method 1: With filter
list_test :: Integral a => a -> [a]
list_test n = [2] ++ [3,5..n]   -- 2 + Odd integers inside [3 n] (for primality test)

list_primes :: Integral a => a -> [a]
list_primes n = filter is_prime (list_test n)   -- Method 1: With filter (fastest)


-- Alternative implementations (Prime number listing)
-- Method 2: Sieve of Eratosthenes
list_primes2 :: Integral a => a -> [a]
list_primes2 n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


-- Method 3 with Foldl (slowest)
add_if_no_div :: Integral a => [a] -> a -> [a]
add_if_no_div list n =
  if is_not_div_list n (filter (\k -> k <= isqrt n) list)
    then [n] ++ list
    else list

list_primes3 :: Integral a => a -> [a]
list_primes3 n = reverse (foldl add_if_no_div [] (list_test n))  -- Méthode 3: foldl / slowest



-- Misc examples
f_square :: Num a => a -> a
f_square x = x * x

--Apply a function to each element of a list
haskell_map :: Num a => [a] -> [a]
haskell_map list = map f_square list

--Filter elements of a list following a criteria/predicat (function returning bool)
is_even :: Integer -> Bool
is_even x = (mod x 2) == 0
haskell_filter :: [Integer] -> [Integer]
haskell_filter list = filter is_even list

is_palindrome :: String -> Bool
is_palindrome s = s == reverse s
is_palindrome_list :: [String] -> [(String, Bool)]
is_palindrome_list list = map (\s -> (s, is_palindrome s)) list

----------------------------------------
-- main code

main :: IO ()
main = do
    putStrLn "Outputs:"
    putStrLn (show (factorial 5))  -- 1 * 2 * 3 * 4 * 5 = 120
    putStrLn (show (factorial2 5))
    putStrLn (show (factorial3 5))
    putStrLn (show (factorial4 5))
    
    putStrLn (show (map fibonacci [0..10]))  -- [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    putStrLn (show (map fibonacci2 [0..10]))
    
    putStrLn (show (qsort [1, 5, 3, 10, 11, 4, 8]))

    putStrLn (show (bisection (\x -> x * x - 2) 1 2 30))
    
    putStrLn (show (map pi_approx [1, 5, 10, 20, 30, 100]))

    putStrLn (show (f_gcd 15 18))
    putStrLn (show (f_gcd 18 15))
    putStrLn (show (f_gcd 11 13))
    putStrLn (show (f_gcd 2 12))
    putStrLn (show (f_gcd 16643 4403))  -- 16643 = 11 * 17 * 89  ,  4403 = 7 * 17 * 37
    
    putStrLn (show (map (\k -> (k, is_prime k)) [0..11]))
    putStrLn (show (map (\k -> (k, is_prime2 k)) [0..11]))
    
    putStrLn (show (list_primes 100))
    putStrLn (show (list_primes2 100))
    putStrLn (show (list_primes3 100))
    
    putStrLn (show (haskell_map [1..10]))
    putStrLn (show (haskell_filter [1..10]))
    
    putStrLn (show (is_palindrome_list ["abcd", "abccba", "abcba", "a", "aa", "", "abcdef"]))
    