doubleme x = x+x
doubleus x y = doubleme x + doubleme y
doubleifsmall x = if x<100 then x*2 else x

myname = "Toni"

doublelist :: [Int] -> [Int]
doublelist xs = [2*x | x<-xs]

factorial :: Integer -> Integer
factorial n = product [1..n]

factorialrec :: Integer -> Integer
factorialrec 0 = 1
factorialrec n = n * factorial (n-1)

fibonaccirec :: Integer -> Integer
fibonaccirec 1 = 1
fibonaccirec 2 = 1
fibonaccirec n = fibonaccirec (n-1) + fibonaccirec (n-2)

isseven :: Integer -> [Char]
isseven 7 = "Yes seven!"
isseven x = "Not seven!"

addvector :: (Num a) => (a,a) -> (a,a) -> (a,a)
addvector (a,b) (c,d) = (a+c,b+d)

headmine :: [a] -> a
headmine [] = error "empty list!"
headmine (x:[]) = x
headmine (x:_) = x

magnitude :: Integer ->  Integer
magnitude x
	| x <= 10 = 1
	| x <= 100 = 10
magnitude x
	| x <= 1000 = 100
	| x <= 10000 = 1000
	| otherwise = -1
	
initials :: [Char] -> [Char] -> [Char]
initials a b = [f]++"."++[s]++"."
	where f = head a
	      s = head b


whatis :: Integer -> [Char]
whatis x = "It is " ++ what x
	where what 0 = "zero"
	      what 1 = "one"
	      what x = "i don't know"

whatis' :: Integer -> [Char]
whatis' x = "It is " ++ case x of 3 -> "three"
                                  4 -> "four"
				  otherwise -> "i don't know"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' (x:[]) = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Integer -> b -> [b]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n-1) x

take' :: Integer -> [a] -> [a]
take' 0 _ = []
take' 1 (x:_) = [x]
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
reverse'' = foldl (\acc x -> x:acc) []
reverse''' = foldl (flip (:)) []

repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) 
     | x==y = True
     | otherwise = elem' x ys 
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldl (\acc e -> acc || e==x) False

quick' :: (Ord a) => [a] -> [a]
quick' [] = []
quick' (x:xs) = 
    let s = filter (<=x) xs
        g = filter (>x) xs
    in quick' s ++ [x] ++ quick' g

compare100 = compare 100

divideTenBy :: (Floating a) => a -> a
divideTenBy = (/) 10

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) 

isTen :: Int -> Bool
isTen = (`elem` [1..10])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = \x y -> f y x

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x  
     | odd x = let r=x*3+1 in  x:collatz r
     | even x = let r=x `div` 2 in  x:collatz r  

multi' =  foldl (\acc x -> acc*x) 1
multi'' =  foldl (*) 1

map' f = foldl (\acc x -> acc ++ [f x]) []
map'' f = foldr (\x acc -> f x : acc) []

