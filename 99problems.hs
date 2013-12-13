myLast1 :: [a] -> a
myLast1 [x] = x
myLast1 (_:xs) = myLast1 xs 

myLast1' = head . reverse

myLast'' :: [a] -> a
myLast'' [] = error "empty list"
myLast'' x = x !! (length x -1)

myButLast2 :: [a] -> a
myButLast2 = last . init

myButLasti'2 x = reverse x !! 1

myButLast2'' [x,_] = x
myButLast2'' (x:xs) = myButLast2'' xs

elementAt3 :: [a] -> Int -> a
elementAt3 (x:_) 1 = x
elementAt3 [] _ = error "index out of bounds"
elementAt3 (x:xs) n
    | n > 1       = elementAt3 xs (n-1)
    | otherwise   = error "index out of bounds"

myLength4 :: [a] -> Int
myLength4 [] = 0
myLength4 (x:xs) = 1 + myLength4 xs

myLength4' :: [a] -> Int
myLength4' = foldl (\acc x -> acc+1) 0 

myLength4'' :: [a] -> Int
myLength4'' = sum . map (\_ -> 1)

--myLength4''' :: [a] -> Int
--myLength4''' = foldl ((+1) . const) 0

myLength4'''' :: [a] -> Int
myLength4'''' = fst . last . zip [1..]

myReverse5 :: [a] -> [a]
myReverse5 = foldl (\acc x -> x:acc) []

myReverse5' :: [a] -> [a]
myReverse5' = foldl (flip (:)) []

myReverse5'' :: [a] -> [a]
myReverse5'' = foldr (\x acc-> acc ++ [x]) []

isPalindrome7 :: (Eq a) => [a] -> Bool
--isPalindrome7 [] = True
isPalindrome7 x = x == reverse x

isPalindrome7' :: (Eq a) => [a] -> Bool
isPalindrome7' [] = True
isPalindrome7' [_] = True
isPalindrome7' x = head x == last x && (isPalindrome7' $ init $ tail x)

isPalindrome7'' :: (Eq a) => [a] -> Bool
isPalindrome7'' xs = all (\(x,y) -> x==y) $ zip xs $ reverse xs

compress' [] = []
compress' xs = foldr (\x acc -> if head acc /= x then x:acc else acc) [last xs] xs

compress'' [] = []
compress'' (x:xs) = x : (compress'' $ dropWhile (==x) xs)

compress''' (x:xs@(y:_))
    |x==y = compress''' xs
    |otherwise = x : compress''' xs
compress''' ys = ys --1 element list  case

compress'''' :: (Eq a) => [a] -> [a]
compress'''' = foldl app []
    where app [] x = [x]
          app acc x
	      | x == last acc = acc
	      | otherwise = acc ++ [x]
