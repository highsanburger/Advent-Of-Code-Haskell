import Data.List
ns = [0..9]
nw = ["zero","one","two","three","four","five","six","seven","eight","nine"]
ps = zip ns nw

elemAdd :: Num a => [Maybe a] -> [Maybe a] -> [Maybe a]
elemAdd [] [] = []
elemAdd (Just a:as) (Nothing:bs) = (Just a: elemAdd as bs)
elemAdd (Nothing:as) (Just b:bs) = (Just b: elemAdd as bs)
elemAdd (Nothing:as) (Nothing:bs) = (Nothing: elemAdd as bs)

adds :: Num a => String -> [Maybe a]
adds a = ins0 a `elemAdd` ins1 a `elemAdd` ins2 a `elemAdd` ins3 a `elemAdd` ins4 a `elemAdd` ins5 a `elemAdd` ins6 a `elemAdd` ins7 a `elemAdd` ins8 a `elemAdd` ins9 a

collapse :: [Maybe Int] -> [Int]
collapse [] = []
collapse ((Just n):ns) = (n: collapse ns)
collapse (Nothing:ns) = ( collapse ns)

fun :: String -> Int 
fun s = (head ns) * 10 + last ns
        where ns = collapse $ adds s 
substring :: String -> [String]
substring s = tail . inits =<< tails s

ins0 a = map (\x -> if x then Just 0 else Nothing ) $ map (\x -> (x == "zero") || (x == "0")) $ substring a
ins1 a = map (\x -> if x then Just 1 else Nothing ) $ map (\x -> (x == "one") || (x == "1")) $ substring a
ins2 a = map (\x -> if x then Just 2 else Nothing ) $ map (\x -> (x == "two") || (x == "2")) $ substring a
ins3 a = map (\x -> if x then Just 3 else Nothing ) $ map (\x -> (x == "three") || (x == "3")) $ substring a
ins4 a = map (\x -> if x then Just 4 else Nothing ) $ map (\x -> (x == "four") || (x == "4")) $ substring a
ins5 a = map (\x -> if x then Just 5 else Nothing ) $ map (\x -> (x == "five") || (x == "5")) $ substring a
ins6 a = map (\x -> if x then Just 6 else Nothing ) $ map (\x -> (x == "six") || (x == "6")) $ substring a
ins7 a = map (\x -> if x then Just 7 else Nothing ) $ map (\x -> (x == "seven") || (x == "7")) $ substring a
ins8 a = map (\x -> if x then Just 8 else Nothing ) $ map (\x -> (x == "eight") || (x == "8")) $ substring a
ins9 a = map (\x -> if x then Just 9 else Nothing ) $ map (\x -> (x == "nine") || (x == "9")) $ substring a



a = "two1nine"
b = "eighttwothree"
main = do
    input <- readFile "01-input.txt"
    -- input <- readFile "example1.txt"
    -- input <- readFile "example2.txt"
    -- print $ map fun $ lines $ input
    print $ sum $ map fun $ lines $ input
