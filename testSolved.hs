main = do
    zad_4


zad_1_a :: (Integral a) => a -> a
zad_1_a 1 = 1
zad_1_a n
    | n `mod` 2 == 0 = 0
    | True = 2 * zad_1_a (n - 2) 

zad_1_b n = [zad_1_a i | i <- take n [1 ..] ]


zad_2_a ::[Int] -> [Int]
zad_2_a xs = [x | x <- xs, x `mod` 5 == 0]

zad_2_b ::[Int] -> [Int]
zad_2_b [] = []
zad_2_b (x:xs) = if (x `mod` 5 == 0) then x:zad_2_b xs else zad_2_b xs 

zad_3 :: [[a]] -> [a]
zad_3 [] = []
zad_3 (x:xs) = if (length x == 0) then zad_3 xs else (last x):zad_3 xs

zad_4 :: IO ()
zad_4 = do
    putStrLn "Write the first number:"
    first <- getLine
    putStrLn "Write the second number:"
    second <- getLine
    putStrLn "Write the third number:"
    third <- getLine
    let maxNum = max (read third) $ max (read first) (read second) :: Float
    putStrLn ("maximum: " ++ (show maxNum))

--Credits Adam
