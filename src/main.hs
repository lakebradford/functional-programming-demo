-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = putStrLn (show x)

-- Write division here
division :: Double -> Double -> Maybe Double
division _ 0 = Nothing 
division x y = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1  -- Base case
factorial n = n * factorial (n - 1) 

-- Write factList here
factList :: Int -> [Int]
factList n = [factorial x | x <- [0..n]]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- main function to test the other functions
main :: IO ()
main = do
    printAMessage "Hello, world!"
    printAMessage 42
    printAMessage [1, 2, 3]

    printAMessage (division 10 2) 
    printAMessage (division 10 0) 

    printAMessage (factorial 5)  

    printAMessage (factList 5)  

    printAMessage (merge [1, 3, 5] [2, 4, 6])  
