module Main where

--returns list--
getList:: (Int, Int, [Int]) -> [Int] 
getList (sum, i, list) = list

--sum of a provided list--
getListSum :: (Int, Int, [Int]) -> Int 
getListSum (sum, i, list) = sum

--i value--
startIndex :: (Int, Int, [Int]) -> Int
startIndex (sum, i, list) = i

--j value--
endIndex :: (Int, Int, [Int]) -> Int
endIndex (sum, i, list) = i + length list -1

--Start point for each sublist when creating sublists for the sorting--
subPrefix :: [Int] -> Int -> [(Int, Int, [Int])]
subPrefix [] _ = []
subPrefix xs i = (sum xs, i, xs) : subPrefix (init xs) i

--Creates a series of sublists, where header is the input Int--
subLists :: [Int] -> Int -> [(Int, Int, [Int])]
subLists [] _ = []
subLists xs i = subPrefix xs i ++ subLists (tail xs) (i+1)

--Returns sublists sorted by smallest to biggest. list from header i, where list is read from header to tail xs, where getListSum i is either <= sum of argument list or > sum of argument list--
sortSub :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
sortSub [] = []
sortSub (x:xs) =    let small = sortSub[i | i <- xs, getListSum i <= getListSum x]
                        big = sortSub [i | i <- xs, getListSum i > getListSum x]
                    in  small ++ [x] ++ big

-- takes a list and returns the k first sublists, since they are ordered from smallest to biggest sumwise, the smallest will be selected before the bigger ones--
smallestKsets :: [Int] -> Int -> [(Int, Int, [Int])]
smallestKsets xs k =    let sublist = subLists xs 1
                            sortedSubs = sortSub sublist
                        in take k sortedSubs

-- turns a list into a string--
listToString :: [Int] -> String
listToString [] = "\n"
listToString (x:xs) = show x ++ " " ++ listToString xs

-- format for the string printout --
stringFormat :: [(Int, Int, [Int])] -> String
stringFormat [] = ""
stringFormat (x:xs) = show (getListSum x) ++ " " ++ show(startIndex x) ++ " " ++ show(endIndex x) ++ " " ++ listToString (getList x) ++ stringFormat xs


-- header for clearer output of the program --
stringHeader :: String
stringHeader = "size i j sublist \n"

-- uses the tests provided in the lab instructions--
test1 :: [Int]
test1 = [x * (-1) ^x | x <- [1..100]]

test2 :: [Int]
test2 = [24,-11,-34,42,-24,7,-19,21]

test3 :: [Int]
test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]


--outputs the results--
main = do
    putStr(stringHeader ++ stringFormat (smallestKsets test1 15))
    putStr "\n \n"
    putStr(stringHeader ++ stringFormat (smallestKsets test2 6))
    putStr "\n \n"
    putStr(stringHeader ++ stringFormat (smallestKsets test3 8))