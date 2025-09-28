module Helpers where

-- helper functions

unique :: (Eq a) => [a] -> [a]
unique xs = subUnique xs []

subUnique :: (Eq a) => [a] -> [a] -> [a]
subUnique [] xs2 = xs2
subUnique (x:xs) xs2
    | x `elem` xs2 = subUnique xs xs2
    | otherwise = subUnique xs (x:xs2)

permu :: [a] -> [[a]]
permu xs = subPermu [xs] 0 (length xs - 1)

subPermu :: [[a]] -> Int -> Int -> [[a]]
subPermu xs n cmp
    | n < cmp = subPermu (concat $ map (\x -> permutationAt x n) xs) (n + 1) cmp
    | otherwise = xs

permutationAt :: [a] -> Int -> [[a]]
permutationAt xs n = subPermutationAt xs n n

subPermutationAt :: [a] -> Int -> Int -> [[a]]
subPermutationAt xs n nb
    | n < l = (subPermu3 xs n nb (xs !! nb) (xs !! n) 0):subPermutationAt xs (n + 1) nb
    | otherwise = []
    where l = length xs

subPermu3 :: [a] -> Int -> Int -> a -> a -> Int -> [a]
subPermu3 [] _ _ _ _ _ = []
subPermu3 (x:xs) n nb val val2 n2
    | n2 == nb   = val2:subPermu3 xs n nb val val2 (n2 + 1)
    | n2 /= n   = x:subPermu3 xs n nb val val2 (n2 + 1)
    | otherwise = val:subPermu3 xs n nb val val2 (n2 + 1)

howAdd :: Int -> [[Int]]
howAdd cmp = concat $ subHowAdd cmp 1

subHowAdd :: Int -> Int -> [[[Int]]]
subHowAdd cmp n
    | n < cmp = ((subHowAdd2 cmp n n [n]):subHowAdd cmp (n + 1))
    | otherwise = []

subHowAdd2 :: Int -> Int -> Int -> [Int] -> [[Int]]
subHowAdd2 _ 0 _ outxs = []
subHowAdd2 cmp n n2 outxs
    | n2 < cmp =  subHowAdd2 cmp n (n2 + n) (n:outxs) ++ 
                  subHowAdd2 cmp (n - 1) n2 outxs
    | n2 > cmp = subHowAdd2 cmp (n - 1) n2 outxs
    | otherwise = unique . permu $ outxs

getRangeList :: [a] -> [Int] -> [a]
getRangeList [] _ = []
getRangeList _ [] = []
getRangeList xs (idx:ids) = (xs !! idx):(getRangeList xs ids) 


deleteListElem :: [a] -> Int -> [a]
deleteListElem xs n = subDeleteListElem xs n 0

deleteListElemn :: [a] -> [Int] -> [a]
deleteListElemn xs nxs = subDeleteListElemn xs nxs 0

subDeleteListElemn :: [a] -> [Int] -> Int -> [a]
subDeleteListElemn xs [] _ = xs
subDeleteListElemn xs (cmp:cmpxs) n = 
    let newxs = subDeleteListElem xs (cmp - n) 0
    in subDeleteListElemn newxs cmpxs (n + 1)


subDeleteListElem :: [a] -> Int -> Int -> [a]
subDeleteListElem [] _ _ = []
subDeleteListElem (x:xs) cmp n
    | n /= cmp = x:(subDeleteListElem xs cmp (n + 1))
    | otherwise = subDeleteListElem xs cmp (n + 1)

updateListElem :: [a] -> Int -> a -> [a]
updateListElem xs ncmp val = subUpdateListElem xs 0 ncmp val

subUpdateListElem :: [a] -> Int -> Int -> a -> [a]
subUpdateListElem [] _ _ _ = []
subUpdateListElem (x:xs) n ncmp val
    | n /= ncmp = x:(subUpdateListElem xs (n + 1) ncmp val)
    | otherwise = val:(subUpdateListElem xs (n + 1) ncmp val)


grepmn2 :: (Eq a) => [a] -> [a] -> [Int]
grepmn2 [] _ = []
grepmn2 (x2:xs2) xs = (grepn2 x2 xs) ++ (grepmn2 xs2 xs)

grep2 :: (Eq a) => a -> [a] -> Int
grep2 cmp xs = subGrep2 xs cmp 0

subGrep2 :: (Eq a) => [a] -> a -> Int -> Int
subGrep2 [] _ _ = -1
subGrep2 (x:xs) cmp n
    | cmp == x = n
    | otherwise = subGrep2 xs cmp (n + 1)

myMax :: (Ord a) => [a] -> a
myMax xs = subMyMax xs (head xs)

subMyMax :: (Ord a) => [a] -> a -> a
subMyMax [] cmp = cmp
subMyMax (x:xs) cmp = 
    let cmp2 = if cmp >= x
              then cmp
              else x
    in subMyMax xs cmp2

tailn :: Int -> [a] -> [a]
tailn n xs 
    | n == 0 = xs
    | otherwise = subTailn n xs 1

subTailn :: Int -> [a] -> Int -> [a]
subTailn cmp (_:xs) n
    | n < cmp = subTailn cmp xs (n + 1)
    | otherwise = xs

grepn2 :: (Eq a) => a -> [a] -> [Int]
grepn2 cmp xs = subGrepn2 xs cmp 0 []

subGrepn2 :: (Eq a) => [a] -> a -> Int -> [Int] -> [Int]
subGrepn2 [] _ _ nxs = nxs
subGrepn2 (x:xs) cmp n nxs
    | cmp == x  = subGrepn2 xs cmp (n + 1) (n:nxs)
    | otherwise = subGrepn2 xs cmp (n + 1) nxs


calc :: [Char] -> [Char]
calc xs = 
    let (ids, nums) = parserPar xs
        newxs = subCalc xs ids nums
    in protoCalc newxs
    
subCalc :: [Char] -> [Int] -> [Int] -> [Char]
subCalc xs [] [] = xs
subCalc xs ids nums = 
    let curmax = myMax nums
        [id1, id2] = grepn2 curmax nums
        idstrt = (ids !! id2)
        idstop = (ids !! id1)
        xsstrt = if idstrt > 0
                 then getRangeList xs [0..(idstrt - 1)]
                 else []
        xsstop = if idstop + 1 < length xs
                 then getRangeList xs [(idstop + 1)..(length xs - 1)]
                 else []
        xsbetween = getRangeList xs [(idstrt + 1)..(idstop - 1)]
        rslt = protoCalc xsbetween
        newxs = if head rslt /= '-'
                then xsstrt ++ rslt ++ xsstop
                else let newxsstrt = switchLast xsstrt
                     in if newxsstrt /= xsstrt
                        then newxsstrt ++ tail rslt ++ xsstop
                        else xsstrt ++ rslt ++ xsstop
        (newids, newnums) = parserPar newxs
    in subCalc newxs newids newnums

switchLast :: [Char] -> [Char]
switchLast xs = reverse $ subSwitchLast (reverse xs) False 

subSwitchLast :: [Char] -> Bool -> [Char]
subSwitchLast [] _ = []
subSwitchLast (x:xs) alrd
    | x == '-' && not alrd = ('+':subSwitchLast xs True)
    | x == '+' && not alrd = ('-':subSwitchLast xs True)
    | otherwise = (x:subSwitchLast xs False)

lastIsMinus :: [Char] -> Bool
lastIsMinus xs = subLastIsMinus . reverse $ xs

subLastIsMinus :: [Char] -> Bool
subLastIsMinus [] = False
subLastIsMinus (x:xs)
    | x == '+' = False
    | x == '-' = True
    | otherwise = subLastIsMinus xs

lastIsPlus :: [Char] -> Bool
lastIsPlus xs = subLastIsPlus . reverse $ xs

subLastIsPlus :: [Char] -> Bool
subLastIsPlus [] = False
subLastIsPlus (x:xs)
    | x == '+' = True
    | x == '-' = False
    | otherwise = subLastIsPlus xs

protoCalc :: [Char] -> [Char]
protoCalc xs = 
    let outxs = subProtoCalc2 (subProtoCalc xs []) [] 0
    in outxs

takeBack :: [Char] -> [Char]
takeBack [] = []
takeBack (x:xs) 
    | not (x `elem` "+-*/") = (x:takeBack xs)
    | otherwise = []

takeTailN :: [Char] -> [Char]
takeTailN [] = []
takeTailN (x:xs)
    | not (x `elem` "+-*/") = takeTailN xs
    | otherwise = x:xs

subProtoCalc :: [Char] -> [Char] -> [Char]
subProtoCalc [] outxs = outxs
subProtoCalc (x:xs) outxs
    | x == '*' = 
        if head xs /= '-'
        then let val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xs
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xs
             in subProtoCalc newxs (newoutxs ++ (show (val1 * val2)))
        else let xsb = tail xs
                 val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xsb
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xsb
             in  if null newoutxs
                 then subProtoCalc newxs (newoutxs ++ (show (-val1 * val2)))
                 else if last newoutxs /= '-'
                     then subProtoCalc newxs (init newoutxs ++ (show (-val1 * val2)))
                     else subProtoCalc newxs (init newoutxs ++ "+" ++ (show (val1 * val2)))
    | x == '/' = 
        if head xs /= '-'
        then let val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xs
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xs
             in subProtoCalc newxs (newoutxs ++ (show (val1 `div` val2)))
        else let xsb = tail xs
                 val1 = read . reverse . takeBack . reverse $ outxs
                 val2 = read . takeBack $ xsb
                 newoutxs = reverse . takeTailN . reverse $ outxs
                 newxs = takeTailN xsb
             in if null newoutxs
                then subProtoCalc newxs (newoutxs ++ (show (-val1 `div` val2)))
                else if last newoutxs /= '-'
                    then subProtoCalc newxs (init newoutxs ++ (show (-val1 `div` val2)))
                    else subProtoCalc newxs (init newoutxs ++ "+" ++ (show (val1 `div` val2)))
    | otherwise = subProtoCalc xs (outxs ++ [x])

clearMinus :: [Char] -> [Char]
clearMinus xs = subClearMinus xs 0

subClearMinus :: [Char] -> Int -> [Char]
subClearMinus (x:xs) n
    | x /= '-' = if n `mod` 2 /= 0
                 then  '-':x:xs
                 else x:xs
    | otherwise = subClearMinus xs (n + 1)

subProtoCalc2 :: [Char] -> [Char] -> Int -> [Char]
subProtoCalc2 [] outxs _ = outxs
subProtoCalc2 (x:xs) outxs n
    | x == '+' = 
        let val1 = read . reverse . takeBack . reverse $ outxs
            val2 = read . takeBack $ xs
            newoutxs = reverse . takeTailN . reverse $ outxs
            newxs = takeTailN xs
        in if null newoutxs 
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
           else if last newoutxs /= '-'
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
           else if val1 > val2
                then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
                else subProtoCalc2 newxs (init newoutxs ++ (show (val2 - val1))) (n + 1)
    | x == '-' && n /= 0 = 
        let val1 = read . reverse . takeBack . reverse $ outxs
            val2 = read . takeBack $ xs
            newoutxs = reverse . takeTailN . reverse $ outxs
            newxs = takeTailN xs
        in if null newoutxs 
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
           else if last newoutxs /= '-'
           then subProtoCalc2 newxs (newoutxs ++ (show (val1 - val2))) (n + 1)
           else subProtoCalc2 newxs (newoutxs ++ (show (val1 + val2))) (n + 1)
    | otherwise = subProtoCalc2 xs (outxs ++ [x]) (n + 1)


parserPar :: [Char] -> ([Int], [Int])
parserPar xs = subParserPar xs [] [] [] 0 0

subParserPar :: [Char] -> [Int] -> [Int] -> [Int] -> Int -> Int
                -> ([Int], [Int])
subParserPar [] ids nums _ _ _ = (ids, nums)
subParserPar (x:xs) ids nums valxs n n2
    | x == '(' = 
        let newids = ids ++ [n]
            newnums = nums ++ [n2]
            newvalxs = map (\x -> x + 1) valxs
            newvalxs2 = newvalxs ++ [1]
        in subParserPar xs newids newnums newvalxs2 (n + 1) (n2 + 1)
    | x == ')' = 
        let newvalxs = map (\x -> x - 1) valxs 
            idx = findFirstZero (reverse newvalxs) 0
            idx2 = (length valxs) - idx - 1
            newids = ids ++ [n]
            newnums = nums ++ [(nums !! idx2)]
        in subParserPar xs newids newnums (newvalxs ++ [0]) (n + 1) n2
    | otherwise = subParserPar xs ids nums valxs (n + 1) n2

findFirstZero :: [Int] -> Int -> Int
findFirstZero (xi:xsi) n
              | xi == 0 = n
              | otherwise = findFirstZero xsi (n + 1)


