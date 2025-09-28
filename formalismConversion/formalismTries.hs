data PTree a = PNode a [[PTree a]] deriving (Show, Eq)

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

-- This creates the first formalism, `howAddIntricated (howAdd 4)`

howAddIntricated :: [[Int]] -> [[PTree Int]]
howAddIntricated []= []
howAddIntricated (xs:xss) = 
    let outxs = map (\x -> if x == 1
                   then PNode 1 []
                   else PNode x ((howAddIntricated (howAdd x)))) xs
    in [outxs] ++ howAddIntricated xss

-- What i tried

howAddIntricatedConversion [] _ confset = confset
howAddIntricatedConversion (xs:xss) conf confset
    | all (== 1) xs = if null conf 
               then howAddIntricatedConversion ([]:xss) (replicate (length xs) (PNode 1 [])) confset
               else let [ptree] = conf
                        newptree = appendLastPTreen ptree (length xs)
                    in howAddIntricatedConversion ([]:xss) [newptree] confset
    | otherwise = if null conf 
                  then let (newx, ptree) = untilNotOne xs []
                       in map (\x2 -> concat $ (howAddIntricatedConversion [x2] (ptree 
                                                     ++ [PNode newx []]) []) ++
                       (howAddIntricatedConversion (xs:xss) conf confset)) (howAdd newx)
                  else let [ptree] = conf
                           (newx, ptree2) = untilNotOne xs []
                           newptree2 = if length ptree2 == 0
                                       then appendLastPTree ptree newx
                                       else let newptree = appendLastPTreen ptree (length ptree2)
                                            in appendLastPTree newptree newx
                       in map (\x2 -> concat $ (howAddIntricatedConversion [x2] [newptree2] []) ++
                       (howAddIntricatedConversion (xs:xss) [] confset)) (howAdd newx)



untilNotOne :: [Int] -> [PTree Int] -> (Int, [PTree Int])
untilNotOne (x:xs) outxs
    | x == 1 = untilNotOne xs (outxs ++ [PNode 1 []])
    | otherwise = (x, outxs)

appendLastPTree :: PTree Int -> Int -> PTree Int
appendLastPTree (PNode vl restvl) x = 
    let restvl2 = head restvl
    in if all (\(PNode x2 _) -> x2 == 1) restvl2
       then PNode vl (restvl ++ [[PNode x []]])
       else PNode vl [[appendLastPTree (head restvl2) x]]

appendLastPTreen :: PTree Int -> Int -> PTree Int
appendLastPTreen (PNode vl []) n = PNode vl [replicate n (PNode 1 [])]
appendLastPTreen (PNode vl restvl) n = PNode vl [[appendLastPTreen (head . head $ restvl) n]]

-- Example of what is wrong, but looks good at first glance

howAddIntricatedNotGood :: [Int] -> [[PTree Int]]
howAddIntricatedNotGood [] = []
howAddIntricatedNotGood (x:xs)
    | all (== 1) (x:xs) = [replicate (length xs + 1) (PNode 1 [])]
    | otherwise = map (\x2 -> [PNode x [concat $ (howAddIntricatedNotGood x2) 
                                          ++ (howAddIntricatedNotGood xs)]]) (howAdd x)






