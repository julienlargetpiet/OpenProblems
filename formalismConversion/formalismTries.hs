import Helpers

data PTree a = PNode a [[PTree a]] deriving (Show, Eq)

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






