import Helpers
import Data.List (find)

data PTree a = PNode a [[PTree a]] deriving (Show, Eq)

examplePTree :: PTree Int
examplePTree = PNode 4 [[PNode 1 [],PNode 1 [],PNode 1 [], PNode 1 []],
              [PNode 2 [[PNode 1 [], PNode 1 []]],PNode 1 [], PNode 1 []],
              [PNode 1 [],PNode 2 [[PNode 1 [],PNode 1 []]], PNode 1 []],
              [PNode 1 [], PNode 1 [], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 2 [[PNode 1 [], PNode 1 []]], PNode 2 [[PNode 1 [], PNode 1 []]]],
              [PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 [], PNode 1 [], PNode 1 []]], 
                        PNode 1 []],
              [PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]], 
                        PNode 1 []],
              [PNode 1 [], 
                  PNode 3 [[PNode 2 [[PNode 1 [], PNode 1 []]]], [PNode 1 []]]],
              [PNode 1 [], 
                  PNode 3 [[PNode 1 []], [PNode 2 [[PNode 1 [], PNode 1 []]]]]]]

subDividing :: [Int] -> [Int] -> Int -> [[Int]]
subDividing _ [] _ = []
subDividing ids (n:ns) n2 = (getRangeList ids (map (\x -> x + n2) [0..(n - 1)])):subDividing ids ns (n2 + n)

calculatePTree :: PTree Int -> [[Char]] -> [Int] -> [[Char]] -> [([[Char]], [[Char]])]
calculatePTree (PNode x ptrees) xs ns n = 
    let outids = subDividing ns (map (\vl -> sum (map (\(PNode vl2 _) -> vl2) vl)) ptrees) 0
    in concatMap (
                \(restxs, curids) ->
                  if all (\(PNode vl _) -> vl == 1) restxs
                  then [((getRangeList xs curids), n)]
                  else
                        let outids2 = subDividing curids (map (\(PNode vl _) -> vl) restxs) 0
                        in concatMap (\(ptree, val) -> 
                calculatePTree ptree xs val (n ++ [(foldl (\acc valb -> acc ++ show valb) "" val)])) (zip restxs outids2)
                ) (zip ptrees outids)

createFormula :: [[Char]] -> [Char] -> [Char]
createFormula (num:nums) ops = subCreateFormula nums ops num

subCreateFormula :: [[Char]] -> [Char] -> [Char] -> [Char]
subCreateFormula [] _ outxs = outxs
subCreateFormula _ [] outxs = outxs
subCreateFormula (num:nums) (op:ops) outxs = subCreateFormula nums ops (outxs ++ [op] ++ num)

updateOperators :: [([Char], [[Char]])] -> [Char] -> [Char] -> [Char]
updateOperators [] _ outops = outops
updateOperators _ [] outops = outops
updateOperators ((x, _):xs) (op:ops) outops =
    let nb = length $ grepmn2 "+-*/" x
    in if nb /= 0
       then 
           let newops = tailn (nb - 1) ops
           in if null newops
              then outops
              else updateOperators xs (tail newops) (outops ++ [head newops])
       else updateOperators xs ops (outops ++ [op])

--puzzle :: [Char] -> [[Char]] -> [Char] -> [[Char]]
--puzzle rslt nbs ops = 
--    let outhowadd = howAdd (length nbs)
--        refptree = howAddIntricated outhowadd
--    in concat [val1 | curops <- sequence (rep (length ops) ops)
--           , (val1, val2) <- subPuzzle nbs curops refptree,
--           val2 == rslt]

subPuzzle :: [[Char]] -> [Char] -> (PTree Int) -> [([Char], [Char])]
subPuzzle nbs ops (PNode x restxs) =
    let outv = map (\xs -> let newxs = map (\valx -> [valx]) xs
                               outxs = calculatePTree (PNode x newxs) nbs [0..l] []
                               outxs2 = createFormula2 outxs ops
                               newops = updateOperators outxs2 ops []
                               newformula = evaluateFormula outxs2 newops
                           in (newformula, calc newformula)) restxs
    in outv
    where l = length nbs - 1

evaluateFormula :: [([Char], [[Char]])] -> [Char] -> [Char]
evaluateFormula xs ops
    | all (\(_, vl) -> null vl) xs = 
        let outv = map (\(vl, _) -> vl) xs
        in createFormula outv ops
    | otherwise = 
        let depthxs = map (\(_, l) -> length l) xs
            maxval = myMax depthxs
            idx = grep2 maxval depthxs
            (x, lst) = (xs !! idx)
            newlst = init lst
        in if null newlst
            then let newxs = updateListElem xs idx ("(" ++ x ++ ")", newlst)
                 in evaluateFormula newxs ops
            else case find (\((_, lval), _) -> lval == init lst) (zip xs [0..(length xs - 1)]) of
                          Just xout -> 
                              let ((x2, _), opidx) = xout
                                  (newx2, newops) = if opidx < idx
                                          then (x2 ++ [(ops !! opidx)] ++ "(" ++ x ++ ")"
                                                ,deleteListElem ops opidx)
                                          else ("(" ++ x ++ ")" ++ [(ops !! idx)] ++ x2
                                                ,deleteListElem ops idx)
                                  newxs = updateListElem xs opidx (newx2, newlst)
                                  newxs2 = deleteListElem newxs idx
                              in evaluateFormula newxs2 newops
                          Nothing -> ""
        
createFormula2 :: [([[Char]], [[Char]])] -> [Char] -> [([Char], [[Char]])]
createFormula2 [] _ = []
createFormula2 [(x1, x2)] [] = [(concat x1, x2)]
createFormula2 ((x, n):xs) (op:ops)
    | length x == 1 = [((x !! 0), n)] ++ createFormula2 xs ops
    | otherwise     = 
        let (newx, newops) = subCreateFormula2 (op:ops) x
        in [(newx, n)] ++ createFormula2 xs (tail newops)

subCreateFormula2 :: [Char] -> [[Char]] -> ([Char], [Char])
subCreateFormula2 ops (x:xs) = subCreateFormula2b ops xs x

subCreateFormula2b :: [Char] -> [[Char]] -> [Char] -> ([Char], [Char])
subCreateFormula2b ops [] outxs = (outxs, ops)
subCreateFormula2b (op:ops) (x:xs) outxs = subCreateFormula2b ops xs (outxs ++ [op] ++ x)
