-- inspired by Programming in Haskell by Graham Hutton chapter on Countdown Problem
import Control.Monad

data Operator = Add | Times | Subtract | Divide
  deriving (Show)

ops:: [Operator]
ops = [Add, Times, Subtract, Divide]

evaluate :: Operator -> Maybe Int -> Maybe Int -> Maybe Int
evaluate Add x y = liftM2 (+) x y
evaluate Times x y = liftM2 (*) x y
evaluate Subtract x y = liftM2 (-) x y
evaluate Divide x y = liftM2 (div) x y


valid :: Operator -> Maybe Int -> Maybe Int -> Bool
valid _ Nothing _ = False
valid _ _ Nothing = False
valid Add x y = (x <= y)
valid Times x y = (x <= y)
valid Subtract x y = (x > y)
valid Divide x y = ((y /= (Just 0)) && ((liftM2 (mod) x y)== Just 0))


data ExpTree = Leaf (Maybe Int) | Branch (ExpTree) Operator (ExpTree)

instance Show ExpTree where
  show (Leaf n) = show n
  show (Branch (lTree) op (rTree)) = "(" ++ (show (lTree)) ++ " " ++ (show op) ++ " " ++ (show rTree) ++ ")" 

evaluateExpTree :: ExpTree -> Maybe Int
evaluateExpTree (Leaf n) = n
evaluateExpTree (Branch (lTree) op (rTree)) =
   if (valid op (evaluateExpTree lTree) (evaluateExpTree rTree)) then (evaluate op (evaluateExpTree lTree) (evaluateExpTree rTree))
   else Nothing
   

  
validTree :: ExpTree -> Bool
validTree (Leaf (Just n)) = True
validTree (Leaf Nothing) = False
validTree (Branch (lTree) op (rTree)) = valid op (evaluateExpTree lTree) (evaluateExpTree rTree)

mergeTrees :: ExpTree -> ExpTree -> [ExpTree]
mergeTrees t1 t2 = map (\op -> Branch (t1) op (t2)) ops

allExpTrees :: [Int] -> [ExpTree]
allExpTrees [] = []
allExpTrees [n] = [Leaf (Just n)]
allExpTrees xs = [t | (ls,rs) <- split xs, t1<-(allExpTrees ls), t2<-(allExpTrees rs), t<-mergeTrees t1 t2 , validTree t]

allExpSubTrees :: [Int] -> [ExpTree]
allExpSubTrees xs = [t | ys <- (subListsWithPerm xs), t<-(allExpTrees ys)]

solutions :: [Int] -> Int -> [ExpTree]
solutions = undefined

{- some functions to on lists to - split up a list into two sublists
                                 - find all sublists of a list (where ordering matters) -}

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []           
split (x:xs) = ([x],xs) : ((map(\(ls,rs)->(x:ls,rs))) (split xs))

intersperse :: a -> [a] -> [[a]]
intersperse x [] = [[x]]
intersperse x (y:ys) = (x:y:ys) : (map ((:) y) (intersperse x ys)) 

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations [x] = [[x]]
permutations (x:xs) = concat (map (intersperse x) (permutations xs))

subLists :: [a] -> [[a]]
subLists []  = [[]]
sublists [x] = [[], [x]]
sublists (x:xs)= map ((:) x) (sublists xs) ++ (sublists xs)

subListsWithPerm :: [a] -> [[a]]
subListsWithPerm xs = concat (map (permutations) (sublists xs))


