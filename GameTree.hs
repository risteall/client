-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns, RecordWildCards, BangPatterns #-}

module GameTree(deref, derefNode, leftDepth, treeDepth,
                GameTree(tree, currentPos, viewPos), mkGameTree, pathEnd,
                select, nextBranch, prevBranch,
                deleteViewNode, deleteLine, deleteAll, deleteFromHere,
                treePlan, treeMove, mapCurrentToView) where

import Prelude hiding (GT)
import Data.Tree
import Data.List
import Data.Maybe
import Data.Bifunctor
import Data.Ord (comparing)

setListElem :: [a] -> Int -> a -> [a]
setListElem l n x = take n l ++ x : drop (n+1) l

foldAcc :: ([(a, Forest a, b, acc)] -> b) -> (acc -> Int -> acc) -> acc -> Forest a -> b
foldAcc f g acc forest = f (zipWith (\(Node x forest') ix -> let (!acc', q) = (g acc ix, foldAcc f g acc' forest') in
                                      (x, forest', q, acc'))
                                    forest [0..])


-- mapAcc :: (Int -> Int) -> Forest Int -> Forest Int
-- mapAcc f = foldAcc (map (\(a,_,b,_) -> Node (f a) b)) undefined undefined

-- foldAcc :: ([(a, Forest a, b, acc)] -> b) -> (acc -> Int -> acc) -> acc -> Forest a -> b
-- foldAcc f g acc forest = f (zipWith (\(Node x forest') ix -> let acc' = g acc ix in
--                                       (x, forest', foldAcc f g acc' forest', acc'))
--                                     forest [0..])

-- foldWithIndex :: ([(a, b, [Int])] -> b) -> Forest a -> b
-- foldWithIndex f = foldAcc (f . map (\(x,_,y,z) -> (x,y,z)))
--                           (\ix n -> ix ++ [n])
--                           []

foldWithRelIndex :: ([(a, Forest a, b, [([Int], [Int])])] -> b) -> [[Int]] -> Forest a -> b
foldWithRelIndex f ixs = foldAcc f (\l n -> map (g n) l) (map ([],) ixs)
  where
    g n (xs, y:ys) | n == y = (xs, ys)
    g n (xs, ys) = (n:xs, ys)

-- mapWithRelIndex :: (a -> Forest a -> b -> [([Int], [Int])] -> b) -> [[Int]] -> Forest a -> b
-- mapWithRelIndex f = foldWithRelIndex (map (\(a,b,c,d) -> f a b c d))

mapBetween :: [Int] -> [Int] -> (a -> a) -> Forest a -> Forest a
mapBetween top bottom f = foldWithRelIndex (map g) [top, bottom]
  where
    g (x, _, y, [(_,[]), ([],_)]) = Node (f x) y
    g (x, _, y, _) = Node x y

validIx :: Forest a -> [Int] -> Bool
validIx _ [] = True
validIx f (n:ns) | n >= length f = False
                 | otherwise = validIx (subForest (f !! n)) ns

-- unchecked
deref :: Forest a -> [Int] -> (Maybe a, Forest a)
deref f [] = (Nothing, f)
deref f [n] = case f !! n of Node x f' -> (Just x, f')
deref f (n:ns) = deref (subForest (f !! n)) ns

-- unchecked
derefNode :: Forest a -> [Int] -> Maybe a
derefNode f ix = fst $ deref f ix

-- unchecked
derefForest :: Forest a -> [Int] -> Forest a
derefForest f ix = snd $ deref f ix

leftDepth :: Forest a -> Int
leftDepth [] = 0
leftDepth (Node _ f : _) = 1 + leftDepth f

treeDepth :: Forest a -> Int
treeDepth [] = 0
treeDepth l = 1 + maximum (map (treeDepth . subForest) l)

-- index unchecked
modifyAt :: Forest a -> [Int] -> (Forest a -> Forest a) -> Forest a
modifyAt forest [] f = f forest
modifyAt forest (n:ns) f = setListElem forest n $ Node x (modifyAt forest' ns f)
  where Node x forest' = forest !! n

setAt :: Forest a -> [Int] -> Forest a -> Forest a
setAt f ix f' = modifyAt f ix (const f')

-- index unchecked
deleteNode :: [Int] -> Forest a -> [Int] -> (Forest a, [Int])
deleteNode [] f currentPos = (f, currentPos)
deleteNode [n] f [] = (take n f ++ drop (n+1) f, [])
deleteNode [n] f (m:ms) | n == m = (f, m:ms)
                        | otherwise = (take n f ++ drop (n+1) f, (if m<n then m else m-1) : ms)
deleteNode (n:ns) f (m:ms)
  | n == m = let Node x f' = f !! n in case deleteNode ns f' ms of
    (f'', pos') -> (setListElem f n (Node x f''), m : pos')
deleteNode (n:ns) f currentPos = let Node x f' = f !! n in case deleteNode ns f' [] of
  (f'', pos') -> (setListElem f n (Node x f''), currentPos)

----------------------------------------------------------------

-- invariants: currentPos, pathPos, viewPos always within tree; viewPos <= pathPos
data GameTree a = GT
  {tree :: Forest a
  ,currentPos, viewPos, pathPos :: [Int]
  }

mkGameTree :: Forest a -> [Int] -> [Int] -> GameTree a
mkGameTree f cp vp = GT{tree = f
                       ,currentPos = if validIx f cp then cp else []
                       ,viewPos = vp'
                       ,pathPos = vp'
                       }
  where vp' = if validIx f vp then vp else []

pathEnd :: GameTree a -> [Int]
pathEnd GT{tree, currentPos, pathPos} = pos ++ replicate (leftDepth (derefForest tree pos)) 0
  where pos = if pathPos `isPrefixOf` currentPos then currentPos else pathPos

select :: [Int] -> GameTree a -> GameTree a
select pos gt | not (validIx (tree gt) pos) = gt
select pos gt@GT{viewPos, pathPos}
  = gt{viewPos = pos
      ,pathPos = if isPrefixOf pos pathPos
                   then pathPos
                   else pos
      }

-- index unchecked
sortNode :: GameTree a -> [Int] -> [(Tree a, Int)]
sortNode gt pos = uncurry (++)
                    $ partition (\(_,n) -> (pos ++ [n]) `isPrefixOf` currentPos gt)
                    $ zip (derefForest (tree gt) pos) [0..]

selectBranch :: ([Int] -> Int -> Int) -> GameTree a -> GameTree a
selectBranch _ gt@GT{viewPos = []} = gt
selectBranch f gt@GT{tree, viewPos} = select (lastBranch ++ [n]) gt
  where
    lastBranch = fromMaybe [] $ find (\pos -> 1 < length (derefForest tree pos))
                                     $ tail (reverse (inits viewPos))
    n = f (map snd (sortNode gt lastBranch))
          $ head $ drop (length lastBranch) viewPos

nextBranch, prevBranch :: GameTree a -> GameTree a
nextBranch = selectBranch $ \l n -> case findIndex (== n) l of
  Just ix -> l !! ((ix + 1) `mod` length l)

prevBranch = selectBranch $ \l n -> case findIndex (== n) l of
  Just ix -> l !! ((ix - 1) `mod` length l)

deleteViewNode :: GameTree a -> GameTree a
deleteViewNode gt@GT{currentPos, viewPos} | null viewPos || isPrefixOf viewPos currentPos = gt
deleteViewNode GT{tree, currentPos, viewPos} = case deleteNode viewPos tree currentPos of
  (tree', currentPos') -> GT{tree = tree'
                            ,currentPos = currentPos'
                            ,viewPos = init viewPos
                            ,pathPos = init viewPos
                            }

deleteLine :: GameTree a -> GameTree a
deleteLine gt@GT{tree, currentPos, viewPos} = case reverse (inits viewPos) of
  ix:_ | isPrefixOf ix currentPos -> gt
  ix:ixs -> let removePos = last $ ix : takeWhile (\ix' -> not (isPrefixOf ix' currentPos) && 1 == length (derefForest tree ix')) ixs in
    case deleteNode removePos tree currentPos of
      (tree', currentPos') -> GT{tree = tree'
                                ,currentPos = currentPos'
                                ,viewPos = init removePos
                                ,pathPos = init removePos
                                }

deleteAll :: GameTree a -> GameTree a
deleteAll GT{tree, currentPos, viewPos} = GT{tree = f tree currentPos
                                            ,currentPos = map (const 0) currentPos
                                            ,viewPos = replicate (g viewPos currentPos) 0
                                            ,pathPos = replicate (g viewPos currentPos) 0
                                            }
  where
    f [] _ = []
    f _ [] = []
    f ts (n:ns) = case ts !! n of Node x ts' -> [Node x (f ts' ns)]
    g (x:xs) (y:ys) | x == y = 1 + g xs ys
    g _ _ = 0

-- indices unchecked
deleteFrom :: [Int] -> Forest a -> [Int] -> (Forest a, [Int] -> Maybe [Int])
deleteFrom here forest currentPos = case stripPrefix here currentPos of
  Just (n:_) -> (forest', f)
    where
      forest' = modifyAt forest here (\f -> [f !! n])
      f pos = case stripPrefix here pos of
        Just (n':l) | n == n' -> Just (here ++ 0 : l)
                    | otherwise -> Nothing
        _ -> Just pos
  _ -> (forest', f)
    where
      forest' = modifyAt forest here (const [])
      f pos = case stripPrefix here pos of
        Just (_:_) -> Nothing
        _ -> Just pos


deleteFromHere :: GameTree a -> GameTree a
deleteFromHere GT{..}
    = GT{tree = tree'
        ,currentPos = fromJust (f currentPos)
        ,viewPos = viewPos
        ,pathPos = fromMaybe viewPos $ f pathPos
        }
  where (tree', f) = deleteFrom viewPos tree currentPos

treePlan :: (a -> a -> Bool) -> a -> GameTree a -> GameTree a
treePlan eq x gt@GT{tree, viewPos} = case find (\(Node x' _, _) -> eq x x')
                                               $ zip f [0..] of
    Just (_, n) -> select (viewPos ++ [n]) gt
    Nothing -> gt{tree = modifyAt tree viewPos (++ [Node x []])
                 ,viewPos = viewPos ++ [length f]
                 ,pathPos = viewPos ++ [length f]
                 }
  where f = derefForest tree viewPos

-- returned function on indices returns True if node remains in tree, otherwise (closest remaining node, False)
mapFromRoot :: (b -> a -> Maybe (a, Maybe b)) -> b -> Forest a -> (Forest a, [Int] -> ([Int], Bool))
mapFromRoot f x forest = (map (fst . fst) l, mapIndex)
  where
    g (Node y forest') = case f x y of
      Nothing -> Nothing
      Just (y', Nothing) -> Just (Node y' forest', (,True))
      Just (y', Just x') -> Just $ first (Node y') $ mapFromRoot f x' forest'
    l = [(t, n) | (Just t, n) <- zip (map g forest) [0..]]
    mapIndex [] = ([], True)
    mapIndex (n:ns) = case find ((== n) . snd . fst) (zip l [0..]) of
      Nothing -> ([], False)
      Just (((_,func),_),n') -> first (n':) (func ns)

-- foldAcc :: ([(a, Forest a, b, acc)] -> b) -> (acc -> a -> Int -> acc) -> acc -> Forest a -> b

-- mapFromRoot :: (b -> a -> Maybe (a, Maybe b)) -> b -> Forest a -> (Forest a, [Int] -> ([Int], Bool))
-- mapFromRoot f = foldAcc () (\b a _ -> 

replace :: Int -> a -> (a -> Maybe a) -> [a] -> ([a], Int -> Maybe Int)
replace n x f xs = (xs', (`elemIndex` ixs))
  where
    g (x', n') = (,n') <$> if n' == n then Just x else f x'
    (xs', ixs) = unzip $ mapMaybe g (zip xs [0..])

-- killPlans in caller
treeMove :: Ord b => (a -> a -> Maybe b)
            -> (a -> Maybe a)
            -> (a -> a -> Bool)
            -> (Bool -> a -> a -> Maybe a)
            -> Bool -> GameTree a -> a
            -> (GameTree a, Bool)  -- returns whether to kill arrows
treeMove match replaceFunc eq propagate moveWithCurrent GT{..} x
    = (if atCurrent then select cp' gt' else gt', not viewPreserved || atCurrent)
  where
    liftedReplace (Node a b) = (\a' -> Node a' b) <$> replaceFunc a
    nodes = derefForest tree currentPos
    (tree', cp', mapIx) = case mapMaybe (\z@(Node x' _,_) -> (z,) <$> match x x') $ zip nodes [0..] of
      [] -> (setAt tree currentPos (nodes' ++ [Node x []]), currentPos ++ [length nodes'], mapIx')
        where
          (nodes', ixs) = unzip $ mapMaybe (\(t,ix) -> (,ix) <$> liftedReplace t) (zip nodes [0..])
          mapIx' i = case stripPrefix currentPos i of
            Just (n:ns) -> case elemIndex n ixs of
              Nothing -> (currentPos, False)
              Just n' -> (currentPos++n':ns, True)
            _ -> (i, True)
      l -> (setAt tree currentPos nodes', currentPos ++ [ix'], mapIx')
        where
          ((Node replaced sub, ix), _) = minimumBy (comparing snd) l
          (sub', f) = mapFromRoot (\(oldPrev, newPrev, top) node -> if eq oldPrev newPrev
                                                                      then Just (node, Nothing)
                                                                      else (\node' -> (node', Just (node, node', False)))
                                                                             <$> propagate top newPrev node)
                                  (replaced, x, True) sub
          (nodes', g) = replace ix (Node x sub') liftedReplace nodes
          Just ix' = g ix
          mapIx' i = case stripPrefix currentPos i of
            Just (n:ns) | n == ix -> first ((currentPos++[ix'])++) $ f ns
                        | otherwise -> case g n of
                          Nothing -> (currentPos, False)
                          Just n' -> (currentPos++n':ns, True)
            _ -> (i, True)
    (vp', viewPreserved) = mapIx viewPos
    gt' = GT{tree = tree'
            ,currentPos = cp'
            ,viewPos = vp'
            ,pathPos = fst $ mapIx pathPos
            }
    atCurrent = vp' == currentPos && moveWithCurrent

mapCurrentToView :: (a -> a) -> GameTree a -> GameTree a
mapCurrentToView f gt = gt{tree = mapBetween (currentPos gt) (viewPos gt) f (tree gt)}
