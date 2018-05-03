-- -*- Haskell -*-

{-# LANGUAGE LambdaCase, TupleSections, NamedFieldPuns, RecordWildCards #-}

module GameTree(dropPrefix, derefNode, leftDepth, treeDepth,
                GameTree(tree, currentPos, viewPos), mkGameTree, pathEnd,
                select, nextBranch, prevBranch,
                deleteViewNode, deleteLine, deleteAll, deleteFromHere,
                treePlan, treeMove) where

import Prelude hiding (GT)
import Data.Tree
import Data.List
import Data.Maybe

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix [] l = Just l
dropPrefix _ [] = Nothing
dropPrefix (x:xs) (y:ys) | x == y = dropPrefix xs ys
                         | otherwise = Nothing

setListElem :: [a] -> Int -> a -> [a]
setListElem l n x = take n l ++ x : drop (n+1) l

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
deleteFrom here forest currentPos = case dropPrefix here currentPos of
  Just (n:_) -> let
      forest' = modifyAt forest here (\f -> [f !! n])
      f pos = case dropPrefix here pos of
        Just (n':l) | n == n' -> Just (here ++ 0 : l)
                    | otherwise -> Nothing
        _ -> Just pos
     in (forest', f)
  _ -> let
      forest' = modifyAt forest here (const [])
      f pos = case dropPrefix here pos of
        Just (_:_) -> Nothing
        _ -> Just pos
     in (forest', f)

deleteFromHere :: GameTree a -> GameTree a
deleteFromHere GT{..}
    = GT{tree = tree'
        ,currentPos = fromJust (f currentPos)
        ,viewPos = viewPos
        ,pathPos = fromMaybe viewPos $ f pathPos
        }
  where (tree', f) = deleteFrom viewPos tree currentPos

treePlan :: Eq a => a -> GameTree a -> GameTree a
treePlan x gt@GT{tree, viewPos} = case find (\(Node x' _, _) -> x == x')
                                            $ zip f [0..] of
    Just (_, n) -> select (viewPos ++ [n]) gt
    Nothing -> gt{tree = modifyAt tree viewPos (++ [Node x []])
                 ,viewPos = viewPos ++ [length f]
                 ,pathPos = viewPos ++ [length f]
                 }
  where f = derefForest tree viewPos

  -- returns whether to kill arrows
treeMove :: Eq a => Bool -> Bool -> GameTree a -> a -> (GameTree a, Bool)
treeMove killPlans haveInput gt@GT{tree = t, currentPos = cp, viewPos = vp} x
      = if killPlans then kill gt' else (gt', False)
  where
    nodes = derefForest t cp
              -- if move is already in tree, replace in order to get new move time
    (nodes', bs) = unzip $ map (\(Node x' f) -> if x == x' then (Node x f, True) else (Node x' f, False))
                               nodes
    (tree', currentPos') = case findIndex id bs of
      Just n -> (modifyAt t cp (const nodes'), cp ++ [n])
      Nothing -> (modifyAt t cp (++ [Node x []]), cp ++ [length nodes])
    atCurrent = vp == cp && not haveInput
    gt' = (if atCurrent then select currentPos' else id)
             gt{tree = tree', currentPos = currentPos'}
    kill g@GT{..} = (g{tree = t'
                      ,currentPos = cp'
                      ,viewPos = fromMaybe cp' $ f viewPos
                      ,pathPos = fromMaybe cp' $ f pathPos
                      },
                     isNothing (f viewPos) || atCurrent
                    )
      where (t', f) = deleteFrom cp tree currentPos
            cp' = fromJust $ f currentPos
