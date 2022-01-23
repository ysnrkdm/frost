module Internal.Cdcl where

import Control.Monad ()
import qualified Data.IntMap as IntMap
import Data.List (foldl', nub, sort, (\\))
import Data.Maybe (listToMaybe)
import Debug.Trace
import qualified Dimacs

type Literal = Integer

type Clause = [Literal]

type Formula = [Clause]

type Record = [Literal]

type RecordMap = IntMap.IntMap [Literal]

type DecisionLevel = Int

data SolverState = SolverState
  { formula :: Formula,
    record :: RecordMap,
    decided :: [Literal]
  }
  deriving (Show)

cdcl :: SolverState -> Maybe Record
cdcl = cdcl' 0

cdcl' :: DecisionLevel -> SolverState -> Maybe Record
cdcl' dl (SolverState [] r d) = return $ concat $ IntMap.elems r
cdcl' dl s
  | evaled == Just True = return $ concat $ IntMap.elems r
  | evaled == Just False =
    if dl == 0
      then trace ("UNSAT: " ++ show s') Nothing
      else do
        -- traceM $ "dl is " ++ show dl
        let (c, bl) = analyze s' dl
        -- traceM $ "bl is " ++ show bl
        cdcl' bl (SolverState (c : f) (deleteKeysEqLessThan r bl) (reverse $ take bl (reverse d)))
  | otherwise = do
    -- Todo: Stop seek true false both, and use heuristics to chooseLiteral
    l <- chooseLiteral f r
    cdcl' (dl + 1) (decide (SolverState f r d) l dl)
  where
    s' = unitpropagate dl s
    f = formula s'
    r = record s'
    d = decided s'
    evaled =
      --trace ("evaled: " ++ show (eval s') ++ ", upon: " ++ show s')
      eval s'

imply :: SolverState -> Literal -> DecisionLevel -> SolverState
imply (SolverState f r d) i dl =
  --trace ("implied: " ++ show i ++ " at " ++ show dl ++ ", and " ++ show (SolverState f r' d))
  SolverState f r' d
  where
    r' = IntMap.insertWith (++) dl [i] r

decide :: SolverState -> Literal -> DecisionLevel -> SolverState
decide s@(SolverState f r d) i dl =
  --trace ("decided: " ++ show i ++ " at " ++ show dl ++ ", and " ++ show (SolverState f r' d'))
  SolverState f r' d'
  where
    r' = IntMap.insertWith (++) dl [i] r
    d' = i : d

deleteKeysEqLessThan :: RecordMap -> Int -> RecordMap
deleteKeysEqLessThan r bl =
  case IntMap.lookupGE bl r of
    Nothing -> r
    Just (k, _) -> deleteKeysEqLessThan (IntMap.delete k r) bl

unitpropagate :: DecisionLevel -> SolverState -> SolverState
unitpropagate dl (SolverState f r []) = unitpropagate' dl (SolverState f r []) f
unitpropagate dl (SolverState f r d) = unitpropagate' dl (SolverState (simplify f (head d)) r d) f

unitpropagate' :: DecisionLevel -> SolverState -> Formula -> SolverState
unitpropagate' dl (SolverState f r d) off =
  case getUnit f r of
    Nothing ->
      --trace ("unitpropagated: " ++ show f ++ " <- " ++ show off ++ ", getUnit " ++ show (getUnit f r))
      SolverState off r d
    Just u -> unitpropagate' dl (imply (SolverState (simplify f u) r d) u dl) off

vars :: [Literal] -> [Literal]
vars literal = literal ++ map (\x -> - x) literal

getUnit :: Formula -> RecordMap -> Maybe Literal
getUnit xs r = listToMaybe [x | [x] <- xs, x `notElem` vars (concat $ IntMap.elems r)]

posAndNegs :: [Literal] -> [Literal]
posAndNegs = foldl' (\r x -> (- x) : x : r) []

chooseLiteral :: Formula -> RecordMap -> Maybe Literal
chooseLiteral f r = listToMaybe forms
  where
    literals = nub $ concat f
    litmaps = posAndNegs $ concat $ IntMap.elems r
    forms =
      --trace ("choosingLiteral, " ++ show (literals \\ litmaps) ++ " <- " ++ show literals ++ " \\ " ++ show litmaps)
      literals \\ litmaps

simplify :: Formula -> Literal -> Formula
simplify f l = [simpClause x l | x <- f, not (clauseSat x l)]

simpClause :: Clause -> Literal -> Clause
simpClause c l = filter (/= - l) c

clauseSat :: Clause -> Literal -> Bool
clauseSat c l = l `elem` c

evalClause :: Clause -> Record -> Bool -> Maybe Bool
evalClause [] r b = return b
evalClause (cx : cxs) r b
  | cx `elem` r = evalClause cxs r True
  | (- cx) `elem` r = evalClause cxs r b
  | otherwise = Nothing

evalMaybeList :: [Maybe Bool] -> Maybe Bool
evalMaybeList [] = return True
evalMaybeList (x : xs) = case x of
  Nothing -> Nothing
  Just True -> evalMaybeList xs
  Just False -> return False

eval :: SolverState -> Maybe Bool
eval (SolverState f r _) = evalMaybeList $ map (\x -> evalClause x r' False) f
  where
    r' = nub $ sort $ concat $ IntMap.elems r

invertClause :: Clause -> Clause
invertClause = map (\x -> - x)

analyze :: SolverState -> DecisionLevel -> (Clause, DecisionLevel)
analyze s@(SolverState f r d) dl =
  --trace ("analyzing " ++ show s ++ ", bl to " ++ show (length d - 1))
  (invertClause d, dl - 1)

solve :: [[Integer]] -> Maybe [Integer]
solve f = cdcl $ SolverState f IntMap.empty []

intsFromClause :: [Integer] -> Dimacs.Clause -> [Integer]
intsFromClause s (Dimacs.Clause []) = s
intsFromClause s Dimacs.EmptyClause = s
intsFromClause s (Dimacs.UnitClause x) = intsFromClause s (Dimacs.Clause [x])
intsFromClause s (Dimacs.Clause (x : r)) =
  case x of
    Dimacs.Not (Dimacs.Var i) -> intsFromClause ((- (fromIntegral i)) : s) (Dimacs.Clause r)
    Dimacs.Normal (Dimacs.Var i) -> intsFromClause (fromIntegral i : s) (Dimacs.Clause r)

intsFromCnf :: Dimacs.CNF -> [[Integer]]
intsFromCnf (Dimacs.CNF cnf) =
  map (intsFromClause []) cnf

solveDIMACS :: Dimacs.DIMACS -> Maybe [Integer]
solveDIMACS (Dimacs.DIMACS _ _ cnf) =
  solve $ intsFromCnf cnf
