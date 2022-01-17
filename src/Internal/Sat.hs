module Internal.Sat where

import Control.Monad ()
import Data.Maybe (listToMaybe)
import qualified Dimacs

type Literal = Integer

type Clause = [Literal]

type Formula = [Clause]

type Record = [Literal]

data SolverState = SolverState
  { formula :: Formula,
    record :: Record
  }
  deriving (Show)

dpll :: SolverState -> Maybe Record
dpll (SolverState [] r) = return r
dpll s =
  if null f
    then return r
    else do
      l <- chooseLiteral f
      case dpll (SolverState (simplify f l) (l : r)) of
        Just record -> return record
        Nothing -> dpll $ SolverState (simplify f n) (n : r)
          where
            n = - l
  where
    s' = unitpropagate s
    f = formula s'
    r = record s'

unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
  case getUnit f of
    Nothing -> SolverState f r
    Just u -> unitpropagate $ SolverState (simplify f u) (u : r)

getUnit :: Formula -> Maybe Literal
getUnit xs = listToMaybe [x | [x] <- xs]

chooseLiteral :: Formula -> Maybe Literal
chooseLiteral = listToMaybe . concat

simplify :: Formula -> Literal -> Formula
simplify f l = [simpClause x l | x <- f, not (clauseSat x l)]

simpClause :: Clause -> Literal -> Clause
simpClause c l = filter (/= - l) c

clauseSat :: Clause -> Literal -> Bool
clauseSat c l = l `elem` c

solve :: [[Integer]] -> Maybe [Integer]
solve f = dpll $ SolverState f []

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
