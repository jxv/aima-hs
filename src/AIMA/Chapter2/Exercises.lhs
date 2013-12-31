> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> 
> module AIMA.Chapter2.Exercises where
> import qualified AIMA.Chapter2.Notes as N
> import Control.Monad.RWS
> import Control.Monad.Loops
> import Control.Arrow
> import Control.Applicative
> import Data.List

> boundRange :: (Bounded b, Enum b) => [b]
> boundRange = [minBound..maxBound] 

* 2.1
* 2.2
* 2.3
* 2.4
* 2.5
* 2.6
* 2.7

* 2.8

> data VLoc = 
>     VA
>   | VB
>   deriving (Show, Eq, Enum, Bounded)
> 
> instance N.Percept VLoc
> 
> data VFloor =
>     VClean
>   | VDirty
>   deriving (Show, Eq, Enum, Bounded)
> 
> instance N.Percept VFloor
>  
> data VAction = 
>     VLeft
>   | VRight
>   | VSuck
>   deriving (Show, Eq, Enum, Bounded)
>  
> instance N.Action VAction
> 
> type VRule = VAction
> 
> instance N.Rule VRule
> 
> type VSquare = (VLoc, VFloor) 
> instance N.Percept VSquare
> 
> data VEnv = VEnv
>   { vPriori :: [VSquare]
>   , vLoc :: VLoc }
>   deriving (Show, Eq)
>  
> vLookupPrior :: [VSquare] -> VLoc -> VSquare
> vLookupPrior priori loc = priori !! (fromEnum loc)
> 
> vUpdatePrior :: [VSquare] -> VSquare -> [VSquare]
> vUpdatePrior priori sq = let idx = fromEnum (fst sq)
>                              update = sq
>                          in (take idx priori) ++ [update] ++ (drop (idx + 1) priori)

> vPerformanceMeasure :: (Num a) => VEnv -> a
> vPerformanceMeasure env = (sum . map measure) (vPriori env)
>   where measure (_, p) = if p == VClean then 1 else 0
 

* 2.9

SRVA (Simple Reflex Vacuum Agent)

If the current square is dirty, suck.
Otherwise, alternate between tiles.

> instance N.AgentState VSquare
>  
> vRuleMatch :: VSquare -> [VRule] -> VRule
> vRuleMatch sq rules = case sq of (_, VDirty)  -> VSuck
>                                  (VA, VClean) -> VRight
>                                  (VB, VClean) -> VLeft

SRVA's types over the general implementation.

> 
> srvaProgram :: (Monad m, Functor m) => VSquare -> N.SimpleReflexAgent VSquare VSquare VRule VAction m ()
> srvaProgram = N.simpleReflexAgentProgram

Score the SRVA over *n*-steps in some environment.
For each step, it applies its actions onto the environment then scores the result.
 
> scoreSRVA :: (Monad m, Functor m, Num a) => Int -> VEnv -> m a 
> scoreSRVA steps env = 
>   do let stepper (s, e) = (((+s) . vPerformanceMeasure) &&& id) <$> stepSRVAEnv e
>          runsteps = concatM (replicate steps stepper) 
>      (score,_) <- runsteps (0, env)
>      return score
>  
> stepSRVAEnv :: (Monad m, Functor m, Num a) => VEnv -> m VEnv
> stepSRVAEnv env@(VEnv priori loc) =
>   do let per = (vLookupPrior priori) loc
>      act <- stepSRVAProgram per
>      return (applyVAction env act)
> 
> stepSRVAProgram :: (Monad m, Functor m) => VSquare -> m VAction
> stepSRVAProgram p = 
>   do (_, _, actions) <- runRWST (srvaProgram p) reader ()
>      return (head actions)
>   where reader = (id, vRuleMatch, id, boundRange)
> 
> applyVAction :: VEnv -> VAction -> VEnv
> applyVAction (VEnv priori loc) act =
>   let percept = (snd . vLookupPrior priori) loc
>       (loc', percept') = case act of VSuck  -> (loc, VClean)
>                                      VLeft  -> (VA, percept)
>                                      VRight -> (VB, percept)
>       priori' = vUpdatePrior priori (loc, percept')
>   in (VEnv priori' loc')

Generates all possible vacuum world environments using each attributes' boundaries.

> allVEnvs :: [VEnv]
> allVEnvs = [VEnv [(VA, a), (VB, b)] loc | loc <- boundRange, a <- boundRange, b <- boundRange]

The average score for the SRVA when executed in all possible environments.
 
> vAvgScore :: (Monad m, Functor m) => m Float
> vAvgScore = let envs   = allVEnvs
>                 size   = (fromIntegral . length) envs
>                 scores = (sequence . map (scoreSRVA 1000)) envs
>             in fmap ((/ size) . sum) scores

**Result:** 1999.25


* 2.10
* 2.11
* 2.12
* 2.13
