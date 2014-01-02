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

__2.1__
__2.2__
__2.3__
__2.4__
__2.5__
__2.6__
__2.7__

__2.8__

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
 

__2.9__

SRVA (Simple Reflex Vacuum Agent)

If the current square is dirty, suck.
Otherwise, alternate between tiles.

> instance N.AgentState VSquare
>  
> vRuleMatch :: VSquare -> [VRule] -> VRule
> vRuleMatch sq _ = case sq of (_, VDirty)  -> VSuck
>                              (VA, VClean) -> VRight
>                              (VB, VClean) -> VLeft

SRVA's types over the general implementation.

> srvaProgram :: (Monad m) => VSquare -> N.SimpleReflexAgent VSquare VSquare VRule VAction m ()
> srvaProgram = N.simpleReflexAgentProgram
> 

> scoreSRVA :: (Monad m, Num a) => Int -> VEnv -> m a
> scoreSRVA steps env =
>   do let progs = foldr1 (>>) (replicate steps srvaEnvProgram)
>      (_,scores) <- execRWST progs () env
>      (return . sum) scores
> 
> srvaEnvProgram :: (Monad m, Num a) => RWST () [a] VEnv m ()
> srvaEnvProgram =
>   do env@(VEnv priori loc) <- get
>      let per = (vLookupPrior priori) loc
>      (_,act:_) <- execRWST (srvaProgram per) (id, vRuleMatch, id, boundRange) () 
>      let env' = applyVAction env act
>      put env'
>      tell [vPerformanceMeasure env']
>  
> applyVAction :: VEnv -> VAction -> VEnv
> applyVAction (VEnv priori loc) act =
>   let floor = (snd . vLookupPrior priori) loc
>       (loc', floor') = case act of VSuck  -> (loc, VClean)
>                                    VLeft  -> (VA, floor)
>                                    VRight -> (VB, floor)
>       priori' = vUpdatePrior priori (loc, floor')
>   in (VEnv priori' loc')

Generates all possible vacuum world environments using each attribute's boundaries.

> allVEnvs :: [VEnv]
> allVEnvs = [VEnv [(VA, a), (VB, b)] loc | loc <- boundRange, a <- boundRange, b <- boundRange]

The average score for the SRVA when executed in all possible environments.
 
> vAvgScore :: (Monad m) => m Float
> vAvgScore =
>   do let envs = allVEnvs
>          size = (fromIntegral . length) envs
>      scores <- (sequence . map (scoreSRVA 1000)) envs
>      return ((sum scores) / size)

*Result: 1999.25*

__2.10__
__2.11__
__2.12__
__2.13__

