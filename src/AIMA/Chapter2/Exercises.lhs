> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> 
> module AIMA.Chapter2.Exercises where
> import qualified AIMA.Chapter2.Notes as N
> import Control.Monad.RWS
> import Control.Monad.Loops
> import Control.Arrow
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
> data VPercept =
>     VClean
>   | VDirty
>   deriving (Show, Eq, Enum, Bounded)
> 
> instance N.Percept VPercept
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
> type VSquare = (VLoc, VPercept) 
> instance N.Percept VSquare
> 
> data VEnv = VEnv
>   { vPriori :: [VSquare], vLoc :: VLoc }
>   deriving (Show, Eq)
 
> vLookupPrior :: [VSquare] -> VLoc -> VSquare
> vLookupPrior priori loc = priori !! (fromEnum loc)
> 
> vUpdatePrior :: [VSquare] -> VSquare -> [VSquare]
> vUpdatePrior priori sq = let idx = fromEnum (fst sq)
>                              update = sq
>                          in (take idx priori) ++ [update] ++ (drop (idx + 1) priori)

> vPerformanceMeasure :: (Num a) => VEnv -> a
> vPerformanceMeasure env = (sum . map measure) (vPriori env)
>   where measure (_,p) = if p == VClean then 1 else 0
 

* 2.9

> instance N.AgentState VSquare
>  
> vInterpretInput :: VSquare -> VSquare
> vInterpretInput = id
>  
> vRuleMatch :: VSquare -> [VRule] -> VRule
> vRuleMatch sq rules = case sq of (_, VDirty)  -> VSuck
>                                  (VA, VClean) -> VRight
>                                  (VB, VClean) -> VLeft
>  
> vRuleAction :: VRule -> VAction 
> vRuleAction = id
> 
> vReader = (vInterpretInput, vRuleMatch, vRuleAction, boundRange)

SRVA (Simple Reflex Vacuum Agent)

> srvaProgram :: (Monad m) => VSquare -> N.SimpleReflexAgent VSquare VSquare VRule VAction m ()
> srvaProgram = N.simpleReflexAgentProgram
> 
> stepSRVAProgram :: (Monad m) => VSquare -> m VAction
> stepSRVAProgram p = 
>   do (_, _, actions) <- runRWST (srvaProgram p) vReader ()
>      return (head actions)


> applyVAction :: VEnv -> VAction -> VEnv
> applyVAction (VEnv priori loc) act =
>   let percept = (snd . vLookupPrior priori) loc
>       (loc', percept') = case act of VSuck  -> (loc, VClean)
>                                      VLeft  -> (VA, percept)
>                                      VRight -> (VB, percept)
>       priori' = vUpdatePrior priori (loc, percept')
>   in (VEnv priori' loc')
> 
> stepScoreSRVA :: (Monad m, Num a) => VEnv -> m (a, VEnv) 
> stepScoreSRVA env@(VEnv priori loc) =
>   do let per = (vLookupPrior priori) loc
>      act <- stepSRVAProgram per
>      let env' = applyVAction env act
>      let score = vPerformanceMeasure env'
>      return (score, env')
> 
> scoreSRVA :: (Monad m, Num a) => Int -> VEnv -> m a 
> scoreSRVA steps env =
>   do let stepper (s, e) = stepScoreSRVA e >>= \(s', e') -> return (s + s', e')
>          runsteps = concatM (replicate steps stepper) 
>      (score,_) <- runsteps (0, env)
>      return score

> allVEnvs :: [VEnv]
> allVEnvs = [VEnv [(VA, a), (VB, b)] loc | loc <- boundRange, a <- boundRange, b <- boundRange]
> 
> vAvgScore :: (Monad m, Functor m) => m Integer
> vAvgScore = (fmap sum . sequence . map (scoreSRVA 1000)) allVEnvs


* 2.10
* 2.11
* 2.12
* 2.13
