> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> module AIMA.Chapter2.Exercises where
> import qualified AIMA.Chapter2.Notes as N
> import Control.Monad.RWS

* 2.1
* 2.2
* 2.3
* 2.4
* 2.5
* 2.6
* 2.7

* 2.8

> data VacLocation = 
>     VacA
>   | VacB
>   deriving (Show, Eq, Enum, Bounded)
> 
> instance N.Percept VacLocation
> 
> data VacPercept =
>     VacClean
>   | VacDirty
>   deriving (Show, Eq, Enum, Bounded)
> 
> instance N.Percept VacPercept
>  
> data VacAction = 
>     VacLeft
>   | VacRight
>   | VacSuck
>   deriving (Show, Eq, Enum, Bounded)
>  
> instance N.Action VacAction
> 
> type VacRule = VacAction
> 
> instance N.Rule VacRule
>
> type VacSquare = (VacLocation, VacPercept) 
> instance N.Percept VacSquare
> 
> data VacEnv = VacEnv
>   { vacEnvPriori :: [VacSquare] }
>   deriving (Show, Eq)
> 
> vacEnvSquares :: VacEnv -> [VacSquare]
> vacEnvSquares = vacEnvPriori 

> vacPerformanceMeasure :: VacEnv -> Integer
> vacPerformanceMeasure env = (sum . map measure) (vacEnvSquares env)
>   where measure (_,p) = if p == VacClean then 1 else 0
 

* 2.9

> instance N.AgentState VacSquare
>  
> vacInterpretInput :: VacSquare -> VacSquare
> vacInterpretInput = id
>  
> vacRuleMatch :: VacSquare -> [VacRule] -> VacRule
> vacRuleMatch sq rules = case sq of (_, VacDirty) -> VacSuck
>                                    (VacA, VacClean) -> VacRight
>                                    (VacB, VacClean) -> VacLeft
>  
> vacRuleAction :: VacRule -> VacAction 
> vacRuleAction = id

> simpleReflexVacuumAgentProgram :: (Monad m) => VacSquare -> N.SimpleReflexAgent VacSquare VacSquare VacRule VacAction m ()
> simpleReflexVacuumAgentProgram = N.simpleReflexAgentProgram
>
> stepSimpleReflexVacuumAgentProgram :: (Monad m) => VacSquare -> m VacAction
> stepSimpleReflexVacuumAgentProgram p = 
>   do (_, _, actions) <- runRWST (simpleReflexVacuumAgentProgram p)
>                                 (vacInterpretInput, vacRuleMatch, vacRuleAction, [minBound..maxBound])
>                                 ()
>      return (head actions)

* 2.10
* 2.11
* 2.12
* 2.13
