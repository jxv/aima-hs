> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> 
> module AIMA.Chapter2.Exercises where
> import qualified AIMA.Chapter2.Notes as N
> import Control.Monad.RWS
> import Data.Maybe

> bndRng :: (Bounded b, Enum b) => [b]
> bndRng = [minBound..maxBound] 

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

> srvaProgram :: VSquare -> VAction
> srvaProgram per = 
>   let st = per -- | Interpret input
>       rule = st -- | Rule match
>       act = case rule of -- | Rule action
>               (_, VDirty)  -> VSuck 
>               (VA, VClean) -> VRight
>               (VB, VClean) -> VLeft
>   in act -- | Return action

Score the agent within an environment using *n*-steps.

> scoreSRVA :: (Num a) => Int -> VEnv -> a
> scoreSRVA stepcount env =
>   let steps = foldr1 (>>) (replicate stepcount stepSRVAEnv)
>       (_,scores) = execRWS steps () env
>   in sum scores
> 
> stepSRVAEnv :: (Num a) => RWS () [a] VEnv ()
> stepSRVAEnv =
>   do env@(VEnv priori loc) <- get
>      let per = (vLookupPrior priori) loc
>          act = srvaProgram per
>          env' = applyVAction env act
>      put env'
>      tell [vPerformanceMeasure env']
>  
> applyVAction :: VEnv -> VAction -> VEnv
> applyVAction (VEnv priori loc) act =
>   let flor = (snd . vLookupPrior priori) loc
>       (loc', flor') = case act of VSuck  -> (loc, VClean)
>                                   VLeft  -> (VA, flor)
>                                   VRight -> (VB, flor)
>       priori' = vUpdatePrior priori (loc, flor')
>   in VEnv priori' loc'

Generates all possible vacuum world environments using each attribute's boundaries.

> allVEnvs :: [VEnv]
> allVEnvs = [VEnv [(VA, fa), (VB, fb)] loc | loc <- bndRng, fa <- bndRng, fb <- bndRng]

The average score for the SRVA when executed in all possible environments.
 
> avgSRVAScore :: Float
> avgSRVAScore =
>   let size = (fromIntegral . length) allVEnvs
>       scores = map (scoreSRVA 1000) allVEnvs
>   in (sum scores) / size

*Result: 1999.25*

__2.10__
__2.10.a__ No, because the agent cannot remember or sense how dirty or clean is the adjacent location. It will never always make the correct choice.
__2.10.b__ Yes, because the agent can remember the previously visited locations.

> srsvaProgram :: [VSquare] -> VSquare -> ([VSquare], Maybe VAction)
> srsvaProgram st per = 
>   let st' = per : st -- Update state
>       rule = ruleMatch st' -- Rule match
>       act = rule -- Rule rction
>   in (st', act) -- Return updated state and possible action
>   where
>     ruleMatch states = 
>       if and [(loc, VClean) `elem` states | loc <- bndRng] 
>          then Nothing
>          else Just $ case (head states) of (_, VDirty)  -> VSuck
>                                            (VA, VClean) -> VRight
>                                            (VB, VClean) -> VLeft

> scoreSRSVA :: (Num a) => Int -> VEnv -> a
> scoreSRSVA stepcount env =
>   let steps = foldr1 (>>) (replicate stepcount stepSRSVAEnv)
>       (_, scores) = execRWS steps () ([], env)
>   in sum scores
> 
>  
> stepSRSVAEnv :: (Num a) => RWS () [a] ([VSquare],VEnv) ()
> stepSRSVAEnv =
>   do (st, env@(VEnv priori loc)) <- get
>      let per = (vLookupPrior priori) loc
>          (st', mact) = srsvaProgram st per
>          (env', doesmove) = fromMaybe (env, False) (mayApply env mact)
>          perf = vPerformanceMeasure env' - (if doesmove then 1 else 0)
>      put (st', env')
>      tell [perf]
>   where
>     mayApply env mact =
>       do act <- mact
>          let env' = applyVAction env act
>              doesmove = (or . map (== act)) [VLeft, VRight]
>          return (env', doesmove)

__2.10.c__ The former, simple reflex vacuum agent, can behave perfectly rational now.
           The latter, simple reflex state vacuum agent, won't need to sense adjacent locations, so it has a slight improve to perfect rationality. 

__2.11__
__2.12__
__2.13__

