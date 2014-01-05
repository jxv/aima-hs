> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> 
> module AIMA.Chapter2.Exercises where
> import qualified AIMA.Chapter2.Notes as N
> import Control.Monad.RWS
> import Control.Monad.State
> import Data.Maybe
> import Data.List
> import Data.List.Split
> import System.Random

> bndRng :: (Bounded b, Enum b) => [b]
> bndRng = [minBound..maxBound] 
> 
> io :: (MonadIO m) => IO a -> m a
> io = liftIO
> 
> randEnum :: (Enum e) => (e, e) -> IO e
> randEnum (low,high) = do r <- randomRIO (fromEnum low, fromEnum high)
>                          return (toEnum r)

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
> data VFloor =
>     VClean
>   | VDirty
>   deriving (Show, Eq, Enum, Bounded)
> 
> data VAction = 
>     VLeft
>   | VRight
>   | VSuck
>   deriving (Show, Eq, Enum, Bounded)
>  
> type VSquare = (VLoc, VFloor) 
> 
> data VEnv = VEnv
>   { vPriori :: [VSquare]
>   , vLoc :: VLoc }
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
>   where measure (_, p) = if p == VClean then 1 else 0
 

__2.9__

SRVA (Simple Reflex Vacuum Agent)

If the current square is dirty, suck.
Otherwise, alternate between tiles.

> srvaRuleAction :: VSquare -> VAction
> srvaRuleAction rule =
>   case rule of -- | Rule action
>     (_, VDirty)  -> VSuck 
>     (VA, VClean) -> VRight
>     (VB, VClean) -> VLeft
> 
> srvaProgram :: VSquare -> VAction
> srvaProgram per = 
>   let st = per -- | Interpret input
>       rule = st -- | Rule match
>       act = srvaRuleAction rule  -- | Rule action
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
>   let flr = (snd . vLookupPrior priori) loc
>       (loc', flr') = case act of VSuck  -> (loc, VClean)
>                                  VLeft  -> (VA, flr)
>                                  VRight -> (VB, flr)
>       priori' = vUpdatePrior priori (loc, flr')
>   in VEnv priori' loc'

Generates all possible vacuum world environments using each attribute's boundaries.

> allVEnvs :: [VEnv]
> allVEnvs = [VEnv [(VA, fa), (VB, fb)] loc | loc <- bndRng, fa <- bndRng, fb <- bndRng]

The average score for the SRVA when executed in all possible environments.
 
> avgSRVAScore :: Float
> avgSRVAScore =
>   let size = (fromIntegral . length) allVEnvs
>       scores = map (scoreSRVA 1000) allVEnvs
>   in sum scores / size

*Result: 1999.25*

__2.10__

* a. No, because the agent cannot remember or sense how dirty or clean is the adjacent location. It will never always make the correct choice.

* b. Yes, because the agent can remember the previously visited locations.

> srsvaProgram :: [VSquare] -> VSquare -> ([VSquare], Maybe VAction)
> srsvaProgram st per = 
>   let st' = per : st -- Update state
>       rule = st' -- Rule match
>       act = raction rule -- Rule action
>   in (st', act) -- Return updated state and maybe action
>   where
>     raction perhist = 
>       if and [(loc, VClean) `elem` perhist | loc <- bndRng] 
>          then Nothing
>          else (Just . srvaRuleAction . head) perhist

> scoreSRSVA :: (Num a) => Int -> VEnv -> a
> scoreSRSVA stepcount env =
>   let steps = foldr1 (>>) (replicate stepcount stepSRSVAEnv)
>       (_,scores) = execRWS steps () ([], env)
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

* c. The former, simple reflex vacuum agent, can behave perfectly rational now.
     The latter, simple reflex state vacuum agent, won't need to sense adjacent locations, so it has a slight improve to perfect rationality. 

__2.11__

* a. No, such an agent, which lacks the ability to store and remember state, cannot benefit from exploration to make (perfect) rational decisions.
* b. Yes.

> data V2Action =
>     V2Suck
>   | V2Left
>   | V2Right
>   | V2Up
>   | V2Down
>   deriving (Show, Eq, Enum, Bounded)
> 
> data V2Floor = 
>     V2Clean
>   | V2Dirty
>   | V2Obstacle
>   deriving (Show, Eq, Enum, Bounded)
> 
> type V2Size = (Int,Int)
> type V2Loc = (Int,Int)
> type V2Square = (V2Loc,V2Floor)
> 
> data V2Env = V2Env
>   { v2Priori :: [V2Square]
>   , v2Size :: V2Size
>   , v2Loc :: V2Loc }
>   deriving (Show, Eq)
  
> v2LookupPrior :: V2Env -> V2Loc -> V2Square
> v2LookupPrior (V2Env priori (w,_) _) (x,y) = priori !! (w * y + x)
> 
> v2UpdatePrior :: V2Env -> V2Square -> [V2Square]
> v2UpdatePrior (V2Env priori (w,_) _) sq@((x,y), flr) = let idx = w * y + x
>                                                        in take idx priori ++ [sq] ++ drop (idx + 1) priori

> v2PerformanceMeasure :: (Num a) => V2Env -> a
> v2PerformanceMeasure env = let measure V2Dirty    = (-1)
>                                measure V2Obstacle = 0 
>                                measure V2Clean    = 1
>                            in (sum . map (measure . snd) . v2Priori) env
 
> -- | Takes in width and height into a randomly generated environment
> mkV2Env :: Int -> Int -> IO V2Env
> mkV2Env w h = do let randflr = randEnum (minBound, maxBound)
>                      idxs = [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)]]
>                      zipper i mf = mf >>= \f -> return (i,f)
>                      mrows = zipWith zipper idxs (repeat randflr)
>                  priori <- sequence mrows
>                  let openloc = (map fst . filter ((/= V2Obstacle) . snd)) priori
>                  idx <- randomRIO (0, length openloc - 1)
>                  let loc = openloc !! idx
>                  return (V2Env priori (w,h) loc)

Simple Reflex Randomized Vacuum Agent

> srrvaProgram :: V2Square -> IO V2Action
> srrvaProgram per = let st = per -- | Interpret input
>                        rule = snd st -- | Rule match
>                        act = srrvaRuleAction rule  -- | Rule action
>                    in act -- | Return action
> 
> srrvaRuleAction :: V2Floor -> IO V2Action
> srrvaRuleAction rule = case rule of V2Dirty -> return V2Suck 
>                                     _       -> randEnum (V2Left, V2Down)

> scoreSRRVA :: (Num a) => Int -> V2Env -> IO a
> scoreSRRVA stepcount env = do let steps = foldr1 (>>) (replicate stepcount stepSRRVAEnv)
>                               (_,scores) <- execRWST steps () env
>                               return (sum scores)
> 
> stepSRRVAEnv :: (Num a) => RWST () [a] V2Env IO ()
> stepSRRVAEnv = do env@(V2Env priori size loc) <- get
>                   let per = v2LookupPrior env loc
>                   act <- (io . srrvaProgram) per
>                   let env' = applyV2Action env act
>                   put env'
>                   tell [v2PerformanceMeasure env']
> 
> v2ValidLoc :: V2Env -> V2Loc -> Bool
> v2ValidLoc env@(V2Env _ size@(w,h) _) loc@(x,y) =    x >= 0 && x < w
>                                                   && y >= 0 && y < h 
>                                                   && snd (v2LookupPrior env loc) /= V2Obstacle
> 
> applyV2Action :: V2Env -> V2Action -> V2Env
> applyV2Action env@(V2Env priori size loc@(x,y)) act = let flr = snd (v2LookupPrior env loc)
>                                                           loc' = case act of V2Suck  -> loc
>                                                                              V2Left  -> (x - 1, y    )
>                                                                              V2Right -> (x + 1, y    )
>                                                                              V2Up    -> (x    , y - 1)
>                                                                              V2Down  -> (x    , y + 1)
>                                                           loc'' = if v2ValidLoc env loc' then loc' else loc
>                                                           flr' = if act == V2Suck then V2Clean else flr
>                                                           priori' = v2UpdatePrior env (loc'', flr')
>                                                       in V2Env priori' size loc''

> testV2Envs :: IO [V2Env]
> testV2Envs = sequence [mkV2Env w h | (w,h)<-replicate 10 (10,10)]

> avgSRRVAScore :: [V2Env] -> Int -> IO Float
> avgSRRVAScore envs stepcount = do let size = (fromIntegral . length) envs
>                                   scores <- (sequence . map (scoreSRRVA stepcount)) envs
>                                   return ((sum scores) / size)

> v2PrintEnv :: V2Env -> IO ()
> v2PrintEnv (V2Env priori (w,_) loc) = do let shw V2Clean    = '.'
>                                              shw V2Dirty    = 'o'
>                                              shw V2Obstacle = 'X'
>                                              rows = (unlines . chunksOf w . map (shw . snd)) priori
>                                          putStr rows
>                                          print loc

* c.

> -- | Trapped vacuum agent by obstacles in the top left corner
> v2PoorScoringEnv :: V2Env
> v2PoorScoringEnv = V2Env priori size loc
>   where size = (10,10)
>         loc  = (0,0)
>         flrs = V2Clean : V2Obstacle : (replicate 8 V2Dirty)
>                ++ [V2Obstacle] ++ (replicate 89 V2Dirty)
>         priori = zip [(x,y) | y<-[0..9], x<-[0..9]] flrs
> 

* d. Yes.

> rsvaProgram :: V2Square -> StateT (V2Loc, [V2Loc], Maybe V2Action) IO V2Action
> rsvaProgram per = 
>   do (lastloc, lochist, mlastact) <- get
>      let st = per -- | Update State
>          rule = snd st -- | Rule match
>          act = rsvaRuleAction rule
>      put (lastloc, lochist, mlastact)
>      io act -- | Return action
> 
> rsvaRuleAction :: V2Floor -> IO V2Action
> rsvaRuleAction rule = case rule of V2Dirty -> return V2Suck 
>                                    _       -> randEnum (V2Left, V2Down)




__2.12__

__2.13__

