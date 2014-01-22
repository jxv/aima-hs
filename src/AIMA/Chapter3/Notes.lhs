3 - Solving Problems by Searching
=================================

> module AIMA.Chapter3.Notes where
> import Control.Monad.RWS
> import Control.Monad.Fix

This chapter is about the problem-sovling agent, a goal based agent which represents the environment as atomic,
and solving with uninformed and informed search algorithms each producing a sequence of actions.

3.1 Problem Solving
-------------------

* Having a goal organize an agent's behavior towards __goal formulation__. A clear goal produces a set of states which the agent pursues.
* Choosing which action and state to follow while pursuing the goal is the process of __problem formulation__.
  How the agent decides depends on the environment; whether it's unknown, observerable, known, or determinstic.
* After the process of __searching__ for the goal, the agent __executes__ the __solution__ in the environment.
* Because this agent views the world as atomic, it executes the solution while ignoring new percepts. The process is known as __open loop__ amongst control theorists.

> class Percept p
> class Action a
> class AgentState s
> class Goal g
> class Problem pr
 
> type UpdateState s p = s -> p -> s
> type FormulateGoal s g = s -> g
> type FormulateProblem s g pr = s -> g -> pr
> type Search pr a = pr -> [a]
   
> type SimpleProblemSolvingAgent p a s g pr m =
>        RWST (UpdateState s p, FormulateGoal s g, FormulateProblem s g pr, Search pr a) [a] ([a], s) m
  
> simpleProblemSolvingAgentProgram :: (Percept p, Action a, AgentState s, Goal g, Problem pr, Monad m) =>
>                                     p -> SimpleProblemSolvingAgent p a s g pr m ()
> simpleProblemSolvingAgentProgram percept =
>   do (updateState, formulateGoal, formulateProblem, search) <- ask
>      (actions, st) <- get
>      let st' = updateState st percept
>  
>      let goal       = formulateGoal st
>      let problem    = formulateProblem st goal
>      let newactions = search problem 
>  
>      let (action', actions') = if (not . null) actions
>                                   then ([head actions], tail actions)
>                                   else if (not . null) newactions
>                                           then ([head newactions], tail newactions)
>                                           else ([], [])
>      tell action'
>      put (actions', st')

**3.1.1 Well-defined problems and solutions**

A searchable problem consists of an agent's:
* The __inital__ state where the agent starts.
* A set of __applicable__ __actions__ for any given state.
* A __transistion model__ describing how each action would change the state.
* A __goal test__ function to check if the agent arrived at a successful terminal state.
* The __path cost__ is the total cost of all __step costs__ for a __path__, a given sequence of actions.

* The __state space__ is the collective nodes on a __graph__ where any path can reach.
* An optimal solution has lowest path cost for all possible solutions.

**3.1.2 Formulating problems**

* The aforementioned components represent a model that __abstracts__ out the detail of the problems.
* A valid abstraction works both in the problem's model and its detailed world.

3.2 Example Problems
--------------------

A __toy problem__ emphasis the method for solving, and a solution to a __real world problem__ are practial for non-programmers.

**3.2.1 Toy Problems**

__Sliding-block puzzles__, such as the 8-puzzle, are NP-complete and commonly used for testing or demonstrating search algorithms.

* __Incremental formulation__ adds pieces into the correct location.
* __Complete-state formulation__ reorganizes pieces to the correct location.

**3.2.2 Real-world problems**

* __Touring problems__, a path finding problem where the solution doesn't cross previously visited locations, include the __traveling salespoerson problem (TSP)__.
* Positioning components and connections in __VLSI (Very-large-scale integration) layout__ while minimizing constraints.
* Both __automatic assembly sequencing__ and __protein design__ must be implemented in the correct sequence.

3.3 Searching for Solutions
---------------------------

> {-
> treeSearch :: Problem -> Maybe Solution
> treeSearch problem = 
>   ($ (initFrontier problem)) $ 
>      fix \loop f -> 
>             if empty f
>                then Nothing
>                else let (next, f') = chooseNextNode f
>                     in if hasGoalState next
>                           then Just (solution next)
>                           else loop (addNodes f' (expandNode next))
> -}

> {-
> graphSearch :: Problem -> Maybe Solution
> graphSearch problem = 
>   ($ (initFrontier problem, emptyExploredSet)) $ 
>      fix \loop (f, e) -> 
>             if empty f
>                then Nothing
>                else let (next, f') = chooseNextNode f
>                     in if hasGoalState next
>                           then Just (solution next)
>                           else let e' = addNodes e next
>                                    isnew x = notInFrontier f' x && notInExplored e' x
>                                    expanded = filter isnew (expandNode next)
>                                    f'' = addNodes f' expanded
>                                in loop (f'', e')
> -}

**3.3.1 Infrastructure for search algorithms**

* Every node in a search algorithm's tree has: state, a parent, an action, a path-cost

> {-
> childNode :: Problem -> Node -> Action -> Node
> childNode problem parent action =
>   let state = (problemResult problem) (nodeState parent) action
>       pathcost = (nodePathCost parent) + (problemStepCount problem) (nodeState parent) action
>   in Node parent action pathcost
> -}

A search algorithm uses a structure to remember nodes in the frontier:
* A __queue__ stores nodes in an order first-in-first-out (FIFO) where the search algorithm can process them by its strategy.
* A __stack__ is similar to a queue, but it processes the nodes last-in-first-out (LIFO).
* A __priority queue__ is a type of queue where higher priority nodes can be placed closer to the front of the queue on insertion.

* Remember when implementing storage of nodes that representing them in __canonical form__ is ideal.

3.4 Uniformed Search Strategies
-------------------------------

3.5 Informed (Heuristic) Search Strategies
------------------------------------------

3.6 Heuristic Functions
-----------------------

3.7 Summary
-----------

Bibliographical and Historical Notes
------------------------------------ 

