3 - Solving Problems by Searching
=================================

> module AIMA.Chapter3.Notes where
> import Control.Monad.RWS

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
> 
> type UpdateState s p = s -> p -> s
> type FormulateGoal s g = s -> g
> type FormulateProblem s g pr = s -> g -> pr
> type Search pr a = pr -> [a]
>   
> type SimpleProblemSolvingAgent p a s g pr m =
>        RWST (UpdateState s p, FormulateGoal s g, FormulateProblem s g pr, Search pr a) [a] ([a], s) m
>  
> simpleProblemSolvingAgentProgram :: (Percept p, Action a, AgentState s, Goal g, Problem pr, Monad m) =>
>                                     p -> SimpleProblemSolvingAgent p a s g pr m ()
> simpleProblemSolvingAgentProgram percept = do
>   (updateState, formulateGoal, formulateProblem, search) <- ask
>   (actions, st) <- get
>   let st' = updateState st percept
>  
>   let goal     = formulateGoal st
>   let problem  = formulateProblem st goal
>   let actions' = search problem 
>  
>   let (action, actions'') = if null actions
>                               then if null actions'
>                                      then ([], [])
>                                      else ([head actions'], tail actions')
>                               else ([head actions], tail actions)
>    
>   tell action >> put (actions'', st')

**3.1.1 Well-defined problems and solutions**

A searchable problem consists of an agent's:
* The __inital__ state where the agent starts.
* A set of __applicable__ __actions__ for any given state.
* A __transistion model__ describing how each action would change the state.
* A __goal test__ function to check if the agent arrived at a successful terminal state.
* The __path cost__ si the total cost of all __step costs__ for a __path__, a given sequence of actions.

* The __state space__ is the collective nodes on a __graph__ where any path can reach.
* An optimal solution has lowest path cost for all possible solutions.

**3.1.2 Formulating problems**

* The aforementioned components represent a model that __abstracts__ out the detail of the problems.
* A valid abstraction works both in the problem's model and its detailed world.

3.2 Example Problems
--------------------

3.3 Searching for Solutions
---------------------------

3.4 Uniformed Search Strategies
-------------------------------

3.5 Informed (Heuristic) Search Strategies
------------------------------------------

3.6 Heuristic Functions
-----------------------

Bibliographical and Historical Notes
------------------------------------

