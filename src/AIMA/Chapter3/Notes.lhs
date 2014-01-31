3 - Solving Problems by Searching
=================================

> module AIMA.Chapter3.Notes where
> import Control.Monad.RWS
> import Control.Monad.Fix

This chapter is about the problem-sovling agent, a goal based agent which represents the environment as atomic,
and solving with uninformed and informed search algorithms each producing a sequence of actions.

3.1 Problem Solving
-------------------

* Having a goal organizes an agent's behavior towards __goal formulation__. A clear goal produces a set of states which the agent pursues.
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

* A __search tree__ is a tree respresenting searchable paths where each point is a __node__.
* The __generation__ process of exploring possibly new nodes is called __expanding__. 
  A validated new node is a __child node__ to the expanded node, the __parent node__.
  The child node is added to the __frontier__ (__open list__) as it is unexplored.
* The __search strategy__ determines how the frontier nodes are dealt.
  A good strategy avoids __repeated state__, __loop paths__, and __redundant paths__.
* An algorithm that checks against its __explored set__ (__closed list__) will avoid researching nodes. 

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

**3.3.2 Measuring problem-solving performance**

Completeness and optimality depend on the defintion of the math function.

* __Completeness__: If there exists a solution, will the algorithm find it?
* __Optimality__: Does the stragtey find the best, possible solution?

Time and space complexity depend on the defintion of the math function, software program, and the machine architecture.

* __Time complexity__: How long does the algorithm take to find a solution?
* __Space complexity__: How much memory space does it consume?

Defintions arising from these complexities include:

* The __branching factor__ expresses the quantity of sibling nodes.
* The __depth factor__ expresses the quantity of child nodes.
* The __search cost__, a synonym for time (and space) complexity, added with the path cost equals the __total cost__.

3.4 Uniformed Search Strategies
-------------------------------

* An __uninformed search__ (__blind search__) chooses to expand each node in the frontier as if it has the equal potential to be as close to a goal-state/node.
* An __informed search__ (__hueristic search__) has additonal functions and/or structures to rank and choose better frontier nodes.

**3.4.1 Breadth-first search**

**Breath-first search** expands the frontier giving preference towards the _nearest/shallowest_ node from the inital node. 
This can be visualized as a pebbled dropped into a pond, where the expanding ripple is the frontier.

> {-
> bfs :: Problem -> Maybe Solution
> bfs problem = 
>   let pathcost = 0 
>       node = Node (initState problem) pathcost
>       frontier = [node] -- FIFO queue
>       explored = [] -- set
>       expanderAndSelector n e action f =
>         let child = nodeChild problem n action
>                     childstate = nodeState child
>         in if and (map (notElem childstate) [f,e])
>               then if (goalTest problem) childstate
>                       then Left (solution child)
>                       else Right (addNode f child)
>               else Right f
>   in if (goalTest problem) (nodeState node)
>         then Just (solution node)
>         else ($ (explored, frontier)) $ fix $
>                \loop (e,f) ->
>                  if empty f
>                     then Nothing
>                     else let (node, f') = pop f
>                              e' = addNode e node
>                              actions = (problemActions problem) (nodeState node)
>                              expansion = foldr (expanderAndSelector node e') (Right f') actions
>                          in either Just (loop . (,) e') expansion
> -}

* Completeness: True.
* Optimal: False.
           BFS returns the first validated path from expansion, which may not be optimal path because the expanded node may have a lower path-cost from another node.
           If every node's path-cost and step is equal, then the resulted path is always optimal.
* Time complexity: _Θ(b^(d+1))_
* Space complexity: _Θ(b^d)_

*Note: __b__ is the number of generated/branch nodes, and __d__ is the depth.*

* Because BFS has expontential complexities, it can consume large amount of memory and time.

**3.4.2 Uniformed-cost search**

> {-
> ucs :: Problem -> Maybe Solution
> ucs problem = 
>   let pathcost = 0 
>       node = Node (initState problem) pathcost
>       frontier = [node] -- priority queue
>       explored = [] -- set
>       expander n e action f =
>         let child = nodeChild problem n action
>                     childstate = nodeState child
>         in if and (map (notElem childstate) [f,e])
>               then addNode f child
>               else fromMaybe f $
>                      child' <- find f child -- maybe the frontier has a previously inserted eqv. child-node
>                      return if (nodePathCost child') > (nodePathCost child)
>                                then addNode f child
>                                else f
>   in if (goalTest problem) (nodeState node)
>         then Just (solution node)
>         else ($ (explored, frontier)) $ fix $
>                \loop (e,f) ->
>                  if empty f
>                     then Nothing
>                     else let (node, f') = pop f
>                          in if (goalTest problem) (nodeState node)
>                                then Just (solution node)
>                                else let e' = addNode e node
>                                         actions = (problemActions problem) (nodeState node)
>                                         expansion = foldr (expander node e') f' actions
>                                     in loop (e', expansion)
> -}


* Completeness: True, when search doesn't infinitely expand over the lowest-cost node using a zero-or-negative stepping-cost.
* Optimal: True.
           UFS returns the lowest-cost validated path,
             which is always an optimal path due to frontier's maintained low-cost node-ordering.
* Time complexity range: [_Θ(b^(d+1))_, _Θ(b^(1+(C*/ε)))_],
                           where C* is the cost of the optimal path and ε is lowest action-cost.
                   The BFS like complexity using _d_ is true when every step is equal.
* Space complexity range: [_Θ(b^d)_, _Θ(b^(C*/ε))]_


**3.4.3 Depth-first search**

* __Depth-first search__ expands the deepest node in the frontier stack (LIFO).
  If the expansion yields nothing,
    then it backtracks the previous frontier.
  It stops when it finds its goal,
    or explored all nodes and fail to find one with a goal state.

* Completeness: True.
* Optimality: False.
              It chooses the first-explored, validated-goal-stated node regardless of its optimality.
* Time/Space complexity: Worst case is _Θ(b^m)_,
                           where the algorithm explores/generates all nodes of the tree with maxmium depth of _m_.

**3.4.4 Depth-limited search**

* Because DFS may explore towards the next-deepen node indefinitely, it can be impracticial for implemenation on a computer with especially small memory.
  __Depth-limited search__ limits the DFS to a maxmium exploration depth.
  A maxmium-depth node in DLS is treated/equivalent to a node with no expansion nodes in DFS.

* Completeness: False.
                The first possible goal-stated node may be out of reach of the depth-limit.
* Optimality: False.
              It chooses the first-explored, validated-goal-stated node regardless of its optimality.
* Time/Space complexity: Worst case is _Θ(b^l)_,
                           where the algorithm explores/generates all nodes of the tree with a discrete, maxmium depth of _l_.

> {-
> dls :: Problem -> Integer -> Maybe Solution
> dls problem limit = recursiveDLS (mkNode (initState problem)) problem limit
>  
> recursiveDLS :: Node -> Problem -> Limit -> Maybe Solution
> recursiveDLS node problem limit = 
>   if (goalTest problem) (nodeState node)
>      then Just (solution node)
>      else if limit == 0
>              then Nothing
>              else let actions = (problemActions problem) (nodeState node)
>                   in foldr (deepen node) Nothing actions
>   where
>     deepen _ _      (Just res) = Just res
>     deepen n action Nothing =
>       do let child = childNode problem n action
>          recursiveDLS child problem (limit - 1)
> -}

* Using the max number of steps/depth, when it is known in advance, defines the __diameter__ of the state space.

**3.4.5 Iterative deepening depth-first search**

> {-
> ids :: Problem -> Maybe solution
> ids problem = (head . dropWhile isNothing) (map (dls problem) [0..])
> -}

* __Iterative deepning search__ recursively calls DLS while increasing the search depth limit until the goal state is found.
  The benefit is that if a goal-state is within a certain number of steps,
    then it may be before DFS.
  Because some state-spaces may be infinitely or extremely large, the search effort may never backtrack.
  This is a problem when a goal-state is on sibling node that is never explored.
  ILS fixes this by acting as a pseudo BFS in which the nodes on the depth limit represents the frontier.
  ILS is useful when BFS is needed by a agent(s) which can't multilocate.


* Completeness: True.
* Optimality: False.
              It chooses the first-explored, validated-goal-stated node regardless of its optimality.
              It's only optimal when all stepping costs are equal.
* Time complexity: Θ(b^d)
* Space complexity: Θ(b*d)

* __Iterative lengthening search__ is similar to IDS, but it uses a path-cost as the 'limit' instead of depth/steps.
  IDS is to ILS as BFS is to UCS.

**3.4.6 Bidirectional search**

* A __bidirectional search__ has two searches from the start to the goal-search-paths and from the goal to the start-search-paths. 
  Obviously, the start and goal node must be known in advance,
    but with the benefit of reducing the necessary depth from the start node.

* Completeness: True.
* Optimality: False.
              It chooses the first, linked paths regardless of its optimality.
              The path is only optimal when all stepping costs are equal and when both directional searches are BFS.
* Time complexity: Θ(b^(d/2))
* Space complexity: Θ(b^(d/2))


**3.4.7 Comparing uniformed search strategies**

_Look at the completeness, time and space complexities, optimality above._

3.5 Informed (Heuristic) Search Strategies
------------------------------------------

* __Informed search__ is class of search strategies which uses an externally-defined, informative guidance while searching.
  With the right guidance, informed searches likely outcompete their uninformed search counter-parts.

* __Best-first search__, a general type of informed search, that uses __evaluation function__ _f(n)_ for cost-estimation and typically a __huersitic function__ _h(n)_.

**3.5.1 Greedy best-first search**

* A __greedy best-first search__ uses a hueristic function to find and expand the closest/nearest node.
  Use a __straight-line distance__ hueristic for search problems related to traveling distances.

**3.5.2 A-Star search: Minimizing the total estimate solution cost**

* __A-Star search__, which uses _f(n) = g(n) + h(n)_,
    _f(n)_ is the evaluation func which equals the sum of _g(n)_, the step-cost func to each node n, and _h(n)_, the heuristic func for each node.

__Conditions for optimality: Admissibility and consistency__

* An __admissible heuristic__ never overestimates the cost to reach a goal node.
* To measure __consistency__ (__monotoncity__) of a heuristic function with the __triangle inequality__,
    _h(n) <= c(n,a,n') + h(n')_. _c(n,a,n')_ is the consistency function.

__Optimality of A*__

* __Contours__ - topographical lines which split by the _f(n)_ values between the nodes.
* __Pruned__ - eliminating the node from expansion with examination.
* __Optimally efficent__ - an algorithm which chooses the best path with efficiency. 

* The difference between the actual-cost and the heuristic-cost is the __absolute error__ (__relative error__).

**3.5.3 Memory-bounded heuristic search**

**3.5.4 Learning to search better**

3.6 Heuristic Functions
-----------------------

Two common heuristics for the 8-puzzle:
* h1: the number of misplaced tiles
* h2: the sum of the distance, such as the __Manhattan distance__, from the tile's correct location.

**3.6.1 The effect of huristic of accuracy on performance**

**3.6.2 Generating admissible heuristics from relaxed problems**

**3.6.3 Generating admissible heuristics from subproblems: Pattern databases**

**3.6.4 Learning heuristics from experience**

3.7 Summary
-----------

Bibliographical and Historical Notes
------------------------------------ 

