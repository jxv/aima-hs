Chapter 2 - Intelligent Agents
==============================

> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> module AIMA.Chapter2.Notes where
> import Control.Monad.RWS

This chapter gives an abstract introduction to agents.


2.1 Agents and Environments
---------------------------

_Motivation: Define a general model which can be implemented to analyze some given system._

* Within an __environment__, an __agent__ percieves, using __sensors__, and effects, using __actutators__. This happens in a loop, _environment --percept--> agent --action--> environment_, called a __Perception Action Cycle__.
* An agent's sensor creates a __percept__ which can be stored in a __percept sequence__. 
  Ideally, the sequence is allowed to be infinitely large.
* An __agent function__, the mathematical description, and an __agent program__/__control policy__, the software implemenation, transforms percepts (and percept sequences) into __actions__.


2.2 Good Behavior: The Concept of Rationality
---------------------------------------------

_Motivation: Define rational decision-making from a subjective perspective._

* A __rational agent__ chooses actions based on current knowledge and feedback, with an emphasis on the process, the how.
* Typically, a good __perform measure__ quantifies if agent gets what it wants, with an emphasis on the end result, the what.
  Thus, different environments may drastically vary the result of an agent's calculated performance measure, regardless of agent function and program's quality.

**2.2.1 Rationality**

Rationality depends on:
* quality of the performance measure 
* prior knowledge of environment
* available actions
* percept sequence

**2.2.2 Omniscience, learning, autonomy**

* An __omniscient__ agent always knows how its and an environment's actions effect the environment.
  Impossible to create for reality based agents, but it defines an approachable upper limit.
* To cope for the lack of omniscience, a rational agent __gathers information__ with the intention of __learning__.
  And __exploration__ is especially necessary when an agent is placed in an unknown enviroment. 
* However, an __autonomous__ agent ignores percepts and relies heavily on prior knowledge for its actions
  as if it's a hard coded reflex. This is a bad decision when its unable to adapt to a changing environment.
* A more rational agent learns to adapt to the current environment while decoupling itself from its previous knowledge.
  To contrast, an agent, with no prior knowledge, acts choatically while it learns.
  Or worse, an agent, with no prior knowledge and unable to learn, indefinitely acts chaotically.

2.3 The Nature of Environments
------------------------------

**2.3.1 Specifying the task environment**

* When an environment and an agent both satisfy a clearly described __PEAS__ _(Performance measure, Environment, Actuators, Sensors)_, the environment is a __task environment__.
* The difficultly to satisfy PEAS varies with situation.
  Within a virtual environment, the __software agents__/__softbots__ may have more complex PEAS requirements than some reality-based environments.

**2.3.2 Properites of task environments**

Obversability:
* __Fully obvservable__: Entire state space is known. _(eg. a checkers player)_
* __Partially obvservable__: Parts of the state space is known. _(eg. a scrabble player)_
* __Unobvservable__: None of the state space is known. _(eg. a toaster)_

Agent types:
* __Single agent__: Because there's only one agent, the environment is benign. _(eg. crossword puzzles)_
* __Multiagent__:
  * When agents actively obstruct another agent's goal, it's __competitive__/__adversial__. _(eg. king of the hill)_
  * When agents actively benefit towards another agent's goal, it's __cooperative__. _(eg. tgc's game: journey)_
  * It's possible for an environment to be both competitive and cooperative. _(eg. team sports)_
  * _Note: Communication is usually factored into multiagent environments._
           _With highly competitive environments, randomized behavior with the intention of deceit isn't uncommon._

Action effects:
* __Deterministic__: The outcome of an agent's action in the environment yields predictably. _(eg. playing a checkers move)_
* __Nondeterministic__: The outcome yields one of many results with __uncertainty__. _(eg. typing input on the keyboard)_
* __Stochastic__: Like nondeterministic, except each result has an associated probability. _(eg. rolling a die)_

Percept relations:
* __Episodic__: Each agent experience is independent of prior percepts. _(eg. robot picking out defective jellybeans on a conveyer belt)_
* __Sequential__: All agent experiences are dependent of prior percepts (in the percept sequence). _(eg. a medical diagnosis based on a patient's symptoms along with his/her personal and family medical history)_

Environment changes:
* __Static__: An environment that never changes during an agent's time spent decision-making. _(eg. connect four)_
* __Dynamic__: An environment that consistently changes regardless of the agent. _(eg. driving)_
* __Semidynamic__: An environment that doesn't change over time, but the agent's time management while decision-making reflects into its performance measure score. _(eg. timed chess games)_

State space:
* __Discrete__: All possibly states in space have well defined, black-and-white boundaries. _(eg. Integers: -200, 2, 0, 30, 123^456)_
* __Continuous__: The states have no defined boundaries and instead is fluid-like or a continuum-of-grays. _(eg. Real numbers: -34.121, 0.30552, 10032.3, π)_

Environment rules:
* __Known__: An agent understands the environment's "rules." _(eg. an agent knows the physics engine in its platformer game)._
* __Unknown__:  An agent is ignorant of the "rules" and should probably learn them. _(eg. kindergarteners)_

__Environment generator__, an abstract factory, uses an __environment class__, a prototype, to build test-case environments for testing agent(s).

2.4 The Structure of Agents
---------------------------

* __architecture__, physical computing device with sensors and actuators
  combined with an agent program creates an agent.

**2.4.1 Agent programs**

Table Driven Agent

* For many environments, table driven agents require unrealistic knowledge for all possible percepts and actions.
  Prefer choosing a specialized agent for the specific task rather than a general one.

> class TdAction act where
>   allActions :: [act]
>  
> class TdPercept per where
>   allPercepts :: [per]
> 
> data TableDrivenAgentState per = TableDrivenAgentState { tdPerceptSequence :: [per] }
>  
> class Table tbl where
>   fullTable :: (TdAction act) => tbl act
>   lookupTable :: (TdPercept per, TdAction act) => tbl act -> [per] -> act
> 
> type TableDrivenAgent per tbl act = RWS (tbl act) [act] (TableDrivenAgentState per) ()
>  
> tableDrivenAgentProgram :: (TdPercept per, Table tbl, TdAction act) => per -> TableDrivenAgent per tbl act
> tableDrivenAgentProgram p = do
>   table <- ask
>   TableDrivenAgentState perseq <- get
>   let perseq' = p : perseq
>   put (TableDrivenAgentState perseq')
>   tell [lookupTable table perseq']


**2.4.2 Simple reflex agents**

* __Simple reflex agents__ only choose actions based on the current percept.
  It ignores all previous percepts.
* __Condition-action rule__, fancy way of saying _if <condition> then <act>_ for an agent's reflex behavior.

> data SrState = SrState -- | dummy state...
>  
> class SrPercept per where
>   interpretInput :: per -> SrState
>  
> data SrAction = SrAction -- | dummy action...
> 
> class SrRule r where
>   ruleMatch :: SrState -> [r] -> r
>   ruleAction :: r -> SrAction
>  
> type SimpleReflexAgent rule = RWS [rule] [SrAction] () ()
>  
> simpleReflexAgentProgram :: (SrPercept per, SrRule rule) => per -> SimpleReflexAgent rule
> simpleReflexAgentProgram p = do
>   rules <- ask
>   let st = interpretInput p
>   let rule = ruleMatch st rules
>   let action = ruleAction rule
>   tell [action]

* limited intelligence, present focus
* even worse without sensors
* __randomized__ behavior getting out of a rut

**2.4.3 Model-based reflex agents**

* Agents with __interntal state__ maintain the state by applying new percepts, so the current state always depends on previous percepts.
* A __model-based agent__'s internal state must contain __model(s)__.
  * Understanding how an environment works independently of the agent's presence.
  * Ability to infer how the agent's actions will effect the environment.

 class MbrState s a p m | a p m -> s where
   mbrUpdateState :: s -> (Maybe a) -> p -> m -> s
 
 class MbrRule r s a | s a -> r where
   mbrRuleMatch :: s -> [r] -> r
   mbrRuleAction :: r -> a

 type ModelBasedReflexAgent s m r a = RWS ([r], m) [a] (s, Maybe a)
 
 modelBasedReflexAgentProgram p = do
   (rules, model) <- ask
   (st, maction) <- get
   let st' = mbrUpdateState st maction p model
   let rule = mbrRuleMatch st' rules
   let action = mbrRuleAction rule
   tell [action]


**2.4.4 Goal-based agents**

**2.4.5 Utility-based agents**

**2.4.6 Learning agents**

* Hard coding rules into intelligent machines is often tedious.
  Alternatively, a preferred way is to create a learning machine then teach it.

Necessary to __learning agents__:
  * learning element
  * performance element
  * critic
  * problem generator

* __reward__
* __penalty__


**2.4.7 How the components of agent programs work** 

* __atomic, factored, structured__

* __atomic representation__

* __factored representation__

* __variable__

* __attribute__

* __value__

* __structured representation__

* __expressiveness__


Bibliographical and Historical Notes
------------------------------------

* __controller__

* dynamic programming

* __multiagent systems__

* softbot
