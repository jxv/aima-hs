Chapter 2 - Intelligent Agents
==============================

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

> class Action a
> class Percept p
> type PerceptSequence p = [p]

> class Table t where
>   fullTable :: (Action a) => t a
>   lookupTable :: (Percept p, Action a) => t a -> [p] -> a
> 
> type TableDrivenAgent p t a = RWS (t a) [a] (PerceptSequence p) ()

> tableDrivenAgentProgram :: (Percept p, Table t, Action a) => p -> TableDrivenAgent p t a
> tableDrivenAgentProgram p =
>   do table <- ask
>      perseq <- get
>      let perseq' = p : perseq
>      put perseq'
>      tell [lookupTable table perseq']


**2.4.2 Simple reflex agents**

* __Simple reflex agents__ only choose actions based on the current percept.
  It ignores all previous percepts.
* __Condition-action rule__, fancy way of saying _if (condition) then (act)_ for an agent's reflex behavior.

> class Rule r
> class AgentState s

> type InterpretInput s p = p -> s
> type RuleMatch s r = s -> [r] -> r
> type RuleAction r a = r -> a
>  
> type SimpleReflexAgent s p r a = RWST (InterpretInput s p, RuleMatch s r, RuleAction r a, [r])
>                                       [a]
>                                       ()

> simpleReflexAgentProgram :: (Percept p, Rule r, Action a, AgentState s, Monad m) =>
>                             p -> SimpleReflexAgent s p r a m ()
> simpleReflexAgentProgram p =
>   do (interpretInput, ruleMatch, ruleAction, rules) <- ask
>      let st = interpretInput p
>      let rule = ruleMatch st rules
>      let action = ruleAction rule
>      tell [action]

* The agent's disadvantage is its present focused intelligence, and it's dysfunctional without sensors.
  An reflex agent without sensors should use __randomized__ behavior to avoid getting stuck.

**2.4.3 Model-based reflex agents**

* Agents with __interntal state__ maintain the state by applying new percepts, so the current state always depends on previous percepts.
* A __model-based agent__'s internal state must contain __model(s)__.
  * Understanding how an environment works independently of the agent's presence.
  * Ability to infer how the agent's actions will effect the environment.

> class Model m

> type UpdateState s a p md = s -> (Maybe a) -> p -> md -> s
>  
> type ModelBasedReflexAgent s p md r a = RWST (UpdateState s a p md, RuleMatch s r, RuleAction r a, [r], md)
>                                              [a]
>                                              (s, Maybe a)
  
> modelBasedReflexAgentProgram :: (Percept p, Model md, Action a, AgentState s, Rule r, Monad m) =>
>                                 p -> ModelBasedReflexAgent s p md r a m ()
> modelBasedReflexAgentProgram p =
>   do (updateState, ruleMatch, ruleAction, rules, model) <- ask
>      (st, maction) <- get
>      let st' = updateState st maction p model
>      let rule = ruleMatch st' rules
>      let action = ruleAction rule
>      put (st', Just action)
>      tell [action]

**2.4.4 Goal-based agents**

* __Goals__ are binary: Achieved or Not-Achieved.
* Goal-based agents use goals as an explicitly and internally defined performance measure.

**2.4.5 Utility-based agents**

* __Utility function__ returns the usefulness, __utility__, of an agent's actions.
* A rational utility-based agent in a partially observable and stochastic environment maximizes realistic probabilities or the __expected utility__.

**2.4.6 Learning agents**

* Hard coding rules into intelligent machines is often tedious.
  Alternatively, a preferred way is to create a learning machine then teach it.

__Learning agents__ components:
  * The __learning element__ searches for improvements by creating goals
  * The __performance element__ decides the action by judging percepts for reflexes, comparing goal progress to expectations, and looking for new areas to explore.
  * The __critic__ gives percepts meaning and looks for performance results
  * The __problem generator__ prevents local maxima for goals

* Together the components act as interdependent and improving system of checks-and-balances using outside feeback as rewards and penalties.

**2.4.7 How the components of agent programs work** 

* An __atomic__ component represents a purely functional structure with no internal components.
* A __factored__ component represents a collection of __variables__/__attributes__ of various types each having a __value__.
* A __structured__ component represents a systematic relationship of variables/attributes.
* A component's __expressiveness__ lie on an axis complexty-and-conciseness vs simplicity-and-quantity.

Bibliographical and Historical Notes
------------------------------------

* The concept of an agent derived from __controller__ in control theory. Today, it has evolved from simple/indepedent to complex/interdependent components.
* Dynamic programming derives from agents existing in a partially observable environment.
* __autonomic computing__
* In agent-oriented programming, a __multiagent system__, typically a system of interdependent agents each with independent internal state, serves as a model for autonomic computing.
* The idea of goal-based agents, popular with traditional cognitive psychology, are exhibited through softbot programs.
  Users often recognize the agent's _intentions_ towards its _desires_.
