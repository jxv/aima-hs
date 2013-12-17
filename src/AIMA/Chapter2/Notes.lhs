Chapter 2 - Intelligent Agents
==============================

> module AIMA.Chapter2.Notes where
> import Control.Monad.State
> import Data.Map as M


This chapter gives an abstract introduction to agents.


2.1 Agents and Environments
---------------------------

_Motivation: Define a general model which can be implemented to analyze some given system._

* Within an _environment_, an _agent_ senses, using _sensors_, and reacts, using _actutators_.
* An agent's sensor creates a _percept_ which can be stored in a _percept sequence_. 
  Ideally, the sequence is allowed to be infinitely large.
* An _agent function_, the mathematical description, and an _agent program_, the software implemenation, transforms percepts (and percept sequences) into _actions_.


2.2 Good Behavior: The Concept of Rationality
---------------------------------------------

_Motivation: Define accurate feedback about an agent's choices._

* A rational agent usually performs well when "it gets what it wants."
* _Performance mesaure_ is usually
  So a certain environment may drastically vary the result of an agent's performance measure.

Rationality depends on
- prior knowledge of environment
- available action
- order and size of the percept sequence
- Motivation: 


2.3 The Nature of Environments
------------------------------

