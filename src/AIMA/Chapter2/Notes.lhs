Chapter 2 - Intelligent Agents
==============================

> module AIMA.Chapter2.Notes where


This chapter gives an abstract introduction to agents.


2.2 Agents and Environments
---------------------------

_Motivation: Define a general model which can be implemented to analyze some given system._

* Within an _environment_, an _agent_ senses, using _sensors_, and reacts, using _actutators_.
* An agent's sensor creates a _percept_ which can be stored in a _percept sequence_. 
  Ideally, the sequence is allowed to be infinitely large.
* An _agent function_, the mathematical description, and an _agent program_, the software implemenation, transforms percepts (and percept sequences) into _actions_.


2.2 Good Behavior: The Concept of Rationality
---------------------------------------------

_Motivation: Define rational decision making from a subjective perspective._

* A _rational agent_ will choose actions based on current knowledge and feedback, with an emphasis on the process, the how.
* Typically, a good _perform measure_ quantifies if agent gets what it wants, with an emphasis on the end result, the what.
  Different environments may drastically vary the result of an agent's performance measure, regardless of process's quality.

Rationality depends on:
* quality of the performance measure 
* prior knowledge of environment
* available actions
* percept sequence

2.3 The Nature of Environments
------------------------------

