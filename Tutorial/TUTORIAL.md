## Introduction

This repo is based on a new mathematical foundation of economic game theory. We developed the theory in the last years. The most relevant articles are:

* [Compositional game theory](https://arxiv.org/abs/1603.04641) by Ghani, Hedges, Winschel and Zahn
* [Bayesian open games](https://arxiv.org/abs/1910.03656) by Bolt, Hedges and Zahn

See also Jules Hedges' [webpage](https://julesh.com/2017/11/09/compositional-game-theory-reading-list/) which contains further resources. 

The essential innovation is the decomposition of strategic interactions into atomic components. These components can be assembled through a fixed set of operations while guaranteeing that the result is well-behaved -- i.e. forms a proper *open game*.

The software has two main components: (i) it enables users to represent strategic interactions and (ii) it enables them to analyze these games, typically checking whether a particular strategy is in equilibrium. 

NOTE: In its current form, compositional game theory is essentially a programmable reincarnation of classical game theory. The models you can generate with it can also be generated with classical game theory. What is different is *how* one approaches that modelling task: it becomes a programming task.

This has two main consequences:  

Hence: What we get here is mostly good ol' game theory -- but in a very different form. If you expect a "different" kind of theory, you are (for now) looking in the wrong place. If you are looking for "new" theory (i.e. proving novel theorems), you are also in the wrong place. 

### Intended Audience - required background knowledge

This tutorial is directed towards _applied modellers_ who want to use the software for modelling of concrete situations. We do not give a detailed explanation how the engine works. If you are interested in that, please check out ...

Some knowledge of Haskell is helpful as we are implementing some auxiliary stuff (like utility functions) in Haskell. However, this is mostly about defining functions which should be not too hard to grasp even without knowledge of Haskell. 

### Current Limitations

* Focus of the engine is on BayesianGames
* Repeated games are not possible (but Markov games will be soon)
* This engine is not (yet) optimized for performance

Progress on all of these fronts is happening. But we are not there yet. 

## Representation of Games

### Basic structure of an _open game_

The theory of open games is developed in a particular category theoretic structure which allows representations in terms of _string diagrams_. These are essentially two-dimensional diagrammatic representations. 

![Basic Game](GameSimple.png)

Essentially, all elements in a strategic interaction are translated into computations. These computations are two-way information transformers. They take information from the left (X) and from the right (R) as inputs and transform them into outputs - also in both directions (Y and S). 

At a high level, the modelling task is to plug these components together such that the assembled object represents the intended interaction. You can think of this modelling as "plumbing together" the information flow of an interaction.  Here is a simple example of a simultaneous move game with two players -- essentially a standard bimatrix game like the Prisoner's Dilemma.

![Bimatrix](Bimatrix.png)

Here is an example of a sequential game with two players. That is, one player moves first, the second player observes the first player's move and then makes a decision.

![Sequential](Sequential.png)

And lastly, here is an example of an auction where the two bidders have private information about their evaluation for the good.

![Auction](Auction.png)

We will not dwell much on the technical details behind these graphical representations. What is important though (and what you should take away from the pictures at this stage): The graphical language operates in two dimensions. In the bimatrix case players 1 and 2 are put side-by-side as they move simultaneously. They are still contacted though as their utility is affected not only by their own action but also by the action of the other player.

Obviously, there is still significant information missing such as what exactly are the strategies the players can choose? What are the payoffs etc? 

We will come to that. 

So far it is important to note, there is a way of stitching together the different components which mainly works along two time dimensions: simultaneous and sequential composition.  

### Types

You may wonder what the restrictions are when we want to plug games together. Obviously, arbitrary combinations do not make much sense. The solution to this are _types_. If you do not know about what types are, you can think of some as a tag to each wire. The tag tells us what kind of object we are dealing with. Only equally-tagged games can be connected.

These tags are important as they also play a major role in programming games. In fact, our software will prevent you from composing games where the tags do not match. You will get what is called a type error. 


### Programming syntax

The graphical representation is useful as one gets a quick perspective on what the information actually is. Moreover, turning towards our actual goal, namely program these interactions, it is important to note one challenge that we face (and which will pervade throughout the programming): The graphical syntax is two-dimensional but like most programming languages, our code most be one-dimensional. 

Hence, we need a way to flatten the two dimensions into one. We achieve this by cutting the two dimensions into parts, sequentialize these components, and connect them through variables. Bear this in mind; hopefully this makes it easier to grasp what is going on.


The syntax for programming has two parts, an _outside perspective_ and an _inside perspective_. Let us begin with the outside perspective. 

        _NameOfGame_ _var1_ _var2_ ... = [opengame|
            
            inputs    : _InputGame_ ;                    -- This corresponds to X in the diagram
            feedback  : _FeedbackGame_ ;                 -- This corresponds to S in the diagram
            :-------------------------:
            
            INTERNALS OF THE GAME
            
            :-------------------------:
            output    : _OutputToOutside_ ;              -- This corresponds to Y in the diagram
            returns   : _ResultsReceivedFromOutside_ ;   -- This corresponds to R in the diagram
            
        |] 

Think of the declaration of an open game as defining a (possibly constant) function. That is, on the left hand side of the equation you provide the name of the game `_NameOfGame_` as well as possible arguments that the game depends on `_var1_ ,...`. Think of the arguments as exogenous parameters on which the game depends such as utility functions, cost functions, discount factors etc. 

Regarding the right hand side of the equation, the `[opengame| ... |]` is the syntax that contains the relevant information to define an open game. The internals are seperated from the externals through two separating lines `:---:`. 

For now, we focus on the external parts. Recall the diagram above. The outside perspective corresponds to having the box with 4 wires. Hence, we need to provide inputs/outputs. (Information to be inputted is depicted in _..._). In many cases this will boil down to assign variable names to make clear on which information a game does depend on.

Let us now turn to the internal structure. It has the following shape:


        :-------------------------:

        inputs    : x ;                                                  --\
        feedback  : f ;                                                     \
        operation : dependentDecision _playerName_ (\y -> actionSpace) ;     ==> Line 1 
        outputs   : y ;                                                     / 
        returns   : payoffFunction x y (...) ;                           --/

        inputs    : a ;                                                  --\
        ...                                                                 \
        ...                                                                  ==> Line 2
        ...                                                                 /
        ...                                                              --/
        
        ...
       
        :-------------------------:

The internals of an open game consists of one or more _Lines_. Each line consists of five fields. 
It is not accidental that the internal structure closely resembles the outer structure. There are also four input/output fields. 

NOTE: For the compiler to work, the order of the fields must be kept intact. For instance, `operation` has to preceed `returns`. However, fields are optional. That is, if, for instance, there is no input, one ignore that field.

Recall that our goal is to model each component as a possible standalone open game which can be connected.  
The main difference between the outer and inner layer is that the each Line requires an operation field. This is where information is actually transformed or generated. We will see several canonical operations that one can use. This includes, as depicted in the box above, a decision operator which, given some possible prior observation, and an action space (a list of possible actions to take), and information that is returned (his payoff), models a single agent who chooses an action. 

What action? For now, you can think about the agent maximizing his expected payoffs. In principle, we can also consider other decision criteria. But for simplicity, we will stick to the maximization for now.     

It is crucial to realize that there is a "gap" between the decision operation and the return field. When we speak of maximization of payoffs, the objective function is typically a payoff function. Yet, you may wonder: In a strategic situation the decisions of others matter. Otherwise, what is the whole point, right? 

But as indicated above with `(...)` in the returns field, the payoff function can rely on other inputs from outside that single line, e.g. another player making a move.  

It is your task as a modeller to make this dependency explicit. The compiler in the back then takes care of threading the needed information together. This also gives you a first rule of thumb how to approach modelling in our language. Whenever there needs to be an explicit modelling of decisions, payoffs etc., you will typically model that parts in terms of the internals of a game. 


NOTE: We expect that the syntax above is easier to grasp for people without much background knowledge in certain aspects of Haskell (you tell us!). For those familiar with Arrow Syntax (see, e.g. [here](https://www.haskell.org/arrows/)), there is an alternative way available which we detail in the [LINK BROKEN](#alternative-syntax). 



### Building blocks

After having laid out the basic structure of interal 

What elements do we need?

* decisions

* functions

* nature

* payoff information -> 

TODO find the right abstraction

## Examples

### Decision Problem
### Piping of information flow
### Single decision
### Simultaneous moves
### Sequential moves
### Mixed strategies
### Incomplete information and updating -> single player and auction
### Piping together different modules 

## Analyzing Games

### Overview

After all that work, what can you actually do with a game that is represented? You can analyze it in different ways. 

0. You can check whether a given strategy tuple, that you need to come-up with!, is an equilibrium. You will receive a message like this, if it is:

xxx

and a message like this, if it is not:

xxx

If your game depends on outside parameters, you can check for a whole class of parameters

xxx

1. You can simulate the outcomes for a game given a strategy. And of course you can do all sorts of illustrations with that

xxx Simulate outcome for a specific game

xxx Tabulate outcomes for a game.

2. You can also try to solve a game, which means, given a game, you are trying to find an equilibrium. Now, there are limitations in what you can do here. Hard limitations in the sense that computing equilibria in general is a computationally very hard problem. Weak limitations in the sense that we have spent no (nada!) efforts to optimize the code for performance. This will change in the future but for now keep that in mind.

Here is an example: 



### Supplying strategies

If you want to check for equilibria, you need to supply strategies. Which means that you need to supply a function for each move that can be made. Which means you need to know how to define functions in our system. 

Note, that even if there are no previous moves, no previous information to observe on which a strategy would be conditioned, you still supply a function -- just a constant function. This may look strange at the beginning but in essence it helps to make the system over all more uniform. And do not worry! You will not notice it much because we provide syntactic sugar which allow you to forget about this if you want to. 

NOTE: If you are familiar with game theory, then most what we describe here probably makes sense to you. After all, if we take the Ultimatum Game, what you have to supply for the receiver is a function: given the offer by the sender, should the receiver accept or reject? Similarly, in a Bayesian Game like an auction, if one player observes her type, her strategy is function from her type space into the allowed bids. 

As many teachers of game theory can attest to, for beginning students it is often not so clear that a strategy is really a function 

# Appendix

## Alternative syntax

## Extensions

### Different decision criteria
