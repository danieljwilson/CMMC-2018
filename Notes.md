# PROGRAMMING

### SOURCING

Can call r files from other files and have it run to load a bunch of functions, for example. 

------

### DEBUGGING

Set break points in R 

Can set breakpoint in outside function by adding `browser()` at the point we want to break in outside code 

------

#### BONEAU

Conclusion: Smaller sample size with higher variance leads to biggest issues. 

- t.test is smarter now (Welch) to deal with unequal variance 

------

# ON MODELS

**Claim**: Can’t understand things without a model. 

- Even an average is a model of the data 

Choice between models based on: 

1. Quantitative comparison 
2. Intellectual judgement

Classes of Models

1. Data description 
2. Process models (explanatory models) 

------

### ERRORS

Errors faster than correct responses: 

- When time pressure and discriminability of stimuli is high 

Errors slower than correct responses: 

- When time pressure relaxed and task is more difficult 

------

#### Bonini’s Paradox

Nothing is gained by a model that is as complex as reality. 

------

### How do People Choose Explanations?

- Acronyms or labels ([Hintzman](https://books.google.fr/books?hl=fr&lr=&id=LQh7AgAAQBAJ&oi=fnd&pg=PA39&dq=hintzman+1991&ots=WwiNOKDUoc&sig=m5s3-4M5z8XEuI-Ig-uiJsJPCHI#v=onepage&q=hintzman%201991&f=false), 1991) 
- People prefer simpler explanations than are warranted by the data ([Lombrozo](https://www.sciencedirect.com/science/article/pii/S0010028506000739), 2007) 
- People fail to detect circularity ([Rips](https://onlinelibrary.wiley.com/doi/abs/10.1207/s15516709cog2606_3), 2002) 
- - Encoding specificity principle 
- People over-estimate their own explanatory grasp ([Rozenblit & Keil](https://onlinelibrary.wiley.com/doi/abs/10.1207/s15516709cog2605_1), 2002) 
- People love neuroscience ([Weisberg et al](https://www.mitpressjournals.org/doi/abs/10.1162/jocn.2008.20040)., 2008) 

------

#### Verbal Theories & Models

Verbal theory ≈ 2^n models 

------

### Modelling Framework

##### SYMBOLIC (e.g. ACT-R) 

- Concepts & features 
- propositions 
- productions 

##### CONNECTIONIST 

Roughly approximates how the brain works. 

- units 
- connection weights 

##### SPATIAL (e.g. SIMPLE, GCM) 

- memory trace = point in (multi-dimensional) space 

------

### Connectionist Models

##### Representation of Items: 

- Localist 
  - each item = single unique unit (grandmother neuron) 
- Distributed 
  - each item = pattern of activation over all units 

------



# PARAMETER ESTIMATION

#### Free Parameters

* estimated from data to maximize fit
* parsimoney of model determined (in part) by their number

#### Fixed Parameters

----

### Discrepancy Function

Difference between **DATA** (from measurements) and **PREDICTIONS** (from model)

* Must yield single numeric value

* Must be continuous

* Goal = minimization

* AKA cost function, error function, objective function

  

##### RMSD

**R**esidual/Root **M**ean **S**quared **D**eviation ("least squares")

* One version of discrepancy function
* No obvious statistical properties

---

### Parameter Estimation Techniques

##### SIMPLEX

* Simplex = set of D+1 interconnected points for arbitrary dimensionality D (2D = triangle, 3D = pyramid)
* Dimensionality = number of parameters

**SIMPLEX** ISSUES

* Inadvisble to use SIMPLEX with kore than 5 parameters (even 2 can be tough...)

* Can only move downhill (can get stuck in local minima)

  







