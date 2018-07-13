

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

- estimated from data to maximize fit
- parsimoney of model determined (in part) by their number

#### Fixed Parameters

------

### Discrepancy Function

Difference between **DATA** (from measurements) and **PREDICTIONS** (from model)

- Must yield single numeric value

- Must be continuous

- Goal = minimization

- AKA cost function, error function, objective function

  

##### RMSD

**R**esidual/Root **M**ean **S**quared **D**eviation ("least squares")

- One version of discrepancy function
- No obvious statistical properties

------

### Parameter Estimation Techniques

##### SIMPLEX

- Simplex = set of D+1 interconnected points for arbitrary dimensionality D (2D = triangle, 3D = pyramid)
- Dimensionality = number of parameters

**SIMPLEX** ISSUES

- Inadvisble to use SIMPLEX with kore than 5 parameters (even 2 can be tough...)
- Can only move downhill (can get stuck in local minima)

------

### STEVEN'S LAW

### GCM

Model of categorization

**Categorization**: group things based on properties (e. g. given the age and income of a US citizen, likelihood democrat/republican)



### SIMPLE MODEL OF MEMORY

Model of human memory retreival. Applied to tasks such as serial recall, free recall, probed recall.

- No STM-LTM distinction in model
- Scale-Invariant Memory, Perception and Learning 
- Memories organized in terms of their temporal distances (single point in multi-dimensional space)
- NO trace decay
- The *confusability* of any two items is a function of the **ratio** of their temporal distances



# MAXIMUM LIKELIHOOD ESTIMATION

### Distribution

Assigns probabilities to events

**Binomial** is probability of various discrete events given:

- Prob of occurence
- Number of events 

**MLE**: Maximize the probability of the data given the model

**NOTE**: ***Likelihood*** is not  `p(parameters | data)`



### Continuous Distributions

Defined by probability densities



# BAYESIAN

*p(x|theta) = L(theta|x)*

The real distribution of interest is *p(theta|x)*

#### Bayes Law

![P(A\vert B)P(B)=P(A\cap B)=P(B\vert A)P(A)](https://wikimedia.org/api/rest_v1/media/math/render/svg/59128ff464b3c99087db4fb3712ba8237bd77565)



![P(A|B)={\frac  {P(B|A)P(A)}{P(B)}}](https://wikimedia.org/api/rest_v1/media/math/render/svg/4f2024ac51846888d62e91c5771d1a06512d93b3)



Need to specify our uncertainty with respect to the parameters before seeing the data.

**Uncertainty** is captured by the **prior distribution**.

After seeing the data, our **updated knowledge** is described by a **posterior distribution**.



#### Beta Distribution

In [probability theory](https://en.wikipedia.org/wiki/Probability_theory) and [statistics](https://en.wikipedia.org/wiki/Statistics), the **beta distribution** is a family of continuous [probability distributions](https://en.wikipedia.org/wiki/Probability_distribution) defined on the interval [0, 1][parametrized](https://en.wikipedia.org/wiki/Parametrization) by two positive [shape parameters](https://en.wikipedia.org/wiki/Shape_parameter), denoted by *α* and *β*, that appear as exponents of the random variable and control the shape of the distribution.

| Notation   | Beta(*α*, *β*)                                               |
| ---------- | ------------------------------------------------------------ |
| Parameters | *α* > 0 [shape](https://en.wikipedia.org/wiki/Shape_parameter) ([real](https://en.wikipedia.org/wiki/Real_number)) *β* > 0 [shape](https://en.wikipedia.org/wiki/Shape_parameter) ([real](https://en.wikipedia.org/wiki/Real_number)) |



![Probability density function for the Beta distribution](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Beta_distribution_pdf.svg/325px-Beta_distribution_pdf.svg.png)





### Markov Chain Monte Carlo

Approximates multi-dimensional integrals

**GOAL**: Come up with a graph that has the correct **stationary distribution**



### Signal Detection Theory

**SDT Counts:**

* Hits
* Misses
* False Alarms
* Correct Rejection

**Precision** = 1/variance



# MODEL SELECTION

**Fit isn't everything.** 

Need to account for **flexibility**/**complexity**

---

### Factors that inform complexity:

1. Number of parameters
2. Extent of parameter space
3. Functional form of model (how *wriggly* the model is...average curvature of the likelihood surface)

---

#### Complex models fit NOISE

---

**CROSS VALIDATION**: predicting data that haven't been fit



## Information Criteria

**Deviance** (-2 ln L): measure of information lost between the data and the model

[**Kullback-Leibler**](https://www.countbayesie.com/blog/2017/5/9/kullback-leibler-divergence-explained) measure of information (lost)

### Akaike's Information Criteria

Log-likelihood is a biased measure of **KL** divergence

**AIC = -2 ln L + 2K** (where K is the number of parameters)

**AICc** = better version of AIC



### Bayes Factors

**Rough Interpretation**

1 < BF < 3 = barely worth mentioning

3 < BF < 10 = Substantial

BF > 10 = Strong



### Bayes

Posterior = Likelihood * Prior / Evidence

Evidence = probability of the **data** given the **model**

**Prior** x **Likelihood** determines prediction

---

#### Where do priors come from?

* Theoretical knowledge about parameter
* Inspect prior predictives

---













shifted Weibull vs exGaussian?











