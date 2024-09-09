Linear regression
================
Amnist.O
2024-09-10

## Assessment: Interpret linear regression

A statistician has analysed correlation between height (cm) and weight
(kg) and found the following linear model: Expected Weight = -25 +
0.5\*Height or
$$\mathbb{E}(\text{Weight}) = -25 + 0.5 \cdot \text{Height}$$

It means  
1. each cm of height increased is associated (not cause) with an average (since it is $\mathbb{E}(Y|X)$) 500 g increased in weight.  
2. expected weight of a 2 meters tall person is $-25 + 0.5 \times 200$ which is 75 kg.  
3. relationship between height and weight is approximately linear
(according to this analysis)

## Assessment: Linearisation

Consider the following model: $$Y=ae^{bX}$$, where $X$ and $Y$ are
observed variables and $a$ and $b$ are coefficients to be estimated.

What is the linearisation of the model?

$$
\begin{aligned}
Y &= ae^{bX} \\
log(Y) &= log(ae^{bX}) \\
&= log(a)+log(e^{bX}) \\
&= log(a)+bX
\end{aligned}
$$

## Assessment: Checking assumptions

What can Residuals vs Fitted plot tell you?

| Pattern                                                           | What the pattern may indicate |
|-------------------------------------------------------------------|-------------------------------|
| Fanning or uneven spreading of residuals across fitted values     | Nonconstant variance          |
| Curvilinear                                                       | A missing higher-order term   |
| A point that is far away from zero                                | An outlier                    |
| A point that is far away from the other points in the x-direction | An influential point          |

[source](https://support.minitab.com/en-us/minitab/help-and-how-to/statistical-modeling/regression/how-to/fitted-line-plot/interpret-the-results/all-statistics-and-graphs/residual-plots/#residuals-versus-fits)