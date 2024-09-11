Multiple Linear Regression
================
2024-09-11

## Assumptions

Assumptions for the model do not depend on the number of covariates. We
have to check the diagnostic graphs to determine whether the fit, the
residual variance and the residual distribution satisfy the assumptions.

## Coefficient Interpretation

### Additive model

For additive model:
$$\mathbb{E}(Y|X_1,X_2,\dots,X_p)=\beta_0+\beta_1X_1+\dots+\beta_pX_p
$$ 1 unit increase in $X_k$ is associated with an average $\beta_k$
change in $Y$ cet.par.

### Interaction model

There are 3 types of interactions:  
1. categorical x categorical  
2. categorical x continuous  
3. continuous x continuous

It is not common to see interaction terms without linear terms.

It is possible in theory to have more than 2 variables interactions but
the interpretation is really hard.
