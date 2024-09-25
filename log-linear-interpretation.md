Log-linear interpretation
================
2024-09-24

## Log on the dependent variable

The effect of $x$ on $y$ when $\log(y)=\beta_0+\beta_1x$ are derived
from:

$$
\begin{aligned}
\log(y)&=\beta_0+\beta_1x \\
y &=e^{\beta_0+\beta_1x} \\
\text{when }x \text{ increases by 1}\\
y^*&=e^{\beta_0+\beta_1(x+1)} \\
&=e^{\beta_0+\beta_1x+\beta_1} \\
&=e^{\beta_1}\cdot y \\
\frac{y^*}{y}&=e^{\beta_1}\\
\text{when }\lvert \beta_1 \rvert < 0.1, \space e^{\beta_1} \approx 1+\beta_1 \text{ thus,}\\
\frac{y^*}{y} &=(1+\beta_1):\lvert\beta_1\rvert <0.1
\end{aligned}
$$

Thus, $y$ changes by $e^{\beta_1}$ folds. The value of $e^{\beta_1}$ is
approximately $1+\beta_1$ when $\lvert\beta_1\rvert < 0.1$. Thus, when
$|\beta_1| <0.1$, $y$ changes by $\beta_1$.

## Log on the independent variable

The effect of $x$ on $y$ when $y=\beta_0+\beta_1\log(x)$ are derived
from:

$$
\begin{aligned}
y&=\beta_0+\beta_1\log(x) \\
\text{when }x \text{ increases by 1\%}\\
y^*&=\beta_0+\beta_1\log(1.01x) \\
&=\beta_0+\beta_1\log(x)+\beta_1\log(1.01) \\
&=y+\beta_1\log(1.01) \\
\log(1.01) \approx 0.01 \text{ thus,}\\
y^*-y&=\beta_1\cdot 0.01
\end{aligned}
$$

Thus, $y$ increases by $\log(1.01)\times{\beta_1}$. Noted that if $x$
increases by $a \%$, then $y$ would increases by
$\log(1+a)\times \beta_1$ unit.

## Log on both sides

The effect of $x$ on $y$ when $\log(y)=\beta_0+\beta_1\log(x)$ are
derived from:

$$
\begin{aligned}
\log(y)&=\beta_0+\beta_1\log(x) \\
\text{when }x \text{ increases by 1\%}\\
\log(y^*)&=\beta_0+\beta_1\log(1.01x) \\
&=\beta_0+\beta_1\log(x)+\beta_1\log(1.01) \\
&=\log(y)+\beta_1\log(1.01) \\
\log(y^*)-\log(y)&=\beta_1\log(1.01) \\
\log \left( \frac{y^*}{y} \right)&=\log(1.01^{\beta_1})\\
\frac{y^*}{y}&=1.01^{\beta_1}\\
\text{when }\lvert \beta_1 \rvert < 10, \space 1.01^{\beta_1} \approx 1+0.01\beta_1 \text{ thus,}\\
y^* &=y+0.01\beta_1y:\lvert\beta_1\rvert <10
\end{aligned}
$$

Thus, $y$ changes by $1.01^{\beta_1}$. The value of $1.01^{\beta_1}$ is
approximately $1+0.01\beta_1$ when $\lvert\beta_1\rvert < 10$. Thus,
when $|\beta_1| <10$, $y$ changes by $\beta_1 \%$.
