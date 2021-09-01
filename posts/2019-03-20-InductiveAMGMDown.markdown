---
title: Proof Downward Induction AM-GM
tags: mathcs
---



AM-GM Theorem  
$$ (x_1 x_2 .. x_n)^{1/n} \leq \frac{x_1 + x_2 + .. x_n}{n} $$  

### Upward induction step 1  


Base step: n = 1, obvious  
Base step: n = 2, obvious  


$(P(k) \rightarrow P(2k) )$:  

for $k \geq 2$ IH:  
$$(x_1 x_2 .. x_k)^{1/k} \leq \frac{x_1 + x_2 + .. x_k}{k}$$

Prove P(2k) is true:
$$ (x_1 x_2 .. x_{2k})^{1/2k} \leq \frac{x_1 + x_2 + .. x_{2k} }{2k} $$

Thoughts:

* **Shift complexity** from LHS to RHS for IH
  * IH: $(x_1 x_2 .. x_k) \leq (\frac{x_1 + x_2 + .. x_k}{k})^{k}$
  * Goal: $(x_1 x_2 .. x_{2k}) \leq (\frac{x_1 + x_2 + .. x_{2k}}{2k})^{2k}$
* Separate problem to **Subproblem** and **Renaming**
  * (1..2k) to (1..k) and (k+1 ..2k)
  * Renaming
    * ${}^{\times}A : (x_1 x_2 .. x_k)$
    * ${}^{\times}B : (x_{k+1} x_{k+2} .. x_{2k})$ 
    * Goal: ${}^{\times}A \times {}^{\times}B \leq (\frac{{}^{+}A + {}^{+}B}{2k})^{2k}$

* **Apply IH** to the subproblems
  * ${}^{\times}A \leq (\frac{{}^{+}A}{k})^{k}$ 
  * ${}^{\times}B \leq (\frac{{}^{+}B}{k})^{k}$
* Try to **Build towards Goal**(LHS of Goal) using IHs
  * Notice multiplying both IHs works
    * ${}^{\times}A \times {}^{\times}B \leq (\frac{{}^{+}A}{k})^{k} (\frac{{}^{+}B}{k})^{k} = (\frac{{}^{+}A \times {}^{+}B}{k^2})^{k}$

* New Goal: Show $(\frac{{}^{+}A \times {}^{+}B}{k^2})^{k} \leq (\frac{{}^{+}A + {}^{+}B}{2k})^{2k}$  

Get stuck !! How do we show this inequality.

**IMPORTANT TECHNIQUE - IH(2)** meaning induction hypothesis with n=2.

Observe $(\frac{\color{red}{{}^{+}A \times {}^{+}B}}{k^2})^{k}$  

* IH(2): $\color{red}{{}^{+}A \times {}^{+}B \leq (\frac{{}^{+}A \times {}^{+}B}{2})^{2}}$ 

$(\frac{\color{red}{{}^{+}A \times {}^{+}B}}{k^2})^{k} \leq (\frac{1}{k^2})({\color{red}{(\frac{{}^{+}A \times {}^{+}B}{2})^{2}}})^{k} = (\frac{{}^{+}A + {}^{+}B}{2k})^{2k}$

QED:  $(\frac{{}^{+}A \times {}^{+}B}{k^2})^{k} \leq (\frac{{}^{+}A + {}^{+}B}{2k})^{2k}$

Thoughts: Don't fall in to the trap of believing IH should only be used with obvious cases like n-1, n/2   
IH(2) is the crux of this proof.

### Downward induction step 2  

$(P(k) \rightarrow P(k-1) )$:  
After shifting complexity LHS to RHS:

**IMPORTANT** WRITE OUT $x_{n-1}$ instead of just abstracting it into the ellipse.If not you will hit a roadblock when using IH.   
$x_1 + x_2 + .. x_{n}$ is BAD.   
$x_1 + x_2 + ..+x_{n-1}+x_{n}$ is GOOD.   

 
IH: $$ (x_1 x_2 ..x_{n-1} x_n) \leq (\frac{x_1 + x_2 + .. x_{n-1} + x_n}{n})^{n} $$  

Goal: $$ (x_1 x_2 .. x_{n-1}) \leq (\frac{x_1 + x_2 + .. x_{n-1}}{n-1})^{n-1} $$ 


* Find starting point:
  * $(x_1 x_2 .. x_{n-1})$
* Notice similarity of IH LHS $(x_1 x_2 ..x_{n-1} x_n)$ with starting point
  * We can FREELY add one more variable/expression $\framebox{}$ to starting point so we can use IH. How do we Choose?

#### Pause: Reverse meta-guessing possible pathway to solution

Choosing an expression will gives us this IH:
$$(x_1 x_2 .. x_{n-1}\times\framebox{}) \leq (\frac{x_1 + x_2 + .. x_{n-1}+ \framebox{}}{n})^{n} \tag{1}$$


Relating IH back to the Goal.  
$$(x_1 x_2 .. x_{n-1}\times\framebox{}) \leq \framebox{}\times(\frac{x_1 + x_2 + .. x_{n-1}}{n-1})^{n-1}\tag{2}$$

Combined (1) and (2):
$$(x_1 x_2 .. x_{n-1}\times\framebox{}) \leq (\frac{x_1 + x_2 + .. x_{n-1}+ \framebox{}}{n})^{n}\leq \framebox{}\times(\frac{x_1 + x_2 + .. x_{n-1}}{n-1})^{n-1}\tag{3}$$

New Goal by r:  
$$ (x_1 x_2 .. x_{n-1}) \leq (\frac{1}{\framebox{}})(\frac{x_1 + x_2 + .. x_{n-1}+ \framebox{}}{n})^{n} \leq (\frac{x_1 + x_2 + .. x_{n-1}}{n-1})^{n-1}\tag{4}$$

Thoughts:

* **Find syntatic similarity** of the three expression in goal.
  * $x_1 + x_2 + .. x_{n-1}$
* Renaming
  * $A = x_1 + x_2 + .. x_{n-1}$

Renamed Goal:
$$ (x_1 x_2 .. x_{n-1}) \leq (\frac{1}{\framebox{}})(\frac{A + \framebox{}}{n})^{n} \leq (\frac{A}{n-1})^{n-1}$$

Try: $\framebox{} = \frac{A}{n-1}$

* Backenvelop calculations to see where this could go:
  * $\frac{A + \framebox{}}{n} =\frac{A+\frac{A}{n-1}}{n} = \frac{\frac{An-A+A}{n-1}}{n}=\frac{A}{n-1}$ 
  * Looks good

#### Resume: continue proof

Solving Goal:

$$(x_1 x_2 .. x_{n-1}) \leq (\frac{1}{\frac{A}{n-1}})(\frac{A}{n-1})^{n}\leq (\frac{A}{n-1})^{n-1}$$

$$(x_1 x_2 .. x_{n-1}) \leq (\frac{A}{n-1})^{-1}(\frac{A}{n-1})^{n}\leq (\frac{A}{n-1})^{n-1}$$

QED: $$(x_1 x_2 .. x_{n-1}) \leq (\frac{A}{n-1})^{n-1}\leq (\frac{A}{n-1})^{n-1}$$

### Step 3 Combining results from Upwards and Downward induction

QED Combining Upward and Downward induction constitutes a full proof the inequality is true for all naturals.