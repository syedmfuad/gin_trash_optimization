# gin_trash_optimization
This work shows that combustion of cotton gin trash into syngas using a gas turbine and a generator set can produce electricity and ammonia via electrolysis profitably. Using hourly electricity prices, monthly ammonia prices and annual gin trash data, we simulate distributions across a nine-month season and calculate annual, and cumulative profit and return on investment over a 12-year period. Optimization iterates over 10,000 years. Our model is flexible and allows for month-to-month operational decision-making. 

## optimization.R 
Code for operations model to optimize profit for a given installed capacity

Revenue: $`P_p E_p + P_{UB} E_{UB} + P_{LB} E_{LB} + P_M M + 10*GW_f`$

Marginal cost: $`5.5(E_p + E_{UB} + E_{LB} + E_M) + 130.34*M`$

Fixed cost: $`37645*ME + 0.100385*[640000+\frac{4000000}{1.2*C+5}]*C`$ 

$`MAX P_p E_p + P_{UB} E_{UB} + P_{LB} E_{LB} + P_M M - 5.5(E_p + E_{UB} + E_{LB} + E_M) - 130.34*M - 37645*ME`$ $`- 0.100385*[640000+\frac{4000000}{1.2*C+5}]*C + 10*GW_f`$ 

Where: 
* $`P_p`$ is peak electricity price; $`E_p`$ is the MWe of electricity sold each month at peak prices;	
* $`P_{UB}`$ is sub peak electricity price; $`E_{UB}`$ is the MWe of electricity sold each month at subpeak prices; 
* $`P_{LB}`$ is the price of base electricity; $`E_{LB}`$ is the MWe of electricity each month at base prices; 
* $`P_M`$ is the price of ammonia, M; $`E_M`$ is electricity in MWe required to produce M (11 is needed to produce every ton of ammonia, M); 
* ME is the number of ammonia processors, which ranges from 0 to 2;	
* C is installed power capacity, which ranges from 1-5 MWe for the small gin and 1-9 MWe for medium gins;
* $`GW_f`$  is gin waste sold as feed


$`MAX P_p E_p + P_{UB} E_{UB} + P_{LB} E_{LB} + P_M \frac{E_M}{11} + 10*GW_f - 5.5(E_p + E_{UB} + E_{LB} + E_M) - 130.34*\frac{E_M}{11}`$ $`- 0.100385*[640000+\frac{4000000}{1.2*C+5}]*C - 37645*ME`$ 

Subject to:

* $`(E_p + E_{UB} + E_{LB} + E_M + GW_f) \leq CGW`$ 
* $`(E_p + E_{UB} + E_{LB} + E_M) \leq 5403*C`$ 
* $`0 \leq E_p \leq 1071`$ 
* $`0 \leq E_{UB} \leq 2432`$ 
* $`0 \leq E_{LB} \leq 1900`$ 
* $`0 \leq M \leq 550`$ 
* $`0 \leq E_M \leq 6050`$ 

## bayesian simulation.R 
Code for bayesian data simulation 

Specify regression model. For example, to simulate cotton gin waste (CGW) from precipitation (PPT), we may use the following model: 

$`CGW = \beta_0 + \beta_1 PPT + \beta_2 PPT^2 + \epsilon`$

where $`CGW`$ is the dependent variable, $`PPT, PPT^2`$ are independent variables, $`\beta_0, \beta_1, \beta_2`$ are regression coefficients to be estimated, and $`\epsilon`$ represents the error term. 
We then perform a Bayesian regression using Gibbs sampling to estimate the posterior distribution of the model coefficients ($`\beta`$) and error variance ($`\sigma^2`$). The idea is to iteratively draw samples from conditional distributions of both parameters. We set a specified number of iterations for the Gibbs sample (11,000) and a burn-in sample (1,000); the burn-in sample is the number of samples that are discarded before the actual Gibbs sampling algorithm begins drawing from the distribution. 

We then enter a loop where in each iteration, $`\beta`$ and $`\sigma^2`$ parameters are alternately updated. For $`\beta`$, a sample is drawn from a multivariate normal distribution based on the covariance matrix and mean values parameters. The mean and covariance matrix for this distribution are calculated based on the current values of the data and a σ starting value of 0.50 . The $`\sigma`$ is subsequently updated by drawing a sample from an inverse gamma distribution, and the updated $`\sigma`$ is used in the next iteration of drawing $`\beta`$; the loop continues until 10,000 samples are drawn after discarding the burn-in samples. These 10,000 samples represent different plausible values for the model's parameters based on the observed data. Finally, new values of the dependent variable are simulated using the sampled $`\beta`$ coefficients. 

Details: Lacombe, D. J. (2022). [Bayesian Regression Tutorial](https://github.com/djlacombe/Bayesian-Regression-Tutorial)

## plot.R
Plotting code 

## codes_extra summary figure.R 
Summary plot code 
