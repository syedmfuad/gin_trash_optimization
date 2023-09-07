# gin_trash_optimization
This work shows that combustion of cotton gin trash into syngas using a gas turbine and a generator set can produce electricity and ammonia via electrolysis profitably. Using hourly electricity prices, monthly ammonia prices and annual gin trash data, we simulate distributions across a nine-month season and calculate annual, and cumulative profit and return on investment over a 12-year period. Our model is flexible and allows for month-to-month operational decision-making. 

## optimization.R 
Code for operations model to optimize profit for a given installed capacity

$`MAX P_p E_p + P_{UB} E_{UB} + P_{LB} E_{LB} + P_M M - 5.5(E_p + E_{UB} + E_{LB} + E_M) - 130.34*M - 37645*ME`$ $`- 0.100385*[640000+\frac{4000000}{1.2*C+5}]*C + 10*GW_f`$ 

Where: 
* $`P_p`$ is peak electricity price; $`E_p`$ is the MWe of electricity sold each month at peak prices;	
* $`P_{UB}`$ is sub peak electricity price; $`E_{UB}`$ is the MWe of electricity sold each month at subpeak prices; 
* $`P_{LB}`$ is the price of base electricity; $`E_{LB}`$ is the MWe of electricity each month at base prices; 
* $`P_M`$ is the price of ammonia, M; $`E_M`$ is electricity in MWe required to produce M (11 is needed to produce every ton of ammonia, M); 
* ME is the number of ammonia processors, which ranges from 0 to 2;	
* C is installed power capacity, which ranges from 1-5 MWe for the small gin and 1-9 MWe for medium gins;
* $`GW_f`$  is gin waste sold as feed


$`MAX P_p E_p + P_{UB} E_{UB} + P_{LB} E_{LB} + P_M \frac{E_M}{11} + 10*GW_f - 5.5(E_p + E_{UB} + E_{LB} + E_M) - 130.34*\frac{E_M}{11} - 0.100385*[640000+\frac{4000000}{1.2*C+5}]*C - 37645*ME`$ 

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

## plot.R
Plotting code 

## codes_extra summary figure.R 
Summary plot code 
