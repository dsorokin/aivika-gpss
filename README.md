GPSS-like DSL for AivikaSim

This package allows defining simulation models in terms of a GPPS-like
domain specific language, which can be useful if you are going to
translate your models from GPSS into AivikaSim, for example,
for running the [nested] [1] or [distributed] [2] simulation.

The represented DSL is quite similar, but it is not always equivalent. 
GPSS is trying to be too deterministic when describing the behavior of 
transacts in the same time points, regulating the precise order of that 
how the transacts enter the blocks for the same modeling time. On the contrary, 
AivikaSim imposes less restrictions on the order of computations. Moreover, 
such actions as the releasing of Facility or the leaving of Storage have 
immediate effects. You should not try to seize the same Facility that 
you just released, for example, with another priority, as the effect will 
differ from what you used to see in GPSS.

Therefore, in some models you can receive practically the same results as 
in the GPSS software tools. However, in other models, where the order of 
entering the blocks by transacts is crucial, the results may differ and 
significantly. 

[1]: http://www.aivikasoft.com/aivikasim/aivikasim-branches "aivika-branches"
[2]: http://www.aivikasoft.com/aivikasim/aivikasim-distributed "aivika-distributed"
