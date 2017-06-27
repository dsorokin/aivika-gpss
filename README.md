GPSS-like DSL for AivikaSim

This package allows defining simulation models in terms of a GPSS-like
domain specific language, which can be useful if you are going to
translate your models from GPSS into [Aivika](http://hackage.haskell.org/package/aivika).

Note that the GPSS-like domain specific language is not equivalent to
the original GPSS language, but it may return very similar results in
some cases, while it can also return quite different results in other cases.

The package implements the most of GPSS simulation blocks, but the main difference
is as follows.

Like GPSS, the package tries to treat the transact priorities properly within each block.
Here it works in a very similar way even for such non-trivial blocks as PREEMPT, GATHER
and ASSEMBLE. But unlike GPSS, the blocks behave independently from each other, where 
the transact priorities are not used when deciding which of the blocks will be activated next.
The order of activating blocks is unpredictable.

The package creates a new discontinuous process for each new transact. Then that process becomes 
fully responsible for processing the transact by the blocks. Therefore, the speed of simulation 
is slower that it could be when implementing the same model but based on using the standard Aivika 
facilities. 

The rough estimation is that this particular package is slower in 5-6 times than the existent GPSS 
simulators in case of sequential simulation. But the point is that you can combine GPSS with 
discontinuous processes, events and even agents within the same model. Moreover, you can use GPSS
in parallel and distributed simulation models. 
