
{-
CAUTION

This is namely that case when there is a mismatch in the behaviour of 
AivikaSim and GPSS. It happens by the following reasons:

- the modeller tries to release a Facility, change the transact priority 
and seize the same Facility again. Unlike GPSS, AivikaSim executes 
the realising of the Facility immediately, which means that the Facility 
can be captured by another transact before the source transact captures 
the Facility anew. In case of need, this behaviour can change in AivikaSim, 
but there is a more difficult next issue;

- we rely on the time marked in the transact to get the final statistics. 
Therefore, it is very important which of the transacts enters the ASSEMBLE 
block first. Unlike GPSS, AivikaSim reactivates the transacts from 
the preceding GATHER block in unspecified order. Therefore, any transact 
with much greater marked time can enter the ASSEMBLE block first, which 
may decrease the resulting processing time.

In short, this model is too dependent on the order of computations in 
the same modelling time.
 -}
 
{-
TATYM   TABLE M1,450,20,20

        GENERATE ,,,1
Back1   MARK
        SEIZE WORKR
        ADVANCE 8,3
        SPLIT 1,Back1
        RELEASE WORKR
        PRIORITY 1
        GATHER 24

        SEIZE WORKR
        ADVANCE 16,3
        RELEASE WORKR
        ASSEMBLE 12
        TABULATE TATYM
        TERMINATE

        GENERATE 100000
        TERMINATE 1

        START 1
 -}

import Prelude hiding (id)

import Control.Category
import Control.Monad.Trans

import Data.Maybe

import Simulation.Aivika
import Simulation.Aivika.GPSS

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 100000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation Results
model =
  do workr <- runEventInStartTime newFacility
     stats <- newRef emptySamplingStats

     let bottleStream = takeStream 1 $
                        randomUniformStream 0 0

     let bottles     = streamGeneratorBlock bottleStream 0
         bottleChain =
           Block (\a ->
                   do t <- liftDynamics time
                      return $ assignTransactValue a (const t)) >>>
           seizeBlock workr >>>
           advanceBlock (randomUniformProcess_ (8 - 3) (8 + 3)) >>>
           splitBlock [bottleChain] >>>
           releaseBlock workr >>>
           priorityBlock 1 >>>
           gatherBlock 24 >>>
           
           seizeBlock workr >>>
           advanceBlock (randomUniformProcess_ (16 - 3) (16 + 3)) >>>
           releaseBlock workr >>>
           assembleBlock 12 >>>
           Block (\a ->
                   do t <- liftDynamics time
                      let t0 = transactValue a
                      liftEvent $
                        modifyRef stats $
                        addSamplingStats (t - t0)
                      return a) >>>
           terminateBlock

     runProcessInStartTime $
       runGeneratorBlock bottles bottleChain 
  
     return $
       results
       [resultSource "workr" "WORKR" workr,
        resultSource "stats" "TATYM" stats]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
