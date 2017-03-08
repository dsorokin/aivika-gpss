
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
