
{-
TATYM   TABLE M1,200,10,10

        GENERATE ,,,1
Back1   MARK
        SEIZE WORKR
        SPLIT 1 Back1
        ADVANCE 16,3
        RELEASE WORKR
        ASSEMBLE 12
        TABULATE TATYM
        TERMINATE

        GENERATE 10000
        TERMINATE 1

        START 1
 -}

import Prelude hiding (id)

import Control.Category
import Control.Monad.Trans

import Data.Maybe

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS

import Simulation.Aivika.IO

type DES = IO

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 10000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation DES (Results DES)
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
           splitBlock [bottleChain] >>>
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
