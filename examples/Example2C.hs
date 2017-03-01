
{-
  GENERATE 420,360,,,1
  QUEUE LINE
  SEIZE CLERK
  DEPART LINE
  ADVANCE 300,90
  RELEASE CLERK
  TERMINATE

  GENERATE 360,240,,,2
  QUEUE LINE
  SEIZE CLERK
  DEPART LINE
  ADVANCE 100,30
  RELEASE CLERK
  TERMINATE

  GENERATE 28800
  TERMINATE 1

  START 1
 -}

import Control.Category
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.GPSS
import qualified Simulation.Aivika.GPSS.Queue as Q

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 28800.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation Results
model =
  do line  <- runEventInStartTime Q.newQueue
     clerk <- runEventInStartTime newFacility

     let workerStream1 = randomUniformStream (420 - 360) (420 + 360)
         workerStream2 = randomUniformStream (360 - 240) (360 + 240)

     let workers1 = streamGeneratorBlock workerStream1 1
         chain1   =
           queueBlock line 1 >>>
           seizeBlock clerk >>>
           departBlock line 1 >>>
           advanceBlock (randomUniformProcess_ (300 - 90) (300 + 90)) >>>
           releaseBlock clerk >>>
           terminateBlock

     let workers2 = streamGeneratorBlock workerStream2 2
         chain2   =
           queueBlock line 1 >>>
           seizeBlock clerk >>>
           departBlock line 1 >>>
           advanceBlock (randomUniformProcess_ (100 - 30) (100 + 30)) >>>
           releaseBlock clerk >>>
           terminateBlock

     runProcessInStartTime $
       runGeneratorBlock workers1 chain1 

     runProcessInStartTime $
       runGeneratorBlock workers2 chain2 
  
     return $
       results
       [resultSource "line" "Line" line,
        resultSource "clerk" "Clerk" clerk]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
