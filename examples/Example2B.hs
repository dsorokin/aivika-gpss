
{-
  MODEL SEGMENT 1

  GENERATE 35,10
  QUEUE JOEQ
  SEIZE JOE
  DEPART JOEQ
  ADVANCE 18,6
  RELEASE JOE
  TERMINATE

  MODEL SEGMENT 2

  GENERATE 60,20
  QUEUE JOEQ
  SEIZE JOE
  DEPART JOEQ
  ADVANCE 10,2
  ADVANCE 18,6
  RELEASE JOE
  TERMINATE

  MODEL SEGMENT 3

  GENERATE 480
  TERMINATE 1

  START 1
 -}

import Control.Category
import Control.Monad.Trans

import Simulation.Aivika
import Simulation.Aivika.GPSS
import qualified Simulation.Aivika.GPSS.Queue as Q

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation Results
model =
  do joeq <- runEventInStartTime Q.newQueue
     joe  <- runEventInStartTime newFacility

     let clientStream1 = randomUniformStream (35 - 10) (35 + 10)
         clientStream2 = randomUniformStream (60 - 20) (60 + 20)

     let clients1 = streamGeneratorBlock0 clientStream1
         chain1   =
           queueBlock joeq 1 >>>
           seizeBlock joe >>>
           departBlock joeq 1 >>>
           advanceBlock (randomUniformProcess_ (18 - 6) (18 + 6)) >>>
           releaseBlock joe >>>
           terminateBlock

     let clients2 = streamGeneratorBlock0 clientStream2
         chain2   =
           queueBlock joeq 1 >>>
           seizeBlock joe >>>
           departBlock joeq 1 >>>
           advanceBlock (randomUniformProcess_ (10 - 2) (10 + 2)) >>>
           advanceBlock (randomUniformProcess_ (18 - 6) (18 + 6)) >>>
           releaseBlock joe >>>
           terminateBlock

     runProcessInStartTime $
       runGeneratorBlock clients1 chain1 

     runProcessInStartTime $
       runGeneratorBlock clients2 chain2 
  
     return $
       results
       [resultSource "joeq" "The Joe queue" joeq,
        resultSource "joe" "Joe" joe]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
