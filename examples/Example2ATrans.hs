
{-
  GENERATE 18,6
  QUEUE JOEQ
  SEIZE JOE
  DEPART JOEQ
  ADVANCE 16,4
  RELEASE JOE
  TERMINATE

  GENERATE 480
  TERMINATE 1

  START 1
 -}

import Control.Category
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.GPSS
import qualified Simulation.Aivika.Trans.GPSS.Queue as Q

import Simulation.Aivika.IO

type DES = IO

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 480.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation DES (Results DES)
model =
  do joeq <- runEventInStartTime Q.newQueue
     joe  <- runEventInStartTime newFacility

     let clientStream = randomUniformStream (18 - 6) (18 + 6)

     let clients = streamGeneratorBlock0 clientStream
         chain   =
           queueBlock joeq 1 >>>
           seizeBlock joe >>>
           departBlock joeq 1 >>>
           advanceBlock (randomUniformProcess_ (16 - 4) (16 + 4)) >>>
           releaseBlock joe >>>
           terminateBlock

     runProcessInStartTime $
       runGeneratorBlock clients chain 
  
     return $
       results
       [resultSource "joeq" "The Joe queue" joeq,
        resultSource "joe" "Joe" joe]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
