
{-

MEN     STORAGE 3
NOWON   STORAGE 50

        GENERATE ,,,53
Back1   ENTER NOWON
        ADVANCE 157,25
        LEAVE NOWON
        ENTER MEN
        ADVANCE 7,3
        LEAVE MEN
        TRANSFER ,Back1

        GENERATE 6240
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
                spcStopTime = 6240.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation IO (Results IO)
model =
  do men   <- runEventInStartTime $ newStorage 3
     nowon <- runEventInStartTime $ newStorage 50

     let machineStream = takeStream 53 $
                         randomUniformIntStream 0 0

     let machines = streamGeneratorBlock0 machineStream
         chain    = back
         back     =
           enterBlock nowon 1 >>>
           advanceBlock (randomUniformProcess_ (157 - 25) (157 + 25)) >>>
           leaveBlock nowon 1 >>>
           enterBlock men 1 >>>
           advanceBlock (randomUniformProcess_ (7 - 3) (7 + 3)) >>>
           leaveBlock men 1 >>>
           transferBlock back

     runProcessInStartTime $
       runGeneratorBlock machines chain 
  
     return $
       results
       [resultSource "men" "Men" men,
        resultSource "nowon" "NowOn" nowon]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
