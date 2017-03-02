
{-
        GENERATE ,,,4
Back1   ADVANCE 30,5
        SEIZE OVEN
        ADVANCE 8,2
        RELEASE OVEN
        TRANSFER ,Back1

        GENERATE 2400
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
                spcStopTime = 2400.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation DES (Results DES)
model =
  do oven <- runEventInStartTime newFacility

     let assemblerStream = takeStream 4 $
                           randomUniformIntStream 0 0

     let assemblers = streamGeneratorBlock0 assemblerStream
         chain      = back
         back       =
           advanceBlock (randomUniformProcess_ (30 - 5) (30 + 5)) >>>
           seizeBlock oven >>>
           advanceBlock (randomUniformProcess_ (8 - 2) (8 + 2)) >>>
           releaseBlock oven >>>
           transferBlock back

     runProcessInStartTime $
       runGeneratorBlock assemblers chain 
  
     return $
       results
       [resultSource "oven" "Oven" oven]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
