
{-
TATYM   TABLE M1,150,20,20

        GENERATE 300,200
        MARK
        SPLIT 1,Route2
        SEIZE MAN1
        ADVANCE 100,20
Block1  MATCH Block2
        ADVANCE 50,5
Merge   ASSEMBLE 2
        RELEASE MAN1
        TABULATE TATYM
        TERMINATE

Route2  SEIZE MAN2
        ADVANCE 110,25
Block2  MATCH Block1
        ADVANCE 70,10
        RELEASE MAN2
        TRANSFER ,Merge 

        GENERATE 100000
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
                spcStopTime = 100000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation IO (Results IO)
model =
  do man1  <- runEventInStartTime newFacility
     man2  <- runEventInStartTime newFacility
     match <- newMatchChain
     stats <- newRef emptySamplingStats

     let orderStream = randomUniformStream (300 - 200) (300 + 200)

     let orders      = streamGeneratorBlock0 orderStream
         orderChain =
           Block (\a ->
                   do t <- liftDynamics time
                      return $ assignTransactValue a (const t)) >>>
           splitBlock [route2] >>>
           seizeBlock man1 >>>
           advanceBlock (randomUniformProcess_ (100 - 20) (100 + 20)) >>>
           matchBlock match >>>
           advanceBlock (randomUniformProcess_ (50 - 5) (50 + 5)) >>>
           merge
         merge =
           assembleBlock 2 >>>
           releaseBlock man1 >>>
           Block (\a ->
                   do t <- liftDynamics time
                      let t0 = transactValue a
                      liftEvent $
                        modifyRef stats $
                        addSamplingStats (t - t0)
                      return a) >>>
           terminateBlock

         route2 =
           seizeBlock man2 >>>
           advanceBlock (randomUniformProcess_ (110 - 25) (110 + 25)) >>>
           matchBlock match >>>
           advanceBlock (randomUniformProcess_ (70 - 10) (70 + 10)) >>>
           releaseBlock man2 >>>
           transferBlock merge
           
     runProcessInStartTime $
       runGeneratorBlock orders orderChain 
  
     return $
       results
       [resultSource "man1" "MAN1" man1,
        resultSource "man2" "MAN2" man2,
        resultSource "stats" "TATYM" stats]
  
main =
  printSimulationResultsInStopTime
  printResultSourceInEnglish
  model specs
