{-# LANGUAGE ForeignFunctionInterface, ParallelListComp, ScopedTypeVariables #-}

module FFI where

import Common

import Foreign.C
import Foreign
import Graphics.UI.Gtk
import Control.Monad
import Data.IORef
import SQLite (insertRow, upsertRow)
import Database.SQLite hiding (insertRow)

import Graphics.UI.SDL.Mixer

import Control.Concurrent (threadDelay)
import System.Random (randomIO, randomRIO)

foreign import ccall safe "Input.h input" inputC :: FunPtr (CDouble -> CInt -> IO ()) -> FunPtr (CDouble -> IO ()) -> IO ()
foreign import ccall "wrapper" wrapReportProcessed :: (CDouble -> CInt -> IO ()) -> IO (FunPtr (CDouble -> CInt -> IO ()))
foreign import ccall "wrapper" wrapReportRaw :: (CDouble -> IO ()) -> IO (FunPtr (CDouble -> IO ()))

updateWeights netWeightLabel fullWeightLabel tareWeightLabel emptyEntries fullEntries = do
  emptyWeights <- map read `fmap` map ('0':) `fmap` mapM entryGetText emptyEntries
  fullWeights <- map read `fmap` map ('0':) `fmap` mapM entryGetText fullEntries
  labelSetText netWeightLabel (show (sum (fullWeights :: [Double]) - sum (emptyWeights :: [Double])))
  labelSetText fullWeightLabel (show $ sum fullWeights)
  labelSetText tareWeightLabel (show $ sum emptyWeights)



inputThread db activeAxleLabel rawWeightLabel netWeightLabel fullWeightLabel tareWeightLabel indexCurrentlyBeingWeighedRef weightEntriesRef takingWeightingsRef radioWeighAsEmpty = do

  reportRawFun <- wrapReportRaw $ \cd -> postGUIAsync $ labelSetMarkup rawWeightLabel (markSpan [FontSize $ SizePoint 72] $ (show cd ++ " kg"))
--  let reportRawFun cd = postGUIAsync $ labelSetMarkup rawWeightLabel (markSpan [FontSize $ SizePoint 72] $ (show cd ++ " kg"))

  reportFun <- wrapReportProcessed $ \cd ci -> postGUIAsync $ do
--  let reportFun cd ci = postGUIAsync $ do
                 takingWeightings <- readIORef takingWeightingsRef
                 when takingWeightings $
                      do (emptyEntries, fullEntries) <- readIORef weightEntriesRef
                         indexCurrentlyBeingWeighed <- readIORef indexCurrentlyBeingWeighedRef
                         labelSetMarkup activeAxleLabel (markSpan [FontSize $ SizePoint 72] $ case indexCurrentlyBeingWeighed == length emptyEntries of
                                                                                                False -> show (indexCurrentlyBeingWeighed + 1)
                                                                                                True -> "OK")
                         weighAsEmpty <- toggleButtonGetActive radioWeighAsEmpty
                         case length emptyEntries <= indexCurrentlyBeingWeighed of
                           True -> writeIORef takingWeightingsRef False >> writeIORef indexCurrentlyBeingWeighedRef 0
                           False -> let outputElement = (if weighAsEmpty then emptyEntries else fullEntries) !! indexCurrentlyBeingWeighed in
                                    do entrySetText outputElement (show $ realToFrac cd)
                                       when (toEnum (fromEnum ci) == 1) $ 
                                            modifyIORef indexCurrentlyBeingWeighedRef succ

--  mockInputC reportFun reportRawFun


  inputC reportFun reportRawFun

mockInputC fun rawFun = do
  threadDelay 3000000
  d <- fromIntegral `fmap` truncate `fmap` (1000 *) `fmap` (randomIO :: IO Double)
  i <- (\n -> if n > 2 then 0 else 1) `fmap` (randomRIO :: (Int, Int) -> IO Int) (1, 2)
  fun d i
  rawFun d
  mockInputC fun rawFun
