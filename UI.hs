{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module UI where

import SQLite (insertRow)
import Database.SQLite hiding (insertRow)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent
import Control.Monad

import Data.Int
import Data.Tree

import Data.IORef
import Foreign.StablePtr
import Data.Char (isNumber)
import FFI

import Data.List (lookup)
import Data.Maybe (fromJust)

import Data.Time
import System.Locale

import "mtl" Control.Monad.Trans

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Mixer 

justLookup a xs = fromJust $ lookup a xs

castEnum = toEnum . fromEnum

newtype Seller = Seller { sellerName :: String } deriving (Eq, Ord)
data Car = Car { 
      carId :: Int,
      name :: String,
      numAxles :: Int } deriving (Eq)

rowToCar xs =
    case justLookup "name" xs of
      Text name -> case justLookup "numAxles" xs of
                     Int numAxles -> case justLookup "id" xs of 
                                       Int carId -> Car (fromIntegral carId) name (fromIntegral numAxles)

rowToSeller xs =
    case justLookup "name" xs of
      Text name -> Seller name

rowToSector xs =
    case justLookup "name" xs of
      Text name -> name

listStoreAppendMany :: ListStore a -> [a] -> IO Int
listStoreAppendMany s xs =
    last `fmap` mapM (listStoreAppend s) xs


updateSellers db model = do
  listStoreClear model
  content <- execStatement db "select name from sellers"
  case content of
    Left _ -> return ()
    Right rows -> do listStoreAppendMany model (map rowToSeller . head $ rows)
                     return ()
  return model

attachSellerModel view model = do
  comboBoxSetModel view (Just model)
  renderer <- cellRendererTextNew
  
  cellLayoutPackStart view renderer True
  cellLayoutSetAttributes view renderer model $ \row -> [ cellText := sellerName row ]

updateSectors db model = do
  listStoreClear model
  content <- execStatement db "select name from sectors"
  case content of
    Left _ -> return ()
    Right rows -> do listStoreAppendMany model (map rowToSector . head $ rows)
                     return ()
  return model


attachSectorModel view model = do
  comboBoxSetModel view (Just model)
  renderer <- cellRendererTextNew

  cellLayoutPackStart view renderer True
  cellLayoutSetAttributes view renderer model $ \row -> [ cellText := row ]


setUpSellersBox db view = do
  model <- listStoreNew []
  attachSellerModel view model
  updateSellers db model

setUpSectorsBox db view = do
  model <- listStoreNew []
  attachSectorModel view model
  updateSectors db model

updateCarsBox db carsModel sellersBox sellersModel = do
  listStoreClear carsModel
  activeIterM <- comboBoxGetActiveIter sellersBox
  case activeIterM of
    Nothing -> return ()
    Just activeIter -> 
        do activeElement <- listStoreGetValue sellersModel  $ listStoreIterToIndex activeIter 
           content <- execParamStatement db "select id, name, numAxles from cars where owner = :owner" [(":owner", Text $ sellerName activeElement)]
           case content of
             Left _ -> return () -- todo: error management
             Right rows -> do listStoreAppendMany carsModel (map rowToCar . head $ rows)
                              return ()
  return carsModel


setUpCarsBox db view sellersBox sellersModel = do  
  model <- listStoreNew []
  comboBoxSetModel view (Just model)
  renderer <- cellRendererTextNew

  cellLayoutPackStart view renderer True
  cellLayoutSetAttributes view renderer model $ \row -> [ cellText := (name row ++ ", " ++ show (numAxles row) ++ " akselia") ]

  updateCarsBox db model sellersBox sellersModel

doAddCarDialog db dialog mainSellersBox nameEntry axlesEntry ownerBox sellersModel carsModel = do
  resp <- dialogRun dialog
  case resp of
    ResponseUser 2 -> do
              newName <- entryGetText nameEntry
              newAxles <- entryGetText axlesEntry
              ownerIterM <- comboBoxGetActiveIter ownerBox
              case ownerIterM of
                Nothing -> return () -- todo: tell the user they should select something
                Just ownerIter -> 
                    do owner <- listStoreGetValue sellersModel $ listStoreIterToIndex ownerIter
                       r <- insertRow db "cars" [("name", newName), ("owner", sellerName owner), ("numAxles", newAxles)]
                       putStrLn (newName)
                       print r
                       updateCarsBox db carsModel mainSellersBox sellersModel
                       return ()
    _ -> return ()
  entrySetText nameEntry ""
  entrySetText axlesEntry ""
  widgetHide dialog

doAddSellerDialog db dialog nameEntry sellersModel = do
  resp <- dialogRun dialog
  case resp of
    ResponseUser 2 -> 
        do newName <- entryGetText nameEntry
           insertRow db "sellers" [("name", newName)]
           updateSellers db sellersModel
           return ()
    _ -> return ()
  entrySetText nameEntry ""
  widgetHide dialog

doAddSectorDialog db dialog nameEntry sectorsModel = do
  resp <- dialogRun dialog
  case resp of
    ResponseUser 2 -> 
        do newName <- entryGetText nameEntry
           insertRow db "sectors" [("name", newName)]
           updateSectors db sectorsModel
           return ()
    _ -> return ()
  entrySetText nameEntry ""
  widgetHide dialog



setupWeightsDialogs db radioWeighAsEmpty radioWeighAsFull carCurrentlyBeingWeighedRef indexCurrentlyBeingWeighedRef takingReadingsRef tableWeights weightEntriesRef carsBox carsModel = do
  writeIORef takingReadingsRef False
  writeIORef indexCurrentlyBeingWeighedRef 0
  (emptyEntries, fullEntries) <- readIORef weightEntriesRef
  mapM_ widgetDestroy emptyEntries
  mapM_ widgetDestroy fullEntries
  activeIterM <- comboBoxGetActiveIter carsBox
  case activeIterM of
    Nothing -> return ()
    Just activeIter -> 
        do activeElement <- listStoreGetValue carsModel $ listStoreIterToIndex activeIter 
           let axles = numAxles activeElement
           tableResize tableWeights 2 axles
           content <- execParamStatement db "select weight from emptyAxleWeights where carId = :carId order by axleNum asc" [(":carId", Int $ fromIntegral $ carId activeElement)]
           let (weights, doWeighAsEmpty) = case content of
                                             Left _ -> (repeat "0", True)
                                             Right xs -> ((map snd $ map head $ head xs) ++ repeat "0", (length $ head xs) == 0)
           case doWeighAsEmpty of
             True -> toggleButtonSetActive radioWeighAsEmpty True
             False -> toggleButtonSetActive radioWeighAsFull True

           newEmptyEntries <- forM [1..axles] $ \n -> do e1 <- entryNew
                                                         e2 <- entryNew
                                                         entrySetText e1 (weights !! (n - 1))
                                                         entrySetWidthChars e1 5
                                                         entrySetWidthChars e2 5
                                                         tableAttach tableWeights e1 (n-1) n 0 1 [] [] 2 0 
                                                         tableAttach tableWeights e2 (n-1) n 1 2 [] [] 2 0
                                                         return (e1, e2)
           writeIORef weightEntriesRef $ unzip newEmptyEntries
           writeIORef carCurrentlyBeingWeighedRef (carId activeElement)
           widgetShowAll tableWeights


checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice = do
  haveName <- (not . null) `fmap` entryGetText addCarNameEntry
  haveAxles <- (not . null) `fmap` entryGetText addCarAxlesEntry
  haveOwner <- (negate 1 /=) `fmap` comboBoxGetActive addCarOwnerChoice
  widgetSetSensitive addCarAddButton (haveName && haveAxles && haveOwner)

updateTime timeLabel dateLabel = do
  time <- getZonedTime
  labelSetText dateLabel (formatTime defaultTimeLocale "%F" time)
  labelSetText timeLabel (formatTime defaultTimeLocale "%H.%M" time)

sampleNumFromValue (Int i) = fromIntegral i
sampleNumFromValue Null = 0

updateSampleNum db sampleNumEntry = do
  content <- execStatement db "select max(sampleNum) from loads"
  let newNum = case content of
                 Left _ -> 0 -- todo: don't even know how I'm supposed to manage errors here
                 Right xs -> case head xs of
                               [] -> 0 -- no loads in the database -> zero results from max
                               (row:_) -> 1 + (sampleNumFromValue $ snd $ head row)
  entrySetText sampleNumEntry (show newNum)

statRowToTuple xs = 
    case justLookup "seller" xs of 
      Text seller -> 
          case justLookup "fullWeight" xs of 
            Double weight -> (seller, weight)

statRowToQuad :: [(String, Value)] -> ((String, Double), (UTCTime, UTCTime))
statRowToQuad xs = 
    case justLookup "sector" xs of 
      Text sector -> 
          case justLookup "fullWeight" xs of 
            Double weight -> case justLookup "minTime" xs of 
                               Int minTime -> case justLookup "maxTime" xs of 
                                             Int maxTime -> ((sector, weight), 
                                                             (readTime defaultTimeLocale "%s" (show minTime), readTime defaultTimeLocale "%s" (show maxTime)))

updateStatistics db statisticsModel treeviewStatistics radioStatisticsBySeller = do
  treeStoreClear statisticsModel
  bySeller <- toggleButtonGetActive radioStatisticsBySeller
  case bySeller of
    True -> do
      content <- execStatement db "select seller, sum(fullWeight) as fullWeight from loads group by seller"
      case content of
        Left _ -> return ()
        Right xs -> let stats = map statRowToTuple $ head xs in
                    treeStoreInsertTree statisticsModel [] 0 $ Node ("Kaikki: " ++ show (sum . map snd $ stats) ++ " kg") $
                                [Node (name ++ ": " ++ show weight ++ " kg") [] | (name, weight) <- stats]
    False -> do
      content <- execStatement db "select sector, sum(fullWeight) as fullWeight, min(time) as minTime, max(time) as maxTime from loads group by sector"
      case content of
        Left _ -> return ()
        Right xs -> let stats = map statRowToQuad $ head xs in
                    treeStoreInsertTree statisticsModel [] 0 $ Node ("Kaikki: " ++ show (sum . map snd . map fst $ stats) ++ " kg") $
                                            [Node (sector ++ ": " 
                                                   ++ show weight ++ " kg (" 
                                                   ++ formatTime defaultTimeLocale "%F" minTime ++ " - " 
                                                   ++ formatTime defaultTimeLocale "%F" maxTime ++ ")") [] |
                             ((sector, weight), (minTime, maxTime)) <- stats]


setUpStatisticsView view = do
  model <- treeStoreNew []
  treeViewSetModel view model
  column <- treeViewColumnNew 
  renderer <- cellRendererTextNew

  cellLayoutPackStart column renderer True
  cellLayoutSetAttributes column renderer model $ \row -> [ cellText := row ]
  treeViewAppendColumn view column

  return model

setUpLoadsView view = do
  model <- listStoreNew []
  treeViewSetModel view model

  rendererSeller <- cellRendererTextNew
  rendererNetWeight <- cellRendererTextNew
  rendererTime <- cellRendererTextNew

  columnSeller <- treeViewColumnNew
  columnNetWeight <- treeViewColumnNew
  columnTime <- treeViewColumnNew

  cellLayoutPackStart columnSeller rendererSeller True
  cellLayoutPackStart columnNetWeight rendererNetWeight True
  cellLayoutPackStart columnTime rendererTime True

  cellLayoutSetAttributes columnSeller rendererSeller model $ \(s, _, _) -> [ cellText := s ]
  cellLayoutSetAttributes columnNetWeight rendererNetWeight model $ \(_, s, _) -> [ cellText := s ]
  cellLayoutSetAttributes columnTime rendererTime model $ \(_, _, s) -> [ cellText := s ]

  treeViewColumnSetTitle columnSeller "Kuski/isäntä"
  treeViewColumnSetTitle columnNetWeight "Nettopaino"
  treeViewColumnSetTitle columnTime "Aika"


  treeViewAppendColumn view columnSeller
  treeViewAppendColumn view columnNetWeight
  treeViewAppendColumn view columnTime

  return model


doUI = do
  initGUI

  SDL.init [SDL.InitAudio]
  Mixer.openAudio 44100 SDL.AudioS16Sys 2 1024
  db <- openConnection "vaaka.db"
  

  weightEntriesRef <- newIORef ([], [])
  takingWeightingsRef <- newIORef False
  indexCurrentlyBeingWeighedRef <- newIORef 0
  carCurrentlyBeingWeighedRef <- newIORef (negate 1)

  windowXmlM <- xmlNew "vaakaVilja.glade"
  let windowXml = case windowXmlM of
                    Just x -> x
                    Nothing -> error "can't find the interface glade file in the current directory" -- todo: more useful error message?

  win <- xmlGetWidget windowXml castToWindow "window1"
  carsBox <- xmlGetWidget windowXml castToComboBox "boxCars"
  sellersBox <- xmlGetWidget windowXml castToComboBox "boxSellers"
  sectorsBox <- xmlGetWidget windowXml castToComboBox "boxSectors"

  addCarButton <- xmlGetWidget windowXml castToButton "buttonAddCar"
  addCarDialog <- xmlGetWidget windowXml castToDialog "addCarDialog"
  addCarNameEntry <- xmlGetWidget windowXml castToEntry "carName"
  addCarAxlesEntry <- xmlGetWidget windowXml castToEntry "carNumAxles"
  addCarOwnerChoice <- xmlGetWidget windowXml castToComboBox "carOwner"
  addCarAddButton <- xmlGetWidget windowXml castToButton "addCarAddButton"
  
  addSellerButton <- xmlGetWidget windowXml castToButton "buttonAddSeller"
  addSellerDialog <- xmlGetWidget windowXml castToDialog "addSellerDialog"
  addSellerNameEntry <- xmlGetWidget windowXml castToEntry "sellerDialogNameEntry"
  addSellerAddButton <- xmlGetWidget windowXml castToButton "addSellerAddButton"

  addSectorButton <- xmlGetWidget windowXml castToButton "buttonAddSector"
  addSectorDialog <- xmlGetWidget windowXml castToDialog "addSectorDialog"
  addSectorNameEntry <- xmlGetWidget windowXml castToEntry "sectorDialogNameEntry"
  addSectorAddButton <- xmlGetWidget windowXml castToButton "addSectorAddButton"

  tableWeights <- xmlGetWidget windowXml castToTable "tableWeights"
  buttonMeasureWeights <- xmlGetWidget windowXml castToButton "buttonMeasureWeights"
  radioWeighAsEmpty <- xmlGetWidget windowXml castToRadioButton "radioWeighAsEmpty"
  radioWeighAsFull <- xmlGetWidget windowXml castToRadioButton "radioWeighAsFull"

  dryEntry <- xmlGetWidget windowXml castToEntry "dryEntry"
  sampleNumEntry <- xmlGetWidget windowXml castToEntry "sampleNumEntry"

  netWeightLabel <- xmlGetWidget windowXml castToLabel "netWeightLabel"
  fullWeightLabel <- xmlGetWidget windowXml castToLabel "fullWeightLabel"
  tareWeightLabel <- xmlGetWidget windowXml castToLabel "tareWeightLabel"

  dateLabel <- xmlGetWidget windowXml castToLabel "dateLabel"
  timeLabel <- xmlGetWidget windowXml castToLabel "timeLabel"

  buttonRecordLoad <- xmlGetWidget windowXml castToButton "buttonRecordLoad"

  confirmLoadDialog <- xmlGetWidget windowXml castToDialog "confirmLoadDialog"
  confirmSellerLabel <- xmlGetWidget windowXml castToLabel "confirmSellerLabel"
  confirmSectorLabel <- xmlGetWidget windowXml castToLabel "confirmSectorLabel"
  confirmWeightLabel <- xmlGetWidget windowXml castToLabel "confirmWeightLabel"
  confirmDryLabel <- xmlGetWidget windowXml castToLabel "confirmDryLabel"
  confirmSampleNumLabel <- xmlGetWidget windowXml castToLabel "confirmSampleNumLabel"
  confirmDateLabel <- xmlGetWidget windowXml castToLabel "confirmDateLabel"
  confirmTimeLabel <- xmlGetWidget windowXml castToLabel "confirmTimeLabel"
  
  statisticsWindow <- xmlGetWidget windowXml castToWindow "statisticsWindow"
  radioStatisticsBySeller <- xmlGetWidget windowXml castToRadioButton "radioStatisticsBySeller"
  radioStatisticsBySector <- xmlGetWidget windowXml castToRadioButton "radioStatisticsBySector"
  treeviewStatistics <- xmlGetWidget windowXml castToTreeView "treeviewStatistics"
  buttonUpdateStatistics <- xmlGetWidget windowXml castToButton "buttonUpdateStatistics"

  rawWeightLabel <- xmlGetWidget windowXml castToLabel "rawWeightLabel"
  rawWeightWindow <- xmlGetWidget windowXml castToWindow "rawWeightWindow"

  activeAxleLabel <- xmlGetWidget windowXml castToLabel "activeAxleLabel"
  activeAxleWindow <- xmlGetWidget windowXml castToWindow "activeAxleWindow"

  buttonSelectF1 <- xmlGetWidget windowXml castToButton "buttonSelectF1"
  buttonSelectF2 <- xmlGetWidget windowXml castToButton "buttonSelectF2"
  buttonSelectF3 <- xmlGetWidget windowXml castToButton "buttonSelectF3"
  buttonSelectF4 <- xmlGetWidget windowXml castToButton "buttonSelectF4"

  lastLoadsWindow <- xmlGetWidget windowXml castToWindow "lastLoadsWindow"
  lastLoadsView <- xmlGetWidget windowXml castToTreeView "lastLoadsView"

  sellersModel <- setUpSellersBox db sellersBox
  attachSellerModel addCarOwnerChoice sellersModel

  sectorsModel <- setUpSectorsBox db sectorsBox

  carsModel <- setUpCarsBox db carsBox sellersBox sellersModel

  lastLoadsModel <- setUpLoadsView lastLoadsView
  
  on buttonSelectF1 buttonActivated $ comboBoxSetActive sellersBox 0 >> comboBoxSetActive carsBox 0
  on buttonSelectF2 buttonActivated $ comboBoxSetActive sellersBox 1 >> comboBoxSetActive carsBox 0
  on buttonSelectF3 buttonActivated $ comboBoxSetActive sellersBox 2 >> comboBoxSetActive carsBox 0
  on buttonSelectF4 buttonActivated $ comboBoxSetActive sellersBox 3 >> comboBoxSetActive carsBox 0

  buttonSelectF1 `on` buttonReleaseEvent $ tryEvent $
     do buttons <- eventButton
        liftIO $ print buttons

  on addCarButton buttonActivated $ doAddCarDialog db addCarDialog sellersBox addCarNameEntry addCarAxlesEntry addCarOwnerChoice sellersModel carsModel
  on addSellerButton buttonActivated $ doAddSellerDialog db addSellerDialog addSellerNameEntry sellersModel 
  on addSectorButton buttonActivated $ doAddSectorDialog db addSectorDialog addSectorNameEntry sectorsModel

-- on addSellerButton buttonActivated $ doAddSellerDialog 

  on carsBox changed  $ setupWeightsDialogs db radioWeighAsEmpty radioWeighAsFull carCurrentlyBeingWeighedRef indexCurrentlyBeingWeighedRef takingWeightingsRef tableWeights weightEntriesRef carsBox carsModel
  on sellersBox changed $ do updateCarsBox db carsModel sellersBox sellersModel
                             numCars <- listStoreGetSize carsModel
                             when (numCars == 1) $ comboBoxSetActive carsBox 0

  on buttonMeasureWeights buttonActivated $ writeIORef takingWeightingsRef True

  onEditableChanged addSectorNameEntry $ 
     do text <- entryGetText addSectorNameEntry
        widgetSetSensitive addSectorAddButton (not $ null text)

  onEditableChanged addSellerNameEntry $ 
     do text <- entryGetText addSellerNameEntry
        widgetSetSensitive addSellerAddButton (not $ null text)

  onEditableChanged addCarNameEntry $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 
  onEditableChanged addCarAxlesEntry $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 
  on addCarOwnerChoice changed $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 


  onDestroy win mainQuit
  forkOS $ inputThread db activeAxleLabel rawWeightLabel netWeightLabel fullWeightLabel tareWeightLabel carCurrentlyBeingWeighedRef indexCurrentlyBeingWeighedRef weightEntriesRef takingWeightingsRef radioWeighAsEmpty

  restrictCarAxlesToNumbersRef <- newIORef undefined
  restrictCarAxlesToNumbers <- onInsertText addCarAxlesEntry $ \str pos -> do
                                 id <- readIORef restrictCarAxlesToNumbersRef
                                 signalBlock id
                                 pos' <- editableInsertText addCarAxlesEntry (filter isNumber str) pos
                                 signalUnblock id
                                 stopInsertText id
                                 return pos'
  writeIORef restrictCarAxlesToNumbersRef restrictCarAxlesToNumbers

  timeoutAdd (updateTime timeLabel dateLabel >> return True) 1000
  updateTime timeLabel dateLabel

  updateSampleNum db sampleNumEntry

  let doConfirmRecordLoad = do
        sellerIterM <- comboBoxGetActiveIter sellersBox
        sectorIterM <- comboBoxGetActiveIter sectorsBox
        case sellerIterM of
          Nothing -> return () -- todo: tell the user to do stuff?
          (Just sellerIter) ->
              do sector <- case sectorIterM of
                             Nothing -> return "muu lohko"
                             Just i -> listStoreGetValue sectorsModel $ listStoreIterToIndex i

                 time <- getZonedTime
                 weight <- labelGetText netWeightLabel
                 dry <- entryGetText dryEntry
                 sampleNum <- entryGetText sampleNumEntry
                 seller <- sellerName `fmap` (listStoreGetValue sellersModel $ listStoreIterToIndex sellerIter)
                 
                 labelSetText confirmSellerLabel seller
                 labelSetText confirmSectorLabel sector
                 labelSetText confirmWeightLabel weight
                 labelSetText confirmDryLabel dry
                 labelSetText confirmSampleNumLabel sampleNum
                 labelSetText confirmDateLabel (formatTime defaultTimeLocale "%F" time)
                 labelSetText confirmTimeLabel (formatTime defaultTimeLocale "%H.%M" time)

                 r <- insertRow db "loads" [("seller", seller),
                                            ("sector", sector),
                                            ("time", formatTime defaultTimeLocale "%s" time),
                                            ("fullWeight", weight),
                                            ("dryStuff", dry),
                                            ("sampleNum", sampleNum)]

                 loadId <- getLastRowID db

                 resp <- dialogRun confirmLoadDialog
                 case resp of
                   ResponseUser 1 -> do
                       execParamStatement_ db "delete from loads where id = :loadId" [(":loadId", Int $ fromIntegral loadId)]
                       return ()
                   ResponseUser 2 -> 
                       do listStoreAppend lastLoadsModel (seller, weight, formatTime defaultTimeLocale "%H.%M" time)
                          comboBoxSetActive sellersBox (negate 1)
                          comboBoxSetActive carsBox (negate 1)
                          entrySetText dryEntry ""
                          labelSetText netWeightLabel ""
                          updateSampleNum db sampleNumEntry
                          return ()
                 widgetHide confirmLoadDialog

  on buttonRecordLoad buttonActivated $ doConfirmRecordLoad

  statisticsModel <- setUpStatisticsView treeviewStatistics
  on buttonUpdateStatistics buttonActivated $ updateStatistics db statisticsModel treeviewStatistics radioStatisticsBySeller

  widgetShowAll statisticsWindow
  widgetShowAll rawWeightWindow
  widgetShowAll activeAxleWindow
  widgetShowAll lastLoadsWindow
  widgetShowAll win

  mainGUI
