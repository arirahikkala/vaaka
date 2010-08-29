{-# LANGUAGE NoMonomorphismRestriction, PackageImports, ScopedTypeVariables #-}
module UI where

import Common
import Print

import SQLite (insertRow, upsertRow)
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

import Data.List (lookup, findIndex, intercalate, find)
import Data.Maybe (fromJust, catMaybes)

import Data.Time
import System.Locale

import Text.Printf

import "mtl" Control.Monad.Trans

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Mixer 

justLookup a xs = fromJust $ lookup a xs

castEnum = toEnum . fromEnum


rowToCar xs =
    case justLookup "name" xs of
      Text name -> case justLookup "numAxles" xs of
                     Int numAxles -> case justLookup "id" xs of 
                                       Int carId -> Car (fromIntegral carId) name (fromIntegral numAxles)

rowToSeller xs =
    case justLookup "name" xs of
      Text name -> Seller name

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

setUpSellersBox db view = do
  model <- listStoreNew []
  attachSellerModel view model
  updateSellers db model

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

setupWeightsDialogs db radioWeighAsEmpty radioWeighAsFull indexCurrentlyBeingWeighedRef takingReadingsRef tableWeights weightEntriesRef carsBox carsModel netWeightLabel fullWeightLabel tareWeightLabel = do
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

           entries@(emptyEntries, fullEntries) <- unzip `fmap` (forM [1..axles] $ 
                              \n -> do e1 <- entryNew
                                       onEditableChanged e1  $ recordEmptyWeight db activeElement n e1
                                       e2 <- entryNew
                                       entrySetText e1 (weights !! (n - 1))
                                       entrySetWidthChars e1 5
                                       entrySetWidthChars e2 5
                                       tableAttach tableWeights e1 (n-1) n 0 1 [] [] 2 0 
                                       tableAttach tableWeights e2 (n-1) n 1 2 [] [] 2 0
                                       return (e1, e2))

           forM_ (emptyEntries ++ fullEntries) $ \e -> 
               onEditableChanged e $ updateWeights netWeightLabel fullWeightLabel tareWeightLabel emptyEntries fullEntries

           updateWeights netWeightLabel fullWeightLabel tareWeightLabel emptyEntries fullEntries

           writeIORef weightEntriesRef $ entries
           widgetShowAll tableWeights

recordEmptyWeight db activeCar axle weightEntry = do
  weightText <- ('0' :) `fmap` entryGetText weightEntry
  case reads weightText of
    [(weight :: Double, "")] ->
        upsertRow db "emptyAxleWeights" [("carId", show $ carId activeCar), ("axleNum", show axle), ("weight", weightText)] >>
        return ()
    _ -> return ()


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
          case justLookup "netWeight" xs of 
            Double weight -> (seller, weight)

statRowToQuad :: [(String, Value)] -> (Double, (UTCTime, UTCTime))
statRowToQuad xs = 
    case justLookup "netWeight" xs of 
      Double weight -> case justLookup "minTime" xs of 
                         Int minTime -> case justLookup "maxTime" xs of 
                                          Int maxTime -> (weight, 
                                                          (readTime defaultTimeLocale "%s" (show minTime), readTime defaultTimeLocale "%s" (show maxTime)))

updateStatistics db statisticsModel treeviewStatistics = do
  treeStoreClear statisticsModel
  content <- execStatement db "select seller, sum(netWeight) as netWeight from loads group by seller"
  case content of
    Left _ -> return ()
    Right xs -> let stats = map statRowToTuple $ head xs in
                treeStoreInsertTree statisticsModel [] 0 $ Node ("Kaikki: " ++ show (sum . map snd $ stats) ++ " kg") $
                                [Node (name ++ ": " ++ show weight ++ " kg") [] | (name, weight) <- stats]


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

setupWindowInWindowMenu window menuItem = do
  onToggle menuItem $ do
    state <- checkMenuItemGetActive menuItem
    case state of
      True -> widgetShowAll window
      False -> widgetHide window
  on window deleteEvent $ liftIO (widgetHide window >> checkMenuItemSetActive menuItem False >> return True)
  onShow window $ checkMenuItemSetActive menuItem True


updateCalendarGrainPrices db ohraEntry kauraEntry vehnaEntry calendar = do
  (year, month, day) <- calendarGetDate calendar
  -- clear the prices in any case, regardless of what's found in the database
  -- (a better UI would proly separate setting and examining the prices but we'll make do with a simple one for now)
  entrySetText ohraEntry ""
  entrySetText kauraEntry ""
  entrySetText vehnaEntry ""

  content <- execParamStatement db 
                           "select ohraPrice, vehnaPrice, kauraPrice from grainPrices \
                           \where date <= :date \
                           \order by date desc limit 1"
                           [(":date", Text $ printf "%04i-%02i-%02i" year month day)]

  case content of
    Left err -> print err -- todo: do something more useful?
    Right [[]] -> return () -- query was ok, but there was nothing in the database to see
    Right [[currentPrices]] -> do
        case (justLookup "ohraPrice" currentPrices, justLookup "kauraPrice" currentPrices, justLookup "vehnaPrice" currentPrices) of
          (Double ohraPrice, Double kauraPrice, Double vehnaPrice) ->
              do entrySetText ohraEntry $ show ohraPrice
                 entrySetText kauraEntry $ show kauraPrice
                 entrySetText vehnaEntry $ show vehnaPrice
  

updateCalendarMarks db calendar = do
  calendarClearMarks calendar
  (year, month, day) <- calendarGetDate calendar
  content <- execParamStatement db 
                                "select strftime ('%d', date) as day from grainPrices where date like :date"
                                [(":date", Text $ printf "%04i-%02i%%" year month)]

  case content of
    Left err -> print err -- todo: do something more useful?
    Right [setDays] -> do
        forM_ setDays $ \r ->
            case justLookup "day" r of
              (Text rowDay) -> calendarMarkDay calendar (read rowDay) 
                  
updateCalendar db ohraEntry kauraEntry vehnaEntry calendar = do
  updateCalendarGrainPrices db ohraEntry kauraEntry vehnaEntry calendar
  updateCalendarMarks db calendar

setupCalendar db storePricesButton ohraEntry kauraEntry vehnaEntry calendar = do
  onMonthChanged calendar $ updateCalendar db ohraEntry kauraEntry vehnaEntry calendar
  afterDaySelected calendar $ updateCalendarGrainPrices db ohraEntry kauraEntry vehnaEntry calendar
  updateCalendar db ohraEntry kauraEntry vehnaEntry calendar

  on storePricesButton buttonActivated $ do
    ohraText <- entryGetText ohraEntry
    kauraText <- entryGetText kauraEntry
    vehnaText <- entryGetText vehnaEntry

    case (reads ohraText, reads kauraText, reads vehnaText) of
      ([(ohra, "")], [(kaura, "")], [(vehna, "")]) -> 
          do (year, month, day) <- calendarGetDate calendar
             upsertRow db "grainPrices" [("date", printf "%04i-%02i-%02i" year month day),
                                         ("ohraPrice", show (ohra :: Double)),
                                         ("kauraPrice", show (kaura :: Double)),
                                         ("vehnaPrice", show (vehna :: Double))]
      
             return ()
      (ohra, kaura, vehna) -> complainAboutMissingData (zipWith readsValid ["ohran hinta", "kauran hinta", "vehnän hinta"] [ohra, kaura, vehna])

    updateCalendar db ohraEntry kauraEntry vehnaEntry calendar

readsValid _ [(_, "")] = Nothing
readsValid name _ = Just name

complainAboutMissingData xs =
              do dialog <- messageDialogNew Nothing [DialogDestroyWithParent] MessageInfo ButtonsOk
                            ("Seuraavia tietoja ei ole asetettu oikein:\n" ++ 
                             (intercalate "\n" $ catMaybes $ xs))
                 dialogRun dialog
                 widgetDestroy dialog
                 return ()

doUI = do
  initGUI

  db <- openConnection "vaaka.db"
  sampleNoteTemplatePixbuf <- pixbufNewFromFile "lappupohja.jpg"


  weightEntriesRef <- newIORef ([], [])
  takingWeightingsRef <- newIORef False
  indexCurrentlyBeingWeighedRef <- newIORef 0

  windowXmlM <- xmlNew "vaakaVilja.glade"
  let windowXml = case windowXmlM of
                    Just x -> x
                    Nothing -> error "can't find the interface glade file in the current directory" -- todo: more useful error message?

  win <- xmlGetWidget windowXml castToWindow "window1"
  carsBox <- xmlGetWidget windowXml castToComboBox "boxCars"
  sellersBox <- xmlGetWidget windowXml castToComboBox "boxSellers"

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

  tableWeights <- xmlGetWidget windowXml castToTable "tableWeights"
  buttonMeasureWeights <- xmlGetWidget windowXml castToButton "buttonMeasureWeights"
  radioWeighAsEmpty <- xmlGetWidget windowXml castToRadioButton "radioWeighAsEmpty"
  radioWeighAsFull <- xmlGetWidget windowXml castToRadioButton "radioWeighAsFull"

  dryEntry <- xmlGetWidget windowXml castToEntry "dryEntry"
  sampleNumEntry <- xmlGetWidget windowXml castToEntry "sampleNumEntry"
  densityEntry <- xmlGetWidget windowXml castToEntry "densityEntry"

  netWeightLabel <- xmlGetWidget windowXml castToLabel "netWeightLabel"
  fullWeightLabel <- xmlGetWidget windowXml castToLabel "fullWeightLabel"
  tareWeightLabel <- xmlGetWidget windowXml castToLabel "tareWeightLabel"

  dateLabel <- xmlGetWidget windowXml castToLabel "dateLabel"
  timeLabel <- xmlGetWidget windowXml castToLabel "timeLabel"

  buttonRecordLoad <- xmlGetWidget windowXml castToButton "buttonRecordLoad"

  confirmLoadDialog <- xmlGetWidget windowXml castToDialog "confirmLoadDialog"
  confirmSellerLabel <- xmlGetWidget windowXml castToLabel "confirmSellerLabel"
  confirmWeightLabel <- xmlGetWidget windowXml castToLabel "confirmWeightLabel"
  confirmDryLabel <- xmlGetWidget windowXml castToLabel "confirmDryLabel"
  confirmDensityLabel <- xmlGetWidget windowXml castToLabel "confirmDensityLabel"
  confirmSampleNumLabel <- xmlGetWidget windowXml castToLabel "confirmSampleNumLabel"
  confirmDateLabel <- xmlGetWidget windowXml castToLabel "confirmDateLabel"
  confirmTimeLabel <- xmlGetWidget windowXml castToLabel "confirmTimeLabel"
  confirmGrainTypeLabel <- xmlGetWidget windowXml castToLabel "confirmGrainTypeLabel"
  confirmPriceLabel <- xmlGetWidget windowXml castToLabel "confirmPriceLabel"
  printSampleNoteButton <- xmlGetWidget windowXml castToButton "printSampleNoteButton"
                       

  statisticsWindow <- xmlGetWidget windowXml castToWindow "statisticsWindow"
  treeviewStatistics <- xmlGetWidget windowXml castToTreeView "treeviewStatistics"
  buttonUpdateStatistics <- xmlGetWidget windowXml castToButton "buttonUpdateStatistics"

  rawWeightLabel <- xmlGetWidget windowXml castToLabel "rawWeightLabel"
  activeAxleLabel <- xmlGetWidget windowXml castToLabel "activeAxleLabel"

  lastLoadsWindow <- xmlGetWidget windowXml castToWindow "lastLoadsWindow"
  lastLoadsView <- xmlGetWidget windowXml castToTreeView "lastLoadsView"

  radioSeka <- xmlGetWidget windowXml castToRadioButton "radioSeka"

  grainTypeDialog <- xmlGetWidget windowXml castToDialog "grainTypeDialog"

  radioOhra <- xmlGetWidget windowXml castToRadioButton "radioOhra"
  radioKaura <- xmlGetWidget windowXml castToRadioButton "radioKaura"
  radioVehna <- xmlGetWidget windowXml castToRadioButton "radioVehna"
  radioSeka <- xmlGetWidget windowXml castToRadioButton "radioSeka"

  ohraScale <- xmlGetWidget windowXml castToScale "ohraScale"
  kauraScale <- xmlGetWidget windowXml castToScale "kauraScale"
  vehnaScale <- xmlGetWidget windowXml castToScale "vehnaScale"

  weighingStatusWindow <- xmlGetWidget windowXml castToWindow "weighingStatusWindow"

  grainPricesWindow <- xmlGetWidget windowXml castToWindow "grainPricesWindow"
  grainPricesMenuItem <- xmlGetWidget windowXml castToCheckMenuItem "grainPricesMenuItem"
  statisticsMenuItem <- xmlGetWidget windowXml castToCheckMenuItem "statisticsMenuItem"
  recentLoadsMenuItem <- xmlGetWidget windowXml castToCheckMenuItem "recentLoadsMenuItem"
  weighingStatusMenuItem <- xmlGetWidget windowXml castToCheckMenuItem "weighingStatusMenuItem"

  grainPricesCalendar <- xmlGetWidget windowXml castToCalendar "grainPricesCalendar"
  ohraPriceEntry <- xmlGetWidget windowXml castToEntry "ohraPrice"
  kauraPriceEntry <- xmlGetWidget windowXml castToEntry "kauraPrice"
  vehnaPriceEntry <- xmlGetWidget windowXml castToEntry "vehnaPrice"
  storePricesButton <- xmlGetWidget windowXml castToButton "storePricesButton"

  sellersModel <- setUpSellersBox db sellersBox
  attachSellerModel addCarOwnerChoice sellersModel

  setupWindowInWindowMenu grainPricesWindow grainPricesMenuItem
  setupWindowInWindowMenu statisticsWindow statisticsMenuItem
  setupWindowInWindowMenu lastLoadsWindow recentLoadsMenuItem
  setupWindowInWindowMenu weighingStatusWindow weighingStatusMenuItem

  carsModel <- setUpCarsBox db carsBox sellersBox sellersModel

  lastLoadsModel <- setUpLoadsView lastLoadsView

  setupCalendar db storePricesButton ohraPriceEntry kauraPriceEntry vehnaPriceEntry grainPricesCalendar

  on addCarButton buttonActivated $ doAddCarDialog db addCarDialog sellersBox addCarNameEntry addCarAxlesEntry addCarOwnerChoice sellersModel carsModel
  on addSellerButton buttonActivated $ doAddSellerDialog db addSellerDialog addSellerNameEntry sellersModel 

  on carsBox changed  $ setupWeightsDialogs db radioWeighAsEmpty radioWeighAsFull indexCurrentlyBeingWeighedRef takingWeightingsRef tableWeights weightEntriesRef carsBox carsModel netWeightLabel fullWeightLabel tareWeightLabel
  on sellersBox changed $ do updateCarsBox db carsModel sellersBox sellersModel
                             numCars <- listStoreGetSize carsModel
                             when (numCars == 1) $ comboBoxSetActive carsBox 0

  on buttonMeasureWeights buttonActivated $ do 
    writeIORef takingWeightingsRef True

  onEditableChanged addSellerNameEntry $ 
     do text <- entryGetText addSellerNameEntry
        widgetSetSensitive addSellerAddButton (not $ null text)

  onEditableChanged addCarNameEntry $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 
  onEditableChanged addCarAxlesEntry $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 
  on addCarOwnerChoice changed $ checkIfCarAddable addCarAddButton addCarNameEntry addCarAxlesEntry addCarOwnerChoice 


  onDestroy win mainQuit
  forkOS $ inputThread db activeAxleLabel rawWeightLabel netWeightLabel fullWeightLabel tareWeightLabel indexCurrentlyBeingWeighedRef weightEntriesRef takingWeightingsRef radioWeighAsEmpty

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
        dry <- entryGetText dryEntry
        sampleNum <- entryGetText sampleNumEntry
        density <- entryGetText densityEntry
        case (sellerIterM, reads dry, reads sampleNum, reads density) of
          (Just sellerIter, [(dryNum :: Double, "")], [(sampleNumNum :: Int, "")], [(densityNum :: Double, "")]) ->
              do time <- getCurrentTime
                 weight <- labelGetText netWeightLabel
                 seller <- sellerName `fmap` (listStoreGetValue sellersModel $ listStoreIterToIndex sellerIter)

                 grainTypeButtons <- mapM toggleButtonGetActive [radioOhra, radioKaura, radioVehna, radioSeka]
                 mixtureValues <- mapM rangeGetValue [ohraScale, kauraScale, vehnaScale]

                 let (grainTypeDistribution, grainTypeName) 
                      = case findIndex id grainTypeButtons of
                         Just 0 -> ([1, 0, 0], Ohra)
                         Just 1 -> ([0, 1, 0], Kaura)
                         Just 2 -> ([0, 0, 1], Vehna)
                         Just 3 -> (toDistribution mixtureValues, Seka)

                 priceM <- loadPrice db (map (*read weight) grainTypeDistribution) densityNum (100 - dryNum)

                 labelSetText confirmSellerLabel seller
                 labelSetText confirmWeightLabel (weight ++ " kg")
                 labelSetText confirmDryLabel (dry ++ " %")
                 labelSetText confirmSampleNumLabel sampleNum
                 labelSetText confirmDensityLabel (density ++ " kg/hl")
                 labelSetText confirmDateLabel (formatTime defaultTimeLocale "%F" time)
                 labelSetText confirmTimeLabel (formatTime defaultTimeLocale "%H.%M" time)
                 labelSetText confirmGrainTypeLabel $ printGrainDistribution $ grainTypeDistribution
                 labelSetText confirmPriceLabel $ 
                              case priceM of
                                Nothing -> "###"
                                Just price -> printf "%.0f" price ++ " e"

                 r <- insertRow db "loads" [("seller", seller),
                                            ("time", formatTime defaultTimeLocale "%s" time),
                                            ("netWeight", weight),
                                            ("dryStuff", dry),
                                            ("density", density),
                                            ("sampleNum", sampleNum),
                                            ("ohra", show (grainTypeDistribution !! 0)),
                                            ("kaura", show (grainTypeDistribution !! 1)),
                                            ("vehna", show (grainTypeDistribution !! 2))]

                 case r of
                   Nothing -> return ()
                   Just s -> putStrLn s

                 loadId <- getLastRowID db

                 ps <- on printSampleNoteButton buttonActivated $
                   printSampleNote sampleNoteTemplatePixbuf time sampleNum grainTypeName

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
          
                 signalDisconnect ps
          (_, dryReads, sampleReads, densityReads) -> 
              complainAboutMissingData (case sellerIterM of 
                                          Nothing -> Just "kuski/isäntä"
                                          _ -> Nothing
                                          : [readsValid "kuiva-aine" dryReads,
                                             readsValid "näytenumero" sampleReads,
                                             readsValid "hehtolitrapaino" densityReads])

  let doGrainTypeDialog = do
        resp <- dialogRun grainTypeDialog
        widgetHide grainTypeDialog
        return ()

  after radioSeka buttonActivated $ do p <- toggleButtonGetActive radioSeka; when p doGrainTypeDialog

  on buttonRecordLoad buttonActivated $ doConfirmRecordLoad

  statisticsModel <- setUpStatisticsView treeviewStatistics
  on buttonUpdateStatistics buttonActivated $ updateStatistics db statisticsModel treeviewStatistics

  widgetShowAll win
  menuItemActivate weighingStatusMenuItem

  mainGUI

-- scale a list of doubles so that the sum of the whole thing is 1
toDistribution :: [Double] -> [Double]
toDistribution [] = []
toDistribution xs = 
    let s = sum xs in 
    map (/s) xs

printGrainDistribution :: [Double] -> String
printGrainDistribution [ohra, kaura, vehna] =
    intercalate ", " $ catMaybes $
    [if ohra > 0 then Just $ printf "Ohraa %.0f%%" (100 * ohra) else Nothing,
     if kaura > 0 then Just $ printf "Kauraa %.0f%%" (100 * kaura) else Nothing,
     if vehna > 0 then Just $ printf "Vehnää %.0f%%" (100 * vehna) else Nothing]

loadPrice db weights density moisture = 
    let correctedDensity = (density +) $ min 8 $ max 0 $ fromIntegral $ ((round moisture - 15) `div` 3)
        moistureFactor = (100 - moisture) / 86
        mohraFactor = snd `fmap` find ((correctedDensity >=) . fst) (zip kkhlp ohraPrice)
        mkauraFactor = snd `fmap` find ((correctedDensity >=) . fst) (zip kkhlp kauraPrice)
        mvehnaFactor = snd `fmap` find ((correctedDensity >=) . fst) (zip kkhlp vehnaPrice)
    in do
      marketPricesM <- getMarketPrices db
      case (marketPricesM, mohraFactor, mkauraFactor, mvehnaFactor) of
         (Just marketPrices, Just ohraFactor, Just kauraFactor, Just vehnaFactor) -> 
             return $ Just $ (/1000) $ sum $ zipWith3 (\x y z -> x * y * z * moistureFactor) weights marketPrices [ohraFactor, kauraFactor, vehnaFactor]
         _ -> return Nothing

kkhlp = [70,68,66,64,62,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30]

vehnaPrice = replicate 20 1 -- for now

kauraPrice = [0.0,0.0,0.0,1.0,1.0,1.0,0.99,0.97,0.95,0.91,0.87,0.83,0.79,0.75,0.71,0.67,0.63,0.59,0.55,0.51,0.47]
ohraPrice = [1.0,1.0,1.0,1.0,0.98,0.96,0.94,0.92,0.89,0.86,0.83,0.8,0.77,0.73,0.7,0.67,0.63,0.6,0.56,0.52,0.48]

getMarketPrices db = do
  time <- getCurrentTime
  let (year, month, day) = toGregorian . utctDay $ time
  content <- execParamStatement db 
             "select ohraPrice, vehnaPrice, kauraPrice from grainPrices \
             \where date <= :date \
             \order by date desc limit 1"
            [(":date", Text $ printf "%04i-%02i-%02i" year month day)]
  case content of
    Left err -> return Nothing -- todo: do something more useful?
    Right [[]] -> return Nothing -- query was ok, but there was nothing in the database to see
    Right [[currentPrices]] -> do
        case (justLookup "ohraPrice" currentPrices, justLookup "kauraPrice" currentPrices, justLookup "vehnaPrice" currentPrices) of
          (Double ohraPrice, Double kauraPrice, Double vehnaPrice) ->
              return $ Just [ohraPrice, kauraPrice, vehnaPrice]
          _ -> return Nothing
    
