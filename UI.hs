{-# LANGUAGE NoMonomorphismRestriction, PackageImports, ScopedTypeVariables, ParallelListComp, OverloadedStrings #-}
module UI where

import Debug.Trace (trace)

import Common
-- import Print

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

import Data.List (lookup, findIndex, intercalate, find, nub, transpose, zipWith6)
import Data.Maybe (fromJust, catMaybes, isNothing)

import Data.Time
import System.Locale

import Text.Printf

import Control.Arrow

import Data.Set (Set)
import qualified Data.Set as Set

import "mtl" Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Control.Applicative

justLookup a xs = fromJust $ lookup a xs

castEnum = toEnum . fromEnum

showDate = formatTime defaultTimeLocale "%F"

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
                                       e2 <- entryNew
                                       entrySetText e1 (weights !! (n - 1))
                                       onEditableChanged e1  $ recordEmptyWeight db activeElement n e1
                                       entrySetWidthChars e1 5
                                       entrySetWidthChars e2 5
                                       tableAttach tableWeights e1 (n-1) n 0 1 [] [] 2 0 
                                       tableAttach tableWeights e2 (n-1) n 1 2 [] [] 2 0
                                       return (e1, e2))

           forM_ (emptyEntries ++ fullEntries) $ \e -> 
               afterEditableChanged e $ updateWeights netWeightLabel fullWeightLabel tareWeightLabel emptyEntries fullEntries

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


data Load = Load {
      identity :: Int, 
      version :: Int,
      date :: UTCTime,
      seller :: String,
      weight :: Double,
      density :: Double,
      dry :: Double,
      paid :: Bool,
      sampleNum :: Int,
      protein :: Double,
      grainType :: [Double]} deriving Show

onIdentity l f = l { identity = f (identity l) }

statRowToLoad :: [(String, Value)] -> Load
statRowToLoad xs = 
    case (justLookup "date" xs, justLookup "netWeight" xs, justLookup "seller" xs, justLookup "id" xs, justLookup "paid" xs, justLookup "version" xs, justLookup "density" xs, justLookup "dry" xs, justLookup "ohra" xs, justLookup "kaura" xs, justLookup "vehna" xs, justLookup "sampleNum" xs, justLookup "protein" xs) of 
      (Int date, Double weight, Text seller, Int identity, Int paid, Int version, Double density, Double dry, Double ohra, Double kaura, Double vehna, Int sampleNum, Double protein) ->
          Load (castEnum identity) 
               (castEnum version) 
               (readTime defaultTimeLocale "%s" (show date)) 
               seller 
               weight 
               density 
               dry 
               (if paid == 1 then True else False)
               (castEnum sampleNum)
               protein
               [ohra, kaura, vehna]

showLoad (Load identity version date seller weight density dry paid sampleNum protein types) = (seller, show weight, show date, paid)


-- insert values into a treeStore so that the first value in a given treePath is the root of that tree, and everything that comes afterward is children of that
-- fails if p is []
--treeStoreInsertInOrder :: TreeStore a -> TreePath -> a -> IO ()
treeStoreInsertInOrder t p v =
    do existing <- treeStoreLookup t p
       case existing of
         Nothing -> treeStoreInsert t (init p) (last p) v >> return p
         Just tn ->
             treeStoreInsert t p (length $ subForest tn) v >> return (p ++ [length $ subForest tn])

updatePayments db paymentsView sortedPaymentsModel paymentsModel = do
  selection <- treeViewGetSelection paymentsView
  rows <- treeSelectionGetSelectedRows selection
  underlyingRows <- mapM (treeModelSortConvertPathToChildPath sortedPaymentsModel) rows
  subtrees <- mapM (treeStoreLookup paymentsModel) underlyingRows
  let selected = Set.fromList $ map (identity &&& version) $ map rootLabel $ catMaybes subtrees

  treeStoreClear paymentsModel
  smallestIdContent <- execStatement db "select id from loads order by id asc limit 1"
  let modId :: Int -> Int
      modId = case smallestIdContent of
                Right [[[("id", Int identity)]]] -> (subtract (castEnum identity))
                _ -> id

  content <- execStatement db "select sampleNum, date, netWeight, seller, paid, id, version, dryStuff as dry, density, protein, ohra, kaura, vehna from loads order by id asc, version desc"
  case content of
    Left _ -> print content
    Right [xs] -> forM_ xs (\x -> do let toAdd = (\l -> ([modId (identity l)], l)) . statRowToLoad $ x
                                     path <- uncurry (treeStoreInsertInOrder paymentsModel) toAdd
                                     underlyingPath <- treeModelSortConvertChildPathToPath sortedPaymentsModel path
                                     when (Set.member ((identity &&& version) $ snd toAdd) selected) $ treeSelectionSelectPath selection path)

  

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


setupPaymentsView view = do
  model <- treeStoreNew []
  sortedModel <- treeModelSortNewWithModel model
  treeViewSetModel view sortedModel

--treeViewSetHeadersClickable view True

  let sortFuncOn f = (\a b -> do
                        aPath <- treeModelGetPath model a
                        bPath <- treeModelGetPath model b
                        aValue <- treeStoreGetValue model aPath
                        bValue <- treeStoreGetValue model bPath
                        return $ compare (f aValue) (f bValue))

  rendererSeller <- cellRendererTextNew
  rendererNetWeight <- cellRendererTextNew
  rendererTime <- cellRendererTextNew
  rendererSample <- cellRendererTextNew
  rendererPaid <- cellRendererToggleNew

  set rendererPaid [ cellToggleActivatable := False ]

  columnSeller <- treeViewColumnNew
  columnNetWeight <- treeViewColumnNew
  columnTime <- treeViewColumnNew
  columnSample <- treeViewColumnNew
  columnPaid <- treeViewColumnNew  

  cellLayoutPackStart columnSeller rendererSeller True
  cellLayoutPackStart columnNetWeight rendererNetWeight True
  cellLayoutPackStart columnTime rendererTime True
  cellLayoutPackStart columnSample rendererSample True
  cellLayoutPackStart columnPaid rendererPaid True

  cellLayoutSetAttributes columnSeller rendererSeller model $ \l -> [ cellText := seller l ]
  cellLayoutSetAttributes columnNetWeight rendererNetWeight model $ \l -> [ cellText := show (weight l) ]
  cellLayoutSetAttributes columnTime rendererTime model $ \l -> [ cellText := showDate (date l) ]
  cellLayoutSetAttributes columnSample rendererSample model $ \l -> [ cellText := show (sampleNum l) ]
  cellLayoutSetAttributes columnPaid rendererPaid model $ \l -> [ cellToggleActive := paid l ]

  treeSortableSetSortFunc sortedModel 0 (sortFuncOn seller)
  treeSortableSetSortFunc sortedModel 1 (sortFuncOn weight)
  treeSortableSetSortFunc sortedModel 2 (sortFuncOn date)
  treeSortableSetSortFunc sortedModel 3 (sortFuncOn sampleNum)
  treeSortableSetSortFunc sortedModel 4 (sortFuncOn paid)


  treeViewColumnSetTitle columnSeller "Isäntä"
  treeViewColumnSetTitle columnNetWeight "Paino"
  treeViewColumnSetTitle columnTime "Aika"
  treeViewColumnSetTitle columnSample "Näytenro"
  treeViewColumnSetTitle columnPaid "Maksettu"

  treeViewColumnSetSortColumnId columnSeller 0
  treeViewColumnSetSortColumnId columnNetWeight 1
  treeViewColumnSetSortColumnId columnTime 2
  treeViewColumnSetSortColumnId columnSample 3
  treeViewColumnSetSortColumnId columnPaid 4

  treeViewAppendColumn view columnSeller
  treeViewAppendColumn view columnNetWeight
  treeViewAppendColumn view columnTime
  treeViewAppendColumn view columnSample
  treeViewAppendColumn view columnPaid

  return (model, sortedModel)

setupWindowInWindowMenu window menuItem = do
  on menuItem checkMenuItemToggled $ do
    printf "hi"
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
    let moistureFactor = (100 - moisture) / 86
        mohraFactor = snd `fmap` find ((density >=) . fst) ohraHlp
        mkauraFactor = snd `fmap` find ((density >=) . fst) kauraHlp
        mvehnaFactor = snd `fmap` find ((density >=) . fst) vehnaHlp
    in do
      marketPricesM <- getMarketPrices db
      case (marketPricesM, mohraFactor, mkauraFactor, mvehnaFactor) of
         (Just marketPrices, Just ohraFactor, Just kauraFactor, Just vehnaFactor) -> 
             return $ Just $ (/1000) $ sum $ zipWith3 (\weight marketPrice densityFactor -> (marketPrice + densityFactor) * weight * moistureFactor) weights marketPrices [ohraFactor, kauraFactor, vehnaFactor]
         _ -> return Nothing

ohraHlp = [(71, 3), (70, 1), (64, 0), (63, -3), (62, -6), (61, -10), (60, -15), (59, -22), (58, -29), (57, -32), (56, -34), (55, -37), (53, -37), (50, -45), (45, -55), (40, -60), (0, -80)  ]
kauraHlp = [(60, 3), (59, 1), (56, 0), (54, -3), (53, -6), (52, -10), (51, -15), (50, -29), (49, -32) , (47, -34)  , (45, -37)  , (44, -40)  , (43, -45)  , (42, -50)  , (41, -55)  , (40, -60)  , (0, -70) ]
vehnaHlp = [(76, 0), (75, -1), (74, -1.5), (73, -2.5), (72, -4.5), (71, -5), (70, -6), (0, -20)]

kkhlpProAgria = [70,68,66,64,62,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30]
vehnaPriceProAgria = replicate 20 1 -- for now
kauraPriceProAgria = [0.0,0.0,0.0,1.0,1.0,1.0,0.99,0.97,0.95,0.91,0.87,0.83,0.79,0.75,0.71,0.67,0.63,0.59,0.55,0.51,0.47]
ohraPriceProAgria = [1.0,1.0,1.0,1.0,0.98,0.96,0.94,0.92,0.89,0.86,0.83,0.8,0.77,0.73,0.7,0.67,0.63,0.6,0.56,0.52,0.48]

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
    

nothingSelected tv tm stm =
    do s <- treeViewGetSelection tv
       rows <- treeSelectionGetSelectedRows s
       underlyingRows <- mapM (treeModelSortConvertPathToChildPath stm) rows
       subtrees <- mapM (treeStoreLookup tm) underlyingRows
       let selected = map rootLabel $ catMaybes subtrees
       case selected of
         [] -> return True
         _ -> return False

data Config = Config {
      alv :: Double
}

instance Aeson.FromJSON Config where
    parseJSON (Aeson.Object o) = Config <$> o Aeson..: "alv"

doUI = do
  initGUI

  db <- openConnection "vaaka.db"

  configFile <- B.readFile "config.json"
  let config = (\(Just x) -> x) $ Aeson.decode configFile :: Config -- who says Haskell keeps you safe? Not if you're LIVING ON THE EDGE

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
--  printSampleNoteButton <- xmlGetWidget windowXml castToButton "printSampleNoteButton"
                       

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
  paymentsMenuItem <- xmlGetWidget windowXml castToCheckMenuItem "paymentsMenuItem"

  grainPricesCalendar <- xmlGetWidget windowXml castToCalendar "grainPricesCalendar"
  ohraPriceEntry <- xmlGetWidget windowXml castToEntry "ohraPrice"
  kauraPriceEntry <- xmlGetWidget windowXml castToEntry "kauraPrice"
  vehnaPriceEntry <- xmlGetWidget windowXml castToEntry "vehnaPrice"
  storePricesButton <- xmlGetWidget windowXml castToButton "storePricesButton"

  paymentsWindow <- xmlGetWidget windowXml castToWindow "paymentsWindow"
  paymentsView <- xmlGetWidget windowXml castToTreeView "paymentsView"

  loadSellerLabel <- xmlGetWidget windowXml castToLabel "loadSellerLabel"
  loadPriceLabel <- xmlGetWidget windowXml castToLabel "loadPriceLabel"
  loadTypeLabel <- xmlGetWidget windowXml castToLabel "loadTypeLabel"
  loadWeightEntry <- xmlGetWidget windowXml castToEntry "loadWeightEntry"
  loadDryEntry <- xmlGetWidget windowXml castToEntry "loadDryEntry"
  loadDensityEntry <- xmlGetWidget windowXml castToEntry "loadDensityEntry"
  loadDateLabel <- xmlGetWidget windowXml castToLabel "loadDateLabel"
  loadProteinEntry <- xmlGetWidget windowXml castToEntry "loadProteinEntry"
  loadPaidToggle <- xmlGetWidget windowXml castToCheckButton "loadPaidToggle"
 
  sellersModel <- setUpSellersBox db sellersBox
  attachSellerModel addCarOwnerChoice sellersModel

  saveLoadChangesButton <- xmlGetWidget windowXml castToButton "saveLoadChangesButton"
  saveToFileButton <- xmlGetWidget windowXml castToButton "saveToFileButton"

  setupWindowInWindowMenu grainPricesWindow grainPricesMenuItem
  setupWindowInWindowMenu statisticsWindow statisticsMenuItem
  setupWindowInWindowMenu lastLoadsWindow recentLoadsMenuItem
  setupWindowInWindowMenu weighingStatusWindow weighingStatusMenuItem
  setupWindowInWindowMenu paymentsWindow paymentsMenuItem

  carsModel <- setUpCarsBox db carsBox sellersBox sellersModel

  lastLoadsModel <- setUpLoadsView lastLoadsView

  (paymentsModel, sortedPaymentsModel)  <- setupPaymentsView paymentsView 

  setupCalendar db storePricesButton ohraPriceEntry kauraPriceEntry vehnaPriceEntry grainPricesCalendar

  on addCarButton buttonActivated $ doAddCarDialog db addCarDialog sellersBox addCarNameEntry addCarAxlesEntry addCarOwnerChoice sellersModel carsModel
  on addSellerButton buttonActivated $ doAddSellerDialog db addSellerDialog addSellerNameEntry sellersModel 

  on carsBox changed  $ setupWeightsDialogs db radioWeighAsEmpty radioWeighAsFull indexCurrentlyBeingWeighedRef takingWeightingsRef tableWeights weightEntriesRef carsBox carsModel netWeightLabel fullWeightLabel tareWeightLabel
  on sellersBox changed $ do updateCarsBox db carsModel sellersBox sellersModel
                             numCars <- listStoreGetSize carsModel
                             when (numCars == 1) $ comboBoxSetActive carsBox 0

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

                 -- load IDs can't just autoincrement because of versioning, so, find the first free load ID first

                 newLoadIdContent <- execStatement db "select id from loads order by id desc limit 1"
                 let identity = case newLoadIdContent of
                                  Right [[[("id", Int x)]]] -> 1 + castEnum x
                                  _ -> 0

                 r <- insertRow db "loads" [("id", show identity),
                                            ("seller", seller),
                                            ("date", formatTime defaultTimeLocale "%s" time),
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

--                 ps <- on printSampleNoteButton buttonActivated $
--                   printSampleNote sampleNoteTemplatePixbuf time sampleNum grainTypeName

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
          
--                 signalDisconnect ps
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

  let updateLoadEntries = do
        let combineSellers = intercalate ", " . nub
            combineWeights = sum
            combinePrices = sum
--            combineTypes xs | trace (show xs) False = undefined
            combineTypes = (\[ohrat, kaurat, vehnat] -> [sum ohrat, sum kaurat, sum vehnat]) . transpose
            combineDates dates = (min dates, max dates)
                                 
            average xs = sum xs / fromIntegral (length xs)

            combineDry = average
            combineDensity = average

        nothing <- nothingSelected paymentsView paymentsModel sortedPaymentsModel
        when (not nothing) $ do
          selection <- treeViewGetSelection paymentsView
          rows <- treeSelectionGetSelectedRows selection
          underlyingRows <- mapM (treeModelSortConvertPathToChildPath sortedPaymentsModel) rows
          subtrees <- mapM (treeStoreLookup paymentsModel) underlyingRows
          let selected = map rootLabel $ catMaybes subtrees

          let w = map weight selected
              paids = map paid selected
              types = combineTypes $ zipWith (\m xs -> map (m*) xs) w (map grainType selected)
              avgDensity = (/ average w) $ average $ zipWith (*) w $ map density selected
              avgDry = (/ average w) $ average $ zipWith (*) w $ map dry selected
              avgProtein = (/ average w) $ average $ zipWith (*) w $ map protein selected

          labelSetText loadSellerLabel $ combineSellers (map seller selected)
          entrySetText loadWeightEntry $ show $ combineWeights (map weight selected)
          entrySetText loadDensityEntry $ printf "%.2f" $ avgDensity
          entrySetText loadDryEntry $ printf "%.2f" $ avgDry
          entrySetText loadProteinEntry $ printf "%.2f" $ avgProtein
          labelSetText loadTypeLabel $ printGrainDistribution $ toDistribution $ types
          labelSetText loadDateLabel ((showDate $ minimum $ map date selected) ++ " - " ++ (showDate $ maximum $ map date selected))

          toggleButtonSetInconsistent loadPaidToggle False

          if any (==True) paids
             then if any (==False) paids
                  then toggleButtonSetInconsistent loadPaidToggle True
                  else toggleButtonSetActive loadPaidToggle True
             else toggleButtonSetActive loadPaidToggle False

          prices <- sequence [loadPrice db (map (w*) x) y (100 - z) 
                              | x <- map grainType selected
                              | w <- map weight selected 
                              | y <- map density selected
                              | z <- map dry selected]
          case any isNothing prices of
            True -> labelSetText loadPriceLabel "###"
            False -> labelSetText loadPriceLabel (printf "%.2f" (sum $ catMaybes prices) ++ " eur")

--          price <- loadPrice db types avgDensity (100 - avgDry)

          case rows of
            [_] -> do editableSetEditable loadWeightEntry True
                      editableSetEditable loadDensityEntry True
                      editableSetEditable loadDryEntry True
                      editableSetEditable loadProteinEntry True
            _ -> do editableSetEditable loadWeightEntry False
                    editableSetEditable loadDensityEntry False
                    editableSetEditable loadDryEntry False
                    editableSetEditable loadProteinEntry False
          return ()

  let storeLoadChanges = do
        selection <- treeViewGetSelection paymentsView
        rows <- treeSelectionGetSelectedRows selection
        underlyingRows <- mapM (treeModelSortConvertPathToChildPath sortedPaymentsModel) rows
        subtrees <- mapM (treeStoreLookup paymentsModel) underlyingRows
        let selected = map rootLabel $ catMaybes subtrees
        forM_ selected $ \l@(Load identity version date seller weightOld densityOld dryOld paidOld sampleNum proteinOld types) ->
            do versionContent <- execParamStatement db "select version from loads where id = :identity order by version desc limit 1" [(":identity", Int $ castEnum identity)]
               let newVersion = case versionContent of
                                  Right [[[("version", Int x)]]] -> 1 + castEnum x
                                  _ -> 1

               paid <- toggleButtonGetActive loadPaidToggle

               (weight, density, dry, protein) <- case selected of
                                           [_] -> do weight <- entryGetText loadWeightEntry
                                                     density <- entryGetText loadDensityEntry
                                                     dry <- entryGetText loadDryEntry
                                                     protein <- entryGetText loadProteinEntry
                                                     return (weight, density, dry, protein)
                                           _ -> return (show weightOld, show densityOld, show dryOld, show proteinOld)

               case (reads weight, reads density, reads dry, reads protein) of
                 ([(_ :: Double, "")], [(_ :: Double, "")], [(_ :: Double, "")], [(_ :: Double, "")]) ->
                     do e <- insertRow db "loads" [("id", show identity),
                                                   ("seller", seller),
                                                   ("date", formatTime defaultTimeLocale "%s" date),
                                                   ("netWeight", weight),
                                                   ("dryStuff", dry),
                                                   ("density", density),
                                                   ("sampleNum", show sampleNum),
                                                   ("ohra", show (types !! 0)),
                                                   ("kaura", show (types !! 1)),
                                                   ("vehna", show (types !! 2)),
                                                   ("paid", if paid then "1" else "0"),
                                                   ("protein", protein),
                                                   ("version", show newVersion)]
                        case e of
                          Nothing -> return ()
                          Just error -> print error
                 (eWeight, eDensity, eDry, eProtein) -> 
                     complainAboutMissingData [readsValid "paino" eWeight,
                                               readsValid "hehtolitrapaino" eDensity,
                                               readsValid "kuiva-aine" eDry,
                                               readsValid "valkuaisaine" eProtein]

        updatePayments db paymentsView sortedPaymentsModel paymentsModel

  on saveToFileButton buttonActivated $ do
    selection <- treeViewGetSelection paymentsView
    rows <- treeSelectionGetSelectedRows selection
    underlyingRows <- mapM (treeModelSortConvertPathToChildPath sortedPaymentsModel) rows
    subtrees <- mapM (treeStoreLookup paymentsModel) underlyingRows
    let selected = map rootLabel $ catMaybes subtrees
    prices <- sequence [loadPrice db (map (w*) x) y (100 - z) 
                            | x <- map grainType selected
                            | w <- map weight selected 
                            | y <- map density selected
                            | z <- map dry selected]
    let price = sum $ catMaybes prices
        showPrice :: Maybe Double -> String
        showPrice Nothing = "###"
        showPrice (Just d) = printf "%9.2f" d
        analysisPrice = length selected * 15

    marketPrices <- fromJust `fmap` getMarketPrices db

    writeFile "kuitti.txt"
                  (intercalate ", " (nub $ map seller selected) ++ 
                   "\n\nSovitut perushinnat: Ohra " ++ show (marketPrices !! 0) ++ " eur/t, kaura " ++ show (marketPrices !! 1) ++ " eur/t, vehna " ++ show (marketPrices !! 2) ++ " eur/t" ++
                   "\n\n          Laji                 Nettopaino   Kuiva-aine   Hehtolitrapaino   Valkuaisaine   Hinta\n\n" ++
                   (intercalate "\n" $ zipWith6 (printf "%-30s %8.2f kg  %8.2f %%   %9.2f kg/hl   %7.2f %%   %s eur") (map (printGrainDistribution . toDistribution . grainType) selected) (map weight selected) (map dry selected) (map density selected) (map protein selected) (map showPrice prices)) ++ "\n\n" ++
                   "Kuormista yhteensä " ++ printf "%0.2f" (price * (1/(1 + alv config / 100))) ++ " eur (ilman alv.)\n" ++
                   "                   " ++ printf "%0.2f" price ++ " eur (alv. " ++ show (alv config) ++ " %)\n\n" ++
                   " - analysointi " ++ show (length selected) ++ " * 15 eur = " ++ show analysisPrice ++ " eur (sis. alv 23%)\n\n" ++
                   "Tilitetään " ++ printf "%0.2f eur" (price - fromIntegral analysisPrice) ++
                   "\n\nAnalyysit: Tuula Kantola, Suomen Viljava OY, Ylivieska\n\n")
                                


  loadsSelection <- treeViewGetSelection paymentsView
  treeSelectionSetMode loadsSelection SelectionMultiple
  onSelectionChanged loadsSelection $ updateLoadEntries

  on buttonMeasureWeights buttonActivated $ writeIORef takingWeightingsRef True

  after radioSeka buttonActivated $ do p <- toggleButtonGetActive radioSeka; when p doGrainTypeDialog
  on saveLoadChangesButton buttonActivated $ storeLoadChanges
  on buttonRecordLoad buttonActivated $ doConfirmRecordLoad

  statisticsModel <- setUpStatisticsView treeviewStatistics
  on buttonUpdateStatistics buttonActivated $ updateStatistics db statisticsModel treeviewStatistics

  widgetShowAll win
--  menuItemActivate weighingStatusMenuItem

  updatePayments db paymentsView sortedPaymentsModel paymentsModel

  mainGUI

