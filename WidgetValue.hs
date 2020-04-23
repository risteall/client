{-# LANGUAGE ExistentialQuantification, FunctionalDependencies, DefaultSignatures, FlexibleInstances, DeriveGeneric, FlexibleContexts, ScopedTypeVariables, LambdaCase, DataKinds, KindSignatures, GADTs #-}

module WidgetValue where

import Data.Maybe
import Data.Char
import Text.Read hiding (get)
import Generics.Deriving
import Data.List
import Control.Monad
import Data.Colour.SRGB
import GHC.TypeLits
import Data.Kind
import Data.Proxy

import Graphics.UI.Gtk hiding (get, set)
import qualified Graphics.UI.Gtk as Gtk
import Data.AppSettings

class WidgetClass b => WidgetValue a b | a -> b where
  makeWidget :: IO (b, IO (Maybe a), a -> IO ())

defaultMakeWidget :: (Read a, Show a) => IO (Entry, IO (Maybe a), a -> IO ())
defaultMakeWidget = do
  e <- entryNew
  entrySetActivatesDefault e True
  return (e, readMaybe <$> entryGetText e, entrySetText e . show)

instance WidgetValue String Entry where
  makeWidget = defaultMakeWidget

-- empty entry is Nothing
instance WidgetValue (Maybe String) Entry where
  makeWidget = do
    e <- entryNew
    Gtk.set e [widgetWidthRequest := 300]
    entrySetActivatesDefault e True
    return (e
           ,(\s -> if all isSpace s then Just Nothing else Just (Just s)) <$> entryGetText e
           ,entrySetText e . fromMaybe ""
           )

instance WidgetValue Bool CheckButton where
  makeWidget = do
    b <- checkButtonNew
    return (b, Just <$> toggleButtonGetActive b, toggleButtonSetActive b)

maybeWidget :: WidgetValue a w => IO (HBox, IO (Maybe (Maybe a)), Maybe a -> IO ())
maybeWidget = do
  box <- hBoxNew False 5
  b <- checkButtonNew
  (w, get, set) <- makeWidget
  b `on` toggled $ toggleButtonGetActive b >>= widgetSetSensitive w
  boxPackStart box b PackNatural 0
  boxPackStart box w PackNatural 0
  let
    get' = toggleButtonGetActive b >>= \case
      False -> return (Just Nothing)
      True -> fmap (fmap Just) get
    set' Nothing = do
      toggleButtonSetActive b False
      widgetSetSensitive w False
    set' (Just a) = do
      toggleButtonSetActive b True
      widgetSetSensitive w True
      set a
  return (box, get', set')

instance WidgetValue Int Entry where
  makeWidget = defaultMakeWidget

instance WidgetValue (Maybe Int) HBox where
  makeWidget = maybeWidget

instance WidgetValue Double Entry where
  makeWidget = defaultMakeWidget

instance WidgetValue [Int] Entry where
  makeWidget = do
    e <- entryNew
    entrySetActivatesDefault e True
    return (e, readMaybe . (\s -> "[" ++ s ++ "]") <$> entryGetText e, entrySetText e . intercalate ", " . map show)

instance WidgetValue (Maybe [Int]) HBox where
  makeWidget = maybeWidget

enumWidget :: Eq a => [(a, String)] -> IO (HBox, IO (Maybe a), a -> IO ())
enumWidget l = do
  box <- hBoxNew False 5
  buttons@(b:bs) <- mapM (radioButtonNewWithLabel . snd) l
  mapM_ (flip radioButtonSetGroup b) bs
  mapM_ (\b -> boxPackStart box b PackNatural 0) buttons
  return (box
         ,fmap fst . find snd . zip (map fst l)
           <$> mapM toggleButtonGetActive buttons
         ,\x -> zipWithM_ (\x' b -> when (x == x') (toggleButtonSetActive b True)) (map fst l) buttons
         )

intWidget :: Int -> Int -> IO (SpinButton, IO Int, Int -> IO ())
intWidget lo hi = do
  b <- spinButtonNewWithRange (fromIntegral lo) (fromIntegral hi) 1
  return (b
         ,round <$> spinButtonGetValue b
         ,spinButtonSetValue b . fromIntegral
         )

----------------------------------------------------------------

labelledAccessor :: WidgetValue a w => String -> IO (HBox, IO (Maybe a), a -> IO ())
labelledAccessor label = do
  box <- hBoxNew False 5
  l <- labelNew (Just label)
  (w, get, set) <- makeWidget
  containerAdd box l
  containerAdd box w
  return (box, get, set)

