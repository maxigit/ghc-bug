{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (liftM)
import Control.Monad.Trans.RWS.Lazy -- check how strict behaves
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString (ByteString)
import Data.Monoid (Any (..))
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import System.Environment (getEnv)
import Data.ISO3166_CountryCodes
import qualified Data.Map as Map


type Handler = ReaderT () IO
type MForm = RWST (Maybe ([(String, Text)], ()), (), ()) Any [Int]
type Text = ByteString -- close enough

data FormResult a = FormMissing
                  | FormFailure [Text]
                  | FormSuccess a
    deriving Show
instance Functor FormResult where
    fmap _ FormMissing = FormMissing
    fmap _ (FormFailure errs) = FormFailure errs
    fmap f (FormSuccess a) = FormSuccess $ f a
instance Applicative FormResult where
    pure = FormSuccess
    (FormSuccess f) <*> (FormSuccess g) = FormSuccess $ f g
    (FormFailure x) <*> (FormFailure y) = FormFailure $ x ++ y
    (FormFailure x) <*> _ = FormFailure x
    _ <*> (FormFailure y) = FormFailure y
    _ <*> _ = FormMissing
instance Monoid m => Monoid (FormResult m) where
    mempty = pure mempty
    mappend x y = mappend <$> x <*> y
instance Semigroup m => Semigroup (FormResult m) where
    x <> y = (<>) <$> x <*> y

mreq :: MonadIO m => String -> MForm m (FormResult Text, ())
-- fast
--mreq v = pure (FormFailure [], ())
-- slow
mreq v = mhelper v (\m l -> FormFailure ["fail"]) FormSuccess
mcountry :: MonadIO m => String -> MForm m (FormResult CountryCode, ())
mcountry v = mhelper v (\m l -> FormFailure ["fail"]) go where
  go t = let
    m = Map.fromList $ map (fanl $ fromString . readableCountryName) [minBound..maxBound]
    in maybe (FormFailure ["fail"]) FormSuccess $ Map.lookup t m

askParams :: Monad m => MForm m (Maybe [(String, Text)])
askParams = do
    (x, _, _) <- ask
    return $ liftM fst x

mhelper
    :: MonadIO m
    => String
    -> (() -> () -> FormResult b) -- on missing
    -> (Text -> FormResult b)      -- on success
    -> MForm m (FormResult b, ())
mhelper v onMissing onFound = do
    -- without tell, also faster
    tell (Any True)
    -- with different "askParams": faster.
    -- mp <- liftIO $ read <$> readFile v
    mp <- askParams
    (res, x) <- case mp of
        Nothing -> return (FormMissing, ())
        Just p -> do
            return $ case lookup v p of
                Nothing -> (onMissing () (), ())
                Just t -> (onFound t, ())
    return (res, x)
     
data ShippingForm = ShippingForm
  { shCustomerName :: CountryCode
  , shCountry :: CountryCode -- mandatory but we need to not have a default value
  , shPostalCode :: CountryCode
  , shAddress1 :: CountryCode
  , shAddress2 :: CountryCode
  , shCity :: CountryCode
  , shCountyState :: CountryCode
  , shContact :: CountryCode
  , shTelephone :: CountryCode
  , shNotificationEmail :: CountryCode
  , shNotificationCountryCode :: CountryCode
  , shNoOfPackages :: CountryCode
  , shWeight :: CountryCode
  , shGenerateCustomData :: CountryCode
  , shTaxId :: CountryCode
  -- , shCustomValue ::  Double
  , shServiceCode :: CountryCode
  } deriving Show

data Match = Match

shippingForm :: Maybe ShippingForm 
              -> MForm Handler (FormResult ShippingForm)
shippingForm ship  =  do
    customerName <- mcountry "Customer Name"  -- (ship <&> shCustomerName)
    country <- mcountry  "Country"  -- (ship <&> shCountry)
    postalCode <- mcountry "Postal/Zip Code"  -- (ship <&> shPostalCode)
    address1 <- mcountry "Address 1"  -- (ship <&> shAddress1)
    address2 <- mcountry "Address 2"  -- (ship <&> shAddress2)
    city <- mcountry "City"  -- (ship <&> shCity)
    countyState <- mcountry "County/State"  -- (ship <&> shCountyState)
    contact <- mcountry "Contact"  -- (ship <&> shContact)
    telephone <- mcountry "Telephone"  -- (ship <&> shTelephone)
    notificationEmail <- mcountry "Notification Email"  -- (ship <&> shNotificationEmail)
    notificationText <- mcountry "Notification Text"  -- (ship <&>  shNotificationText)
    noOfPackages <- mcountry "No of Packages"  -- (ship <&> shNoOfPackages)
    weight <- mcountry "Weight"  -- (ship <&> shWeight)
    generateCustomData <- mcountry "Custom Data"  -- (ship <&> shGenerateCustomData)
    taxId <- mcountry "EORI"  -- (ship <&>  shTaxId)
    serviceCode <- mcountry "Service"  -- (ship <&> shServiceCode)
    return (ShippingForm <$> fst  customerName
                 <*> fst  country
                 <*> fst  postalCode
                 <*> fst  address1
                 <*> fst  address2
                 <*> fst  city
                 <*> fst  countyState
                 <*> fst  contact
                 <*> fst  telephone
                 <*> fst  notificationEmail
                 <*> fst  notificationText
                 <*> fst  noOfPackages
                 <*> fst  weight
                 <*> fst  generateCustomData
                 <*> fst  taxId
                 <*> fst  serviceCode
          )

fanl f x= (f x , x)

main :: IO ()
main = pure ()

