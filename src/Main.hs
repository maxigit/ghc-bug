{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures, NamedWildCards #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main
( shippingForm
) where

import ClassyPrelude.Yesod
import Yesod.Form.Bootstrap3
import Data.ISO3166_CountryCodes
import qualified Data.Map as Map
import Data.Text(Text, strip)


data ShippingForm = ShippingForm
  { shCustomerName :: Text
  , shCountry :: Maybe CountryCode -- mandatory but we need to not have a default value
  , shPostalCode :: Text
  , shAddress1 :: Text
  , shAddress2 :: Maybe Text
  , shCity :: Text
  , shCountyState :: Maybe Text
  , shContact :: Text
  , shTelephone :: Text
  , shNotificationEmail :: Maybe Text
  , shNotificationText :: Maybe Text
  , shNoOfPackages :: Int
  , shWeight :: Double
  , shGenerateCustomData :: Bool
  , shTaxId :: Maybe Text
  -- , shCustomValue ::  Double
  , shServiceCode :: Text
  , shSave :: Bool
  } deriving Show

data Match = Match

shippingForm :: _ => Maybe ShippingForm 
              -> _Html -> MForm _Handler (FormResult ShippingForm, _Widget)
shippingForm (shipm)  extra =  do
    customerName <- mreq textField (f 35 "Customer Name") (ship <&> take 35 . shCustomerName)
    country <- mopt (selectField countryOptions) "Country" (ship <&> shCountry)
    postalCode <- mreq textField (f 7 "Postal/Zip Code") (ship <&> take 7 . shPostalCode)
    address1 <- mreq textField (f 35 "Address 1") (ship <&> take 35 . shAddress1)
    address2 <- mopt textField (f 35 "Address 2") (ship <&> fmap (take 35) . shAddress2)
    city <- mreq textField (f 35 "City") (ship <&> take 35 . shCity)
    countyState <- mopt textField (f 35 "County/State") (ship <&> fmap (take 35) . shCountyState)
    contact <- mreq textField (f 25 "Contact") (ship <&> take 25 . shContact)
    telephone <- mreq textField (f 15 "Telephone") (ship <&> take 15 .  shTelephone)
    notificationEmail <- mopt textField (f 35 "Notification Email") (ship <&> fmap (take 35) . shNotificationEmail)
    notificationText <- mopt textField (f 35 "Notification Text") (ship <&>  fmap (take 35) .shNotificationText)
    noOfPackages <- mreq intField "No of Packages" (ship <&> shNoOfPackages)
    weight <- mreq doubleField "Weight" (ship <&> shWeight)
    generateCustomData <- mreq boolField "Custom Data" (ship <&> shGenerateCustomData)
    taxId <- mopt textField (f 14 "EORI") (ship <&>  fmap (take 35) .shTaxId)
    serviceCode <- mreq textField "Service" (ship <&> shServiceCode)
    save@(_, saveView) <- mreq boolField "Save" (ship <&> shSave)
    let widget = return ()
        normalize = toLower . strip

    return (ShippingForm <$> fst customerName
                 <*> fst country
                 <*> fst postalCode
                 <*> fst address1
                 <*> fst address2
                 <*> fst city
                 <*> fst countyState
                 <*> fst contact
                 <*> fst telephone
                 <*> fst notificationEmail
                 <*> fst notificationText
                 <*> fst noOfPackages
                 <*> fst weight
                 <*> fst generateCustomData
                 <*> fst taxId
                 <*> fst serviceCode
                 <*> fst save
          , widget)

  where
  ship = truncateForm <$> shipm
  countryOptions = optionsPairs $ map (fanl (p . readableCountryName)) [minBound..maxBound]
  -- fixed length text
  f n f = f { fsAttrs=[("maxlength", tshow n)] }
  -- help type inference
  p :: String -> Text
  p = pack

fanl f x= (f x , x)

joinSpaces = id
    
-- | Truncate each fields of a form to its max length
truncateForm :: ShippingForm -> ShippingForm
truncateForm ShippingForm{..} =
  ShippingForm (take 35 shCustomerName)
               (shCountry)
               (take 7 $ joinSpaces shPostalCode)
               (take 35 shAddress1)
               (fmap (take 35) shAddress2)
               (take 35 shCity)
               (fmap (take 35) shCountyState)
               (take 25 shContact)
               (take 15  shTelephone)
               (fmap (take 35 . joinSpaces) shNotificationEmail)
               ( fmap (take 35 . joinSpaces)shNotificationText)
               (shNoOfPackages)
               (shWeight)
               (shGenerateCustomData)
               ( fmap (take 35)shTaxId)
               (shServiceCode)
               (shSave)
