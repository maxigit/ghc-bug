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
  } deriving Show

data Match = Match

shippingForm :: _ => Maybe ShippingForm 
              -> _Html -> MForm _Handler (FormResult ShippingForm, _Widget)
shippingForm ship  extra =  do
    customerName <- mreq textField "Customer Name" (ship <&> shCustomerName)
    country <- mopt (selectField countryOptions) "Country" (ship <&> shCountry)
    postalCode <- mreq textField "Postal/Zip Code" (ship <&> shPostalCode)
    address1 <- mreq textField "Address 1" (ship <&> shAddress1)
    address2 <- mopt textField "Address 2" (ship <&> shAddress2)
    city <- mreq textField "City" (ship <&> shCity)
    countyState <- mopt textField "County/State" (ship <&> shCountyState)
    contact <- mreq textField "Contact" (ship <&> shContact)
    telephone <- mreq textField "Telephone" (ship <&> shTelephone)
    notificationEmail <- mopt textField "Notification Email" (ship <&> shNotificationEmail)
    notificationText <- mopt textField "Notification Text" (ship <&>  shNotificationText)
    noOfPackages <- mreq intField "No of Packages" (ship <&> shNoOfPackages)
    weight <- mreq doubleField "Weight" (ship <&> shWeight)
    generateCustomData <- mreq boolField "Custom Data" (ship <&> shGenerateCustomData)
    taxId <- mopt textField "EORI" (ship <&>  shTaxId)
    serviceCode <- mreq textField "Service" (ship <&> shServiceCode)
    let widget = return ()

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
          , widget)

  where
  countryOptions = optionsPairs $ map (fanl (p . readableCountryName)) [minBound..maxBound]
  -- help type inference
  p :: String -> Text
  p = pack

fanl f x= (f x , x)
