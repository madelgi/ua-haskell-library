-------------------------------------------------------------------------------
-- Module    : UrbanAirship.Push.Audience
-- Maintener : maxdelgiudice@gmail.com
-- Stability : Experimental
-- Summary   : Contains the base audience data types and ToJSON instances.
--
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

module UrbanAirship.Push.Audience
   ( Audience(..)
   , AtomicSelector(..)
   ) where

import           Data.Aeson
import           Data.Text           ( Text )

--------------------------------------------------------------------------------
-- Models
--
--------------------------------------------------------------------------------

-- | Represents the `audience` key. Can be a [possibly negated] `AtomicSelector`,
--   or a list of Audience types linked together by a boolean operator (`AND` or
--   `OR`).
data Audience = AND [Audience]
              | OR  [Audience]
              | NOT Audience
              | AS  AtomicSelector
                deriving ( Show )

-- | Represents an atomic selector.
data AtomicSelector = All
                    | IosChannel     Text
                    | AndroidChannel Text
                    | AmazonChannel  Text
                    | DeviceToken    Text
                    | DevicePin      Text
                    | Apid           Text
                    | Wns            Text
                    | Mpns           Text
                    | Tag            Text
                    | Alias          Text
                    | Segment        Text
                      deriving ( Show )

instance ToJSON Audience where
   toJSON v = case v of
      AND a -> object [ "AND" .= toJSON a ]
      OR a  -> object [ "OR"  .= toJSON a ]
      NOT a -> object [ "NOT" .= toJSON a ]
      AS a  -> toJSON a

instance ToJSON AtomicSelector where
   toJSON v = case v of
      All              -> String "all"
      IosChannel s     -> object [ "ios_channel" .= s ]
      AndroidChannel s -> object [ "android_channel" .= s ]
      AmazonChannel s  -> object [ "amazon_chhanel" .= s ]
      DeviceToken s    -> object [ "device_token" .= s ]
      DevicePin s      -> object [ "device_pin" .= s ]
      Apid s           -> object [ "apid" .= s ]
      Wns s            -> object [ "wns" .= s ]
      Tag s            -> object [ "tag" .= s ]
      Mpns s           -> object [ "mpns" .= s ]
      Alias s          -> object [ "alias" .= s ]
