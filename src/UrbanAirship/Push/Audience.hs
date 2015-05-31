{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
-- Module: UrbanAirship.Push.Audience
--
--------------------------------------------------------------------------------
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
data Audience = AND [Audience]      -- ^ A list of audience constructors, connected with an AND clause
              | OR  [Audience]      -- ^ A list of audience constructors, connected with an OR clause
              | NOT Audience        -- ^ Negation of a given audience constructor
              | AS  AtomicSelector  -- ^ An atomic selector
                deriving ( Show )

data AtomicSelector = All                 -- ^ Your entire audience
                    | IosChannel     Text -- ^ An iOS Channel
                    | AndroidChannel Text -- ^ An Android Channel
                    | AmazonChannel  Text -- ^ An Amazon Channel
                    | DeviceToken    Text -- ^ A Device Token
                    | DevicePin      Text -- ^ A BlackBerry PIN
                    | Apid           Text -- ^ An APID
                    | Wns            Text -- ^ Unique identifier used to target a Windows device
                    | Mpns           Text -- ^ Unique identifier used to target a Windows Phone device
                    | Tag            Text -- ^ A Tag
                    | Alias          Text -- ^ TODO
                    | Segment        Text -- ^ TODO
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
