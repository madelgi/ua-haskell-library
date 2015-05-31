-------------------------------------------------------------------------------
-- Module    : UrbanAirship.Push.Base
-- Maintener : maxdelgiudice@gmail.com
-- Stability : Experimental
-- Summary   : Contains the support routines for sending pushes through the
--             `/api/push/` endpoint.
--
-------------------------------------------------------------------------------
-- {{{ Module declaration and imports
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module UrbanAirship.Push.Base
   ( Push(..)
   , Notification(..)
   , DeviceTypes(..)
   , DeviceType(..)
   , sendPush
   ) where


import           Control.Monad.Error         ( MonadIO )
import           Data.ByteString.Lazy.Char8  ( unpack )
import qualified Data.Text as T
import           Data.Aeson
import           Network.HTTP.Types          ( StdMethod(..) )
import           UrbanAirship.Base           ( AirshipT(..), AConfig(..), ARequest(..), initAReq
                                             , query_, runAirshipT, AResponseCode(..)
                                             )
import           UrbanAirship.Utils          ( omitNulls )
import qualified UrbanAirship.Push.Audience as PA
-- }}}

-------------------------------------------------------------------------------
-- Data Types and ToJSON instances
--
-------------------------------------------------------------------------------

-- | Represents a device type.
data DeviceType = Ios
                | Android
                | Amazon
                | Wns
                | Mpns
                | Blackberry
                  deriving ( Show )

-- | Represents the `device_types` key-value pair.
data DeviceTypes = All
                 | DTs [DeviceType]
                   deriving ( Show )

-- | Represents the notification object. Currently only supports the `alert` key.
--   (TODO update)
data Notification = Notification
   { alert   :: T.Text
   } deriving ( Show )

-- | Represents the push object.
data Push = Push
   { notification    :: Notification
   , audience        :: PA.Audience
   , deviceTypes     :: DeviceTypes
   } deriving ( Show )

instance ToJSON Push where
   toJSON (Push alert audience dts) =
      object [ "notification" .= alert
             , "audience"     .= audience
             , "device_types" .= dts
             ]

instance ToJSON Notification where
   toJSON (Notification a) =
      object [ "alert" .= a ]

instance ToJSON DeviceType where
   toJSON v =
      case v of
         Ios        -> String "ios"
         Android    -> String "android"
         Amazon     -> String "amazon"
         Wns        -> String "wns"
         Mpns       -> String "mpns"
         Blackberry -> String "blackberry"

instance ToJSON DeviceTypes where
   toJSON v = case v of
      All    -> String "all"
      DTs xs -> toJSON xs

-------------------------------------------------------------------------------
-- Create a push request.
--
-------------------------------------------------------------------------------

-- | Send an API request to the `/api/push/` endpoint.
sendPush :: MonadIO m => Push -> AirshipT m ()
sendPush psh = query_ (pushRq []) { aMethod = POST, aData = T.pack . unpack $ encode psh }

-- | Builds the base request for a call to the `/api/push/` endpoint.
pushRq :: [T.Text] -> ARequest
pushRq pcs = initAReq { aEndpoint = "push":pcs }
