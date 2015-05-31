-------------------------------------------------------------------------------
-- Module    : UrbanAirship.Base
-- Maintener : maxdelgiudice@gmail.com
-- Stability : Experimental
-- Summary   : The base client module -- contains data types and support routines
--             for querying Urban Airship API v3.
--
-------------------------------------------------------------------------------
-- {{{ Module declaration and imports
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module UrbanAirship.Base
   ( AConfig(..)
   , AResponseCode(..)
   , AFailure(..)
   , ARequest(..)
   , Airship
   , AirshipT(AirshipT)
   , runAirshipT
   , initAReq
--   , query
   , query_
   , StdMethod(..)      -- Re-export

   {- for testing -}
   , statusCode
   , prepRq
   , queryOptions
   ) where

import           Control.Applicative
import           Control.Arrow                      ((***))
import           Control.Monad                      ( MonadPlus, liftM, mzero )
import           Control.Monad.Error                ( Error, ErrorT, MonadIO, MonadError
                                                    , runErrorT, throwError
                                                    , strMsg, noMsg
                                                    )
import           Control.Monad.State                ( MonadState, StateT, runStateT, get )
import           Control.Monad.Trans                ( liftIO )
import           Data.Char                          ( toLower )
import           Data.List                          ( intercalate )
import           Data.Text                          ( Text )
import           Data.ByteString.Base64             ( encode )
import           Data.ByteString.Char8              ( pack, unpack )
import qualified Data.ByteString.Lazy               as BL
import qualified Data.HashMap.Lazy                  as HML
import           Network.Curl                       ( CurlOption(..), CurlResponse
                                                    , CurlResponse_(..), curlGetResponse_
                                                    , method_GET, method_HEAD, method_POST
                                                    )
import           Network.HTTP.Types                 ( StdMethod(..) )
import           Network.URI                        ( URI(..), URIAuth(..)
                                                    , uriToString )

import qualified Data.Text  as T
import qualified Data.Aeson as A

-- }}}

-------------------------------------------------------------------------------
-- HTTP defaults
--
-------------------------------------------------------------------------------
acceptHeader      = "Accept: application/vnd.urbanairship+json; version=3"
contentTypeHeader = "Content-Type: application/json"
httpHeaders       = [acceptHeader, contentTypeHeader]

-------------------------------------------------------------------------------
-- Data Types
--
-------------------------------------------------------------------------------

-- | AConfig holds the required Urban Airship authorization data.
data AConfig = AConfig
   { appKey       :: Text
   , masterSecret :: Text
   } deriving (Show)

-- | Represents the possible successes that a connection to the Urban Airship
--   API may encounter.
data AResponseCode = OK
                   | Unknown Int
                   deriving (Show)

-- | Represents the possible failures that a connection to the UA API may
--   encounter.
data AFailure = MovedPermanently
              | BadRequest
              | Unauthorized
              | NotFound
              | UnsupportedMedia
              | NotAcceptable
              | InternalServerError
              | BadGateway
              | ServiceUnavailable
              | GatewayTimeout
              | OtherFailure        (Maybe Text)
              deriving (Show)

-- | Represents the information contained in a request to the Urban Airship API.
data ARequest = ARequest
   { aMethod      :: StdMethod -- ^ The HTTP method (one of `GET`, `PUT`, `DELETE`, or `POST`)
   , aEndpoint    :: [Text]    -- ^ The desired API endpoint
   , aData        :: Text
   , aBody        :: Text
   } deriving (Show)

-------------------------------------------------------------------------------
-- The Airship Monad
--
-------------------------------------------------------------------------------

-- | A specialization of the AirshipT monad transformer with IO as the inner
--   monad
type Airship a = AirshipT IO a

-- | AirshipT is the base monad transformer where all Urban Airship API requests
--   occur
newtype AirshipT m a = AirshipT
   { unAirshipT :: StateT AConfig (ErrorT AFailure m) a
   } deriving ( Functor, Monad, MonadIO, MonadPlus
              , MonadError AFailure
              , MonadState AConfig
              , Alternative
              , Applicative
              )

-- | The airship run function
runAirshipT :: MonadIO m => AConfig -> AirshipT m a -> m (Either AFailure a)
runAirshipT config airship =
  runErrorT . liftM fst . (`runStateT` config) .  unAirshipT $ airship

-------------------------------------------------------------------------------
-- Tools for querying the Urban Airship API.
--
-------------------------------------------------------------------------------

-- | The basic request upon which all UA API calls will be built. We customize
--   the request by overriding one or more of the contained fields. E.g., to
--   hit the Push API endpoint:
--
-- > initAReq { aEndpoint = ["push"] }
initAReq :: ARequest
initAReq  = ARequest
   { aMethod      = GET
   , aEndpoint    = []
   , aData        = ""
   , aBody        = ""
   }

-- | Query the Urban Airship API.
--
-- > let config = AConfig "appKey" "masterSecret"
-- > runAirshipT config $
-- >    query' initAReq { aDestination = ["push"] }
query' :: MonadIO m => ARequest -> AirshipT m (AResponseCode, String)
query' request = do
   config   <- get
   response <- liftIO (curlGetResponse_ (uriToString id (prepRq config request) $ "") opts :: IO CurlResponse)
   code     <- statusCode (respStatus response) (respBody response)
   return (code, respBody response)
   where
      opts        = CurlFailOnError False : queryOptions request

-- | Same as query, but ignore the response code
query_ :: MonadIO m => ARequest -> AirshipT m ()
query_ req = query' req >> return ()

-- | Add headers, agent, other options to cURL request
queryOptions :: ARequest -> [CurlOption]
queryOptions request = CurlUserAgent agent : CurlHttpHeaders httpHeaders
                           : CurlPostFields [T.unpack $ aData request] : mopts
   where agent = "ua-haskell-library/0.1 libcurl"
         mopts = case aMethod request of
                   GET      -> method_GET
                   POST     -> method_POST
                   HEAD     -> method_HEAD
                   PUT      -> [CurlCustomRequest "PUT"]
                   DELETE   -> [CurlCustomRequest "DELETE"]

-- | Takes an AConfig and an ARequest, and returns a valid URI.
prepRq :: AConfig -> ARequest -> URI
prepRq config request = uri { uriPath = (intercalate "/" endpt) }
   where endpt = (uriPath uri) : (map T.unpack $ aEndpoint request)
         uri = baseURI . T.unpack $ T.concat [appKey config, ":", masterSecret config]

-- | Creates the default URI.
--
-- > show $ baseURI "appKey:masterSecret"
-- https://appKey:...@go.urbanairship.com:443/api
baseURI :: String -> URI
baseURI k = URI
   { uriScheme    = "https:"
   , uriAuthority = Just $ URIAuth (k ++ "@") "go.urbanairship.com" ":443"
   , uriPath      = "/api"
   , uriQuery     = ""
   , uriFragment  = ""
   }

-------------------------------------------------------------------------------
-- Error Handling
--
-------------------------------------------------------------------------------
instance Error AFailure where
   noMsg  = OtherFailure Nothing
   strMsg = OtherFailure . Just . T.pack

statusCode :: Monad m => Int -> String -> AirshipT m AResponseCode
statusCode c body = case c of
   200 -> return OK
   301 -> throwError MovedPermanently
   400 -> throwError BadRequest
   401 -> throwError Unauthorized
   404 -> throwError NotFound
   406 -> throwError NotAcceptable
   415 -> throwError UnsupportedMedia
   500 -> throwError InternalServerError
   502 -> throwError BadGateway
   503 -> throwError ServiceUnavailable
   504 -> throwError GatewayTimeout
   _   -> return $ Unknown c
