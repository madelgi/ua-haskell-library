# Urban Airship Haskell Library

## About

This is an Unofficial Haskell wrapper for
[Urban Airship API v3](http://docs.urbanairship.com/api/ua.html).


## Functionality

Currently, the wrapper is push-only.

## Usage

### Installation

Before installing this package, you need the development files for `curl`. If you are using Ubuntu, you can execute:

```
$ sudo apt-get install libcurl4-gnutls-dev
```

To install the `ua-haskell-library`, execute the following commands.

```
$ git clone https://github.com/madelgi/ua-haskell-library
$ cd ua-haskell-library
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal repl
```

This will start up an instance of GHCI with the Haskell library.


### Example Request

Replace `your_appkey_here` and `your_mastersecret_here` items with your actual app key and master secret.

```haskell
module TestPush where

import           Network.Curl
import           Network.URI                  ( uriToString )
import           Data.Aeson                   ( encode )
import qualified Data.Text as T
import           Data.ByteString.Lazy.Char8   ( unpack )
import           UrbanAirship.Base
import           UrbanAirship.Push.Base
import           UrbanAirship.Push.Audience

-- | Authorization details
conf = AConfig
   { appKey = "your_appkey_here"
   , masterSecret = "your_mastersecret_here"
   }

-- | Specifies the `audience`, `device_types`, and `notification` parameters
pizza    = AS (Tag "pizza")
droidIos = DTs [ Android, Ios ]
msg      = Notification { alert = "hi from v0.1 of the urban airship haskell lib" }

-- | Builds the push notification using the above parameters
push = Push
   { notification = msg
   , audience     = pizza
   , deviceTypes  = droidIos
   }

-- | Creates an API request
request = ARequest
   { aMethod   = POST
   , aEndpoint = ["push"]
   , aData     = T.pack . unpack $ encode push
   , aBody     = ""
   }

-- | Send the push
main :: IO ()
main  = do
   bPush <- runAirshipT conf airship
   either err (putStrLn . show) bPush
   where err _   = putStrLn "Uh oh why won't it work dammit"
         airship = do pushData <- sendPush push
                      return pushData

```

## TODOS

- [ ] Add tests
- [ ] Fix versioning issues
- [ ] Update from Error to Control.Monad.Trans.Except
- [ ] Add other API features
