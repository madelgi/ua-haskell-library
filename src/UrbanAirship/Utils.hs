module UrbanAirship.Utils
   ( omitNulls
   ) where

import qualified Data.Aeson as A
import qualified Data.Text  as T
-------------------------------------------------------------------------------
-- JSON Parsing
--
-------------------------------------------------------------------------------
omitNulls :: [(T.Text, A.Value)] -> A.Value
omitNulls = A.object . filter notNull where
   notNull (_, A.Null) = False
   notNull  _        = True
