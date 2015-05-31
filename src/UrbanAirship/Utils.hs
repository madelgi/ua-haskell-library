-------------------------------------------------------------------------------
-- Module    : UrbanAirship.Utils
-- Maintener : maxdelgiudice@gmail.com
-- Stability : Experimental
-- Summary   : A module containing utility functions, mostly related to parsing
--             JSON.
--
-------------------------------------------------------------------------------

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
