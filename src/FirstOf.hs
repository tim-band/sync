module FirstOf (FirstOf(..)) where

-- import Data.Aeson (FromJSON, parseJSON)
import Control.Applicative ((<$>), (<*>))
    
data FirstOf t = FirstOf [t]
