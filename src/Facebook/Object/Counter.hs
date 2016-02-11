{-# LANGUAGE OverloadedStrings #-}
module Facebook.Object.Counter where

import Data.Typeable
import Control.Monad (mzero)
import Data.Aeson

data Counter = Counter
    { count :: Int
    } deriving (Eq,Ord,Read,Show,Typeable)

instance FromJSON Counter where
    parseJSON (Object v) = Counter <$> v .: "count"
    parseJSON _ = mzero


instance ToJSON Counter where
    toJSON (Counter count) = object ["count" .= count]
