{-# LANGUAGE OverloadedStrings
           , FlexibleContexts #-}
module Facebook.Object.Like (getLikes) where

import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl)

import Facebook.Monad
import Facebook.Types
import Facebook.Pager
import Facebook.Graph
import Facebook.Object.User


-- | Get the list of likes of the given object
getLikes
    :: (MonadResource m, MonadBaseControl IO m)
    => Id
    -> [Argument]
    -> AccessToken anyKind
    -> FacebookT anyAuth m (Pager User)
getLikes id_ query tok =
    getObject ("/" <> idCode id_ <> "/likes") query (Just tok)
