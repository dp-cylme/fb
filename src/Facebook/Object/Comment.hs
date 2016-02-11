{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Facebook.Object.Comment
    ( Comment(..)
    , getComments
    ) where


import Data.Typeable
import Control.Applicative()
import Control.Monad (mzero)
import Data.Time()
import Data.Text (Text)
import Data.Aeson ((.:), (.:?), object, (.=))
import qualified Data.Aeson as A
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Control (MonadBaseControl)

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.User


-- | A Facebook comment (see
-- <https://developers.facebook.com/docs/reference/api/comments>).
--
-- /NOTE:/ Does not yet support all fields. Please file an issue if
-- you need any other fields
data Comment = Comment
    { commentId :: Id
    , commentFrom :: Maybe User
    , commentMessage :: Maybe Text
    , commentCreatedTime :: Maybe FbUTCTime
    } deriving (Eq,Ord,Show,Read,Typeable)

instance A.FromJSON Comment where
    parseJSON (A.Object v) =
        Comment <$> v .: "id" <*> v .:? "from" <*> v .:? "message" <*>
        v .:? "created_time"
    parseJSON _ = mzero


instance A.ToJSON Comment where
    toJSON (Comment id_ from_ message_ created_time_) =
        object
            [ "id" .= id_
            , "from" .= from_
            , "message" .= message_
            , "created_time" .= created_time_]

-- | Get the list of comments of the given object
getComments
    :: (MonadResource m, MonadBaseControl IO m)
    => Id
    -> [Argument]
    -> AccessToken anyKind
    -> FacebookT anyAuth m (Pager Comment)
getComments id_ query tok =
    getObject ("/" <> idCode id_ <> "/comments") query (Just tok)
