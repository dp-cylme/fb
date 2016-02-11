{-# LANGUAGE OverloadedStrings
           , FlexibleContexts #-}
module Facebook.Object.Post
       (Post(..), getPost, getSharedPosts) where

import Data.Typeable
import Control.Monad (mzero)
import Control.Applicative()
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
import Facebook.Object.Counter


-- | A Facebook post (see
-- <https://developers.facebook.com/docs/reference/api/page>).
--
-- /NOTE:/ Does not yet support all fields. Please file an issue if
-- you need any other fields.
data Post = Post
    { postId :: Id
    , shares :: Maybe Counter
    , postCreatedTime :: Maybe FbUTCTime
    , postType :: Maybe Text
    , postMessage :: Maybe Text
    , postFrom :: Maybe User
    } deriving (Eq,Ord,Show,Read,Typeable)


instance A.FromJSON Post where
    parseJSON (A.Object v) =
        Post <$> v .: "id" <*> v .:? "shares" <*> v .:? "created_time" <*>
        v .:? "type" <*>
        v .:? "message" <*>
        v .:? "from"
    parseJSON _ = mzero


instance A.ToJSON Post where
    toJSON (Post pid sh pct pt pm pf ) =
        object
            [ "id" .= pid
            , "shares" .= sh
            , "created_time" .= pct
            , "type" .= pt
            , "message" .= pm
            , "from" .= pf
            ]


-- | Get a post using its ID.
getPost
    :: (MonadResource m, MonadBaseControl IO m)
    => Id -> [Argument] -> AccessToken anyKind -> FacebookT anyAuth m Post
getPost id_ query tok = getObject ("/" <> idCode id_) query (Just tok)


-- | Get the list of sharedposts of the given post
getSharedPosts
    :: (MonadResource m, MonadBaseControl IO m)
    => Id
    -> [Argument]
    -> AccessToken anyKind
    -> FacebookT anyAuth m (Pager Post)
getSharedPosts id_ query tok =
    getObject ("/" <> idCode id_ <> "/sharedposts") query (Just tok)
