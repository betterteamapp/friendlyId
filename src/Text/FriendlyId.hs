{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Text.FriendlyId (getFriendlyId) where

import           Data.FileEmbed         (embedFile)
import           Data.List              (intercalate)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8)
import           Prelude
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random (mkQCGen)

data FriendlyId = FriendlyId Text Text Text Text

friendlyChunk :: Gen Text
friendlyChunk = elements dictList

dictList = T.lines $ decodeUtf8 dictionary
dictionary = $(embedFile "./words")

friendlyIdGen :: Gen FriendlyId
friendlyIdGen = FriendlyId <$> friendlyChunk
                           <*> friendlyChunk
                           <*> friendlyChunk
                           <*> friendlyChunk

instance Show FriendlyId where
  show (FriendlyId a b c d) = T.unpack $ T.intercalate "-" [a,b,c,d]

getFriendlyId :: Int -> FriendlyId
getFriendlyId i = unGen friendlyIdGen (mkQCGen i) 12
