-- module Json where

-- import Game
-- import Card

-- import qualified Data.Char as C
-- import Data.Text
-- import Data.Aeson
-- import Data.Aeson.Types
-- import Data.Aeson.Encoding
-- import GHC.Generics

-- toLowerOptions :: Options
-- toLowerOptions = defaultOptions { constructorTagModifier = fmap C.toLower }

-- instance ToJSON Tool where
--   toJSON = genericToJSON toLowerOptions

-- instance ToJSON ActionCard where
--   toEncoding (BrokenToolCard t)          = pairs $ "value" .= t       <> "type" .= ("brokentoolcard" :: Text)
--   toEncoding (RepairToolCard t)          = pairs $ "value" .= t       <> "type" .= ("repairtoolcard" :: Text)
--   toEncoding (RepairDoubleToolCard t t') = pairs $ "value" .= [t, t'] <> "type" .= ("repairtoolcard" :: Text)
--   toEncoding  RockFallCard               = pairs $ "type"  .= ("rockfallcard" :: Text)
--   toEncoding  MapCard                    = pairs $ "type"  .= ("mapcard" :: Text)

-- instance ToJSON PathCardType where
--   toJSON = genericToJSON toLowerOptions

-- instance ToJSON PathCard where
--   toJSON PathCard { pathCardType = t, north = n, east = e, south = s, west = w, rotated = r } =
--     toJSON [serializePathCardType t, boolToChar n, boolToChar e, boolToChar s, boolToChar w, boolToChar r]
--   toEncoding = toEncoding . toJSON


-- -- instance ToJSON ActionCardType where
-- --   toJSON (BrokenToolCard t) = undefined
-- --   toJSON (RepairDoubleToolCard t t') = undefined
-- --   toJSON (RepairToolCard t) = undefined∏
-- --   toJSON RockFallCard = pairs ("type" .= "actioncardtype" <> "value" .= "rockfallcard")
-- --   toJSON MapCard = undefined -- object ["type" .= "actioncardtype", "mapcard"]

-- -- instance ToJSON ActionCard where
--   -- toJSON (ActionCard t) = object ["type" .= "actioncard", "value" .= "hello"]

-- -- instance ToJSON Game where
-- --   toJSON Game

-- -- jsonToString :: JSON -> Maybe String

-- -- types:
-- -- 0 ConnectedCard
-- -- 1 DeadEndCard
-- -- 2 StartCard
-- -- 3 GoldCard | StoneCard

-- serializePathCardType :: PathCardType -> Char
-- serializePathCardType StartCard     = '0'
-- serializePathCardType ConnectedCard = '1'
-- serializePathCardType DeadEndCard   = '2'
-- serializePathCardType _             = '3'

-- boolToChar :: Bool -> Char
-- boolToChar True  = '1'
-- boolToChar False = '0'
