{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module ChannelStatus.Database where

import qualified Data.Aeson                 as JSON
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Scientific
import           Data.Text
import           Data.Time
import           Data.UUID
import           GHC.Int
import           GHC.Generics
import           Opaleye hiding (fromNullable)

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: storepathcontents ----

data Storepathcontent' c1 c2 c3 c4 c5 c6 c7 =
  Storepathcontent
    { storepathcontentStorepath :: c1
    , storepathcontentSubpath :: c2
    , storepathcontentFile :: c3
    , storepathcontentType :: c4
    , storepathcontentIsexecutable :: c5
    , storepathcontentFilesize :: c6
    , storepathcontentTarget :: c7
    } deriving (Generic)


type Storepathcontent = Storepathcontent' Int32 Text Text Int32 Bool (Maybe Int32) (Maybe Text)

type StorepathcontentReadColumns = Storepathcontent' (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4) (Column PGBool) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type StorepathcontentWriteColumns = Storepathcontent' (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4) (Column PGBool) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText)))

type StorepathcontentNullableColumns = Storepathcontent' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGBool)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type StorepathcontentNullable = Storepathcontent' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Bool) (Maybe Int32) (Maybe Text)

fromNullableStorepathcontent :: StorepathcontentNullable -> Maybe Storepathcontent
fromNullableStorepathcontent = fromNullable

$(makeAdaptorAndInstance "pStorepathcontent" ''Storepathcontent')

storepathcontentTable :: Table StorepathcontentWriteColumns StorepathcontentReadColumns
storepathcontentTable = Table "storepathcontents" (pStorepathcontent
  Storepathcontent
    { storepathcontentStorepath = required "storepath"
    , storepathcontentSubpath = required "subpath"
    , storepathcontentFile = required "file"
    , storepathcontentType = required "type"
    , storepathcontentIsexecutable = required "isexecutable"
    , storepathcontentFilesize = optional "filesize"
    , storepathcontentTarget = optional "target"
    }
  )

---- Types for table: storepaths ----

data Storepath' c1 c2 =
  Storepath
    { storepathId :: c1
    , storepathPath :: c2
    } deriving Generic

type Storepath = Storepath' Int32 Text

type StorepathReadColumns = Storepath' (Column PGInt4) (Column PGText)

type StorepathWriteColumns = Storepath' (Maybe (Column PGInt4)) (Column PGText)

type StorepathNullableColumns = Storepath' (Column (Nullable PGInt4)) (Column (Nullable PGText))

type StorepathNullable = Storepath' (Maybe Int32) (Maybe Text)

fromNullableStorepath :: StorepathNullable -> Maybe Storepath
fromNullableStorepath = fromNullable

$(makeAdaptorAndInstance "pStorepath" ''Storepath')

storepathTable :: Table StorepathWriteColumns StorepathReadColumns
storepathTable = Table "storepaths" (pStorepath
  Storepath
    { storepathId = optional "id"
    , storepathPath = required "path"
    }
  )

