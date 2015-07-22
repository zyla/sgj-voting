module Slavic.Model.Category (
    Category
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH

-- FIXME proper categories
data Category =
      Graphics
    | Innovation
    | Gameplay
    deriving (Show, Eq, Read, Enum, Bounded)
derivePersistField "Category"
