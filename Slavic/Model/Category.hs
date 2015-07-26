module Slavic.Model.Category (
    Category(..)
) where

import ClassyPrelude
import Database.Persist.Sql
import Database.Persist.TH

data Category =
      ZgodnoscZTematem
    | Jakosc
    | Innowacyjnosc
    | Grywalnosc
    deriving (Show, Eq, Read, Enum, Bounded)
derivePersistField "Category"
