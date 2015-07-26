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

categoryDescription :: Category -> Text
categoryDescription ZgodnoscZTematem = "Zgodność z tematem"
categoryDescription Jakosc = "Jakosc"
categoryDescription Innowacyjnosc = "Innowacyjnosc"
categoryDescription Grywalnosc = "Grywalnosc"
