-- | Form-related utilities
module Slavic.Forms where

import ClassyPrelude.Slavic
import Yesod
import Yesod.Form

mkFieldSettings :: SomeMessage site -- ^ label
                -> Text -- ^ field ID and name
                -> FieldSettings site
mkFieldSettings label name =
    FieldSettings
        { fsLabel = label
        , fsTooltip = Nothing
        , fsId = Just name
        , fsName = Just name
        , fsAttrs = []
        }
