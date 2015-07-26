module Slavic.Model.Voting where

import ClassyPrelude.Slavic
import Slavic.Model

type Round = Int

getCurrentRound :: SqlM (Maybe Round)
getCurrentRound = map (currentRoundRound . entityVal) <$> selectFirst [] []
