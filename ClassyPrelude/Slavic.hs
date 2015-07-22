-- | Local Prelude with some utilities
module ClassyPrelude.Slavic (
    module ClassyPrelude
  , module Control.Monad.Except
  , module ClassyPrelude.Slavic
) where

import ClassyPrelude
import Control.Monad.Except (runExceptT, throwError, catchError, MonadError)


-- | Extract a monadic Maybe value, throwing an error on Nothing.
-- Intended to be used infix.
orThrow :: MonadError e m => m (Maybe a) -> e -> m a
orThrow val err = val >>= maybe (throwError err) return
