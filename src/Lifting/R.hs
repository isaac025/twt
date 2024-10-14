{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lifting.R where

import Data.Coerce (Coercible (..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Product (..), Sum (..))
