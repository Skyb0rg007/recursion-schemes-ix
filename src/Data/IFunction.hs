{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Data.IFunction
    ( type (~~>)
    ) where

import           Singlethongs (SingI)

infixr 4 ~~>

-- | Indexed function type
type a ~~> b = forall ix. SingI ix => a ix -> b ix

