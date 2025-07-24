{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,18,0)
#else
{-# LANGUAGE RoleAnnotations #-}
#endif
{-| The t'ConstPtr' type was introduced in GHC 9.6.1. However, it is required
  when writing C bindings to a function involving the const annotation using
  the CApi calling convention (see 'Foreign.C.ConstPtr.ConstPtr').
  This module defines t'ConstPtr' for compatibility with earlier versions of
  GHC. Unfortunately, older versions of GHC do not emit the 'const' qualifier
  when emitting the C header files. Therefore, it is also necessary to add
  the following conditional option to your Cabal file:

  @
  if impl(ghc <9.6.1)
    ghc-options:
      -optc=-Wno-discarded-qualifiers
      -optc=-Wno-incompatible-pointer-types-discards-qualifiers
  @
-}
module Foreign.C.ConstPtr.Compat (
  ConstPtr (..),
) where

#if MIN_VERSION_base(4,18,0)
import Foreign.C.ConstPtr (ConstPtr(..))
#else
import Foreign.Ptr (Ptr)

type role ConstPtr phantom
newtype ConstPtr a = ConstPtr { unConstPtr :: Ptr a }
    deriving (Eq, Ord)

-- doesn't use record syntax
instance Show (ConstPtr a) where
    showsPrec d (ConstPtr p) = showParen (d > 10) $ showString "ConstPtr " . showsPrec 11 p
#endif
