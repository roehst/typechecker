module Ty where

data Ty
  = TyConst String
  | TyArr Ty Ty
  deriving (Eq)

instance Show Ty where
  showsPrec prec ty = case ty of
    TyConst s -> showString s
    TyArr t1 t2 ->
      showParen (prec > 0) $
        showsPrec 1 t1 . showString " -> " . showsPrec 1 t2