module Term where

import Ty

data Term
  = Var String
  | Lam String Ty Term
  | App Term Term
  deriving (Eq)

instance Show Term where
  showsPrec prec term = case term of
    Var x -> showString x
    Lam x ty body ->
      showParen (prec > precLam) $
        showString "\\"
          . showString x
          . showString " -> "
          . showsPrec precLam body
    App f x -> showParen (prec > precApp) $ showsPrec precApp f . showString " " . showsPrec precApp x
    where
      precLam = 1
      precApp = 2