module Types (Ty (..), Term (..), Ctx, ctxNew, ctxLookup, ctxPop, ctxPush) where

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

newtype Ctx = Ctx {unCtx :: [(String, Ty)]} deriving (Eq, Show)

ctxNew :: Ctx
ctxNew = Ctx []

ctxPush :: String -> Ty -> Ctx -> Ctx
ctxPush x t (Ctx ctx) = Ctx ((x, t) : ctx)

ctxPop :: Ctx -> Ctx
ctxPop (Ctx ctx) = Ctx (tail ctx)

ctxLookup :: String -> Ctx -> Maybe Ty
ctxLookup x (Ctx ctx) = lookup x ctx

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