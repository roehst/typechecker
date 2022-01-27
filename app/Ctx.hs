module Ctx (Ctx, ctxNew, ctxLookup, ctxPop, ctxPush) where

import Ty

newtype Ctx = Ctx {unCtx :: [(String, Ty)]} deriving (Eq, Show)

ctxNew :: Ctx
ctxNew = Ctx []

ctxPush :: String -> Ty -> Ctx -> Ctx
ctxPush x t (Ctx ctx) = Ctx ((x, t) : ctx)

ctxPop :: Ctx -> Ctx
ctxPop (Ctx ctx) = Ctx (tail ctx)

ctxLookup :: String -> Ctx -> Maybe Ty
ctxLookup x (Ctx ctx) = lookup x ctx