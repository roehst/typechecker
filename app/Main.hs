{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except (MonadError (throwError), runExcept)
import Control.Monad.Reader
import Control.Monad.Writer
import Types

typecheck :: (MonadReader Ctx m, MonadError String m, MonadWriter [(Term, Ty)] m) => Term -> m Ty
typecheck term = do
  ty <- case term of
    Var name -> do
      ctx <- ask
      case ctxLookup name ctx of
        Nothing ->
          let message = "Unbound variable: " ++ name
           in throwError message
        Just ty -> return ty
    Lam name ty body -> TyArr ty <$> local (ctxPush name ty) (typecheck body)
    App fun arg -> do
      funTy <- typecheck fun
      argTy <- typecheck arg
      case funTy of
        TyArr argTy' retTy ->
          if argTy == argTy'
            then return retTy
            else throwError "Type mismatch"
        _ -> throwError "Type error"
  tell [(term, ty)]
  return ty

identityFn :: Term
identityFn = Lam "w" (TyConst "X") (App (Lam "x" (TyConst "X") (Var "x")) (Var "w"))

main :: IO ()
main =
  let result = runExcept $ runWriterT $ runReaderT (typecheck identityFn) ctxNew
   in case result of
        Left err -> putStrLn err
        Right (_, log) -> do
          let terms = map fst log
           in let types = map snd log
               in let termStrs = map show terms
                   in let typeStrs = map show types
                       in let longestTermStr = maximum $ map length termStrs
                           in let longestTypeStr = maximum $ map length typeStrs
                               in do
                                    let header = lpad longestTermStr ' ' "Terms" ++ "    " ++ "Types"
                                     in do
                                          putStrLn header
                                          putStrLn $ replicate (length header) '-'
                                          forM_ (zip termStrs typeStrs) $ \(te, ty) -> do
                                            putStrLn $ lpad longestTermStr ' ' te ++ "    " ++ ty

lpad :: Int -> Char -> String -> [Char]
lpad padding padder string = string ++ replicate (padding - length string) padder
