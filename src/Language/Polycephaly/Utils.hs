{-# LANGUAGE
    , TemplateHaskell
 #-}
{-|
Module      : Language.Polycephaly.Utils
Description : Polycephalous-instances code generation
Copyright   : (c) Julian K. Arni, 2014
License     : BSD3
Maintainer  : jkarni@gmail.com
Stability   : experimental
-}

module Language.Polycephaly.Utils where

import Language.Haskell.TH
Data.Generics.Uniplate.Operations

(=/=) :: Quasi m => m [Dec] -> m [Dec] -> Bool
a =/= b =

-- | Canonicalizes a list of declarations. That is, rewrites the
-- declarations so that the semantics of the original declaration is
-- maintained, but all new variables appear in alphabetic order, starting
-- at 'a' and not skipping any letters.
-- Example:
--
-- >>> canonicalVarName [d| b -> b |]
-- LamE [VarP a] (VarE a)
canonicalVarRename :: m [Dec] -> m [Dec]
canonicalVarRename m = let
    withAlphaOrdNames :: Name -> (StateT (Map.Map Name Name) Q Name)
    withAlphaOrdNames k = do
            (m, alph) <- get
            if Map.member k n
                then return $ fromJust $ Map.lookup k n
                else do
                    let nn = head alph
                    put (Map.insert k nn, tail alph)
                    return nn
    in foldl


withFreshNames :: Name -> (StateT (Map.Map Name Name) Q Name)
withFreshNames k = do
        m <- get
        if Map.member k m
            then return $ fromJust $ Map.lookup k m
            else do
                nn <- lift $ newName "x"
                modify $ Map.insert k nn
                return nn
