{-# LANGUAGE
      DataKinds
    , KindSignatures
    , GADTs
    , TemplateHaskell
    , ScopedTypeVariables
    , NoMonomorphismRestriction
 #-}
{-|
Module      : Language.Polycephaly.TH
Description : Polycephalous-instances code generation
Copyright   : (c) Julian K. Arni, 2014
License     : BSD3
Maintainer  : jkarni@gmail.com
Stability   : experimental

 Tools for automatic generation of 'fake unification' of constraints,
 a la http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap

 The objective is to be able to write something such as this:

    class Print a where
        print :: a -> IO ()

    instance Show a => Print a where
        print x = putStrLn (show x)
    instance           Print a where
        print x = putStrLn "No show method"

 Conventions:
    * The class whose instances we want to "duplicate" (`Print` in thw
    example above) is called the 'instance-head class'.
    * The class that is created behind the scenes to allow for this
    (`Print'` in the link above) is called the 'flag-dispatch class'
-}
module Language.Polycephaly.TH where


import Control.Monad.Trans.Class
import Data.Maybe (fromJust)
import Control.Monad.Trans.State
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi)
import qualified Data.Map.Strict as Map
import Data.Generics


import Language.Polycephaly.Typecast
import Language.Polycephaly.Utils
---------------------------------------------------------------------------
-- Basic Types
--
---------------------------------------------------------------------------
data HTrue
data HFalse

-- | Rewrite rules for the names of auxiliary classes. Note that even
-- though this means there might be name collisions, it allows library
-- users to add custom instances for the intermediate classes.
data RewriteRules = RewriteRules
    { rulePred :: String -> String      -- ^ How to name the shadow predicate
    , ruleHelpers :: String -> String   -- ^ How to name the flag-dispatch class
                                        -- TODO: Consider name collisions
    , trueType :: Name
    , falseType :: Name
    }

defaultRules :: RewriteRules
defaultRules = RewriteRules { rulePred = (++ "Pred")
                            , ruleHelpers = (++ "'")
                            , trueType = mkName "HTrue"
                            , falseType = mkName "HFalse"
                            }

---------------------------------------------------------------------------
-- Flag-dispatch class
--
--   Create the flag-dispatch class, as well as the instance-head class
--   instance.
---------------------------------------------------------------------------

-- | Make flag dispatch class.
-- >> let clsD = runQ [d| class Print a where { print :: a -> IO () } |]
-- >> mkFlagDC defaultRules clsD
-- class Print' flag a where { print' :: flag -> a -> IO () }
mkFlagDC :: RewriteRules   -- ^ The rewrite rules to use
         -> Dec            -- ^ The original class declaration
         -> Dec            -- ^ The new flag-dispatch class
mkFlagDC rr (ClassD ctx name tvb fnDps decs) =
        let rn x  = mkName $ ruleHelpers rr $ nameBase x
            nameN = rn name
            tvbN  = (PlainTV (mkName "flag")):tvb
            decRew (SigD n t) = SigD (rn n) (AppT (AppT ArrowT (VarT (mkName "flag"))) t)
            decRew a = a
        in ClassD ctx nameN tvbN fnDps (map decRew decs)
mkFlagDC _ _ = notCDec "mkFlagDC"


-- | Make instance-head class instance
-- >> mkIHCI defaultRulese (runQ [d| instance Show a => Print a where
--      print x = putStrLn (show x) |])
-- instance (ShowPred a flag, Print' flag a) => Print a where
--      print = print' (undefined::flag)
mkIHCI :: RewriteRules -> Dec -> Dec
mkIHCI rr (ClassD ctx name tv _fnDps decs) =
        let rn x  = mkName $ ruleHelpers rr $ nameBase x
            newPredCls (ClassP n typs) = ClassP (rn n) (typs ++ [VarT $ mkName "flag"])
            newPredCls _               = error "'~' not yet supported"
            ictx  = map newPredCls ctx
            tyVarBndrName (PlainTV n) = VarT n
            tyVarBndrName (KindedTV n _) = VarT n   -- TODO: Fix this kind
                                                    --- dropping.
                                                    -- And factor this out
                                                    -- into its own fn.
            ityp = foldr AppT (ConT name) (map tyVarBndrName tv)
        in InstanceD ictx ityp decs
mkIHCI _ _ = notCDec "mkICHI"
---------------------------------------------------------------------------
-- Pred Instances
--
--   Given a typeclass `T`, with instances `a`, `b`, `c`... we want to
--   automatically generate instances `a HTrue`, `b HTrue`, `c HTrue` for a
--   typeclass `TPred`. This section does that.
--   TODO: Allow instances to include more than just HTrue, for cases where
--   the dispatch is more than 2-way.
---------------------------------------------------------------------------

remakeCDec :: Name -> Q [Dec]
remakeCDec typ = do
        ClassI cdec _ <- reify typ
        return $ [changeCDec defaultRules cdec]

-- | Make a pred class declaration from a normal class declaration.
-- Note that right now kinded variables aren't dealt with properly.
-- Examples:
--
-- >>> comp = runQ [d| class SimplePred a flag | a -> flag where {}]
-- >>> class Simple a where { simpl :: a -> a }
-- >>> changeCDec defaultRules ''Simple == comp
-- True
changeCDec :: RewriteRules -> Dec -> Dec
changeCDec rr (ClassD ctx name tyVs fnDp _dec) =
    let nName = mkName $ (rulePred rr) (nameBase name)
        flag = mkName "flag"
        -- Insert newName for key, unless the key already exists. Return
        -- value associated with key.
        -- TODO: Is this really necessary?
        {-withFreshVarTs (VarT x)         = withFreshNames x-}
        {-withFreshTyVarBndrs (PlainTV x) = withFreshTyVarBndrs x-}
        getTyName (PlainTV n) = n
        getTyName (KindedTV n _) = n       -- This is definitely a TODO
        tyNs = map getTyName tyVs
        newFnDp = (FunDep tyNs [flag]):fnDp
        newTyVs = map PlainTV $ tyNs ++ [flag]  -- TODO: allow kinded.
    in ClassD ctx nName newTyVs newFnDp []
changeCDec _ _ = notCDec "changeCDec"

-- This should output something like:
-- TODO: Clean up variable renaming and move to test module.
-- >> [ClassD [] NPred [PlainTV a_4,PlainTV flag] [FunDep [a_4] [flag]] []]
sanityCheck :: Q [Dec]
sanityCheck = (fmap $ map (changeCDec defaultRules)) (runQ [d| class N a where|])

withFreshNames :: Name -> (StateT (Map.Map Name Name) Q Name)
withFreshNames k = do
        m <- get
        if Map.member k m
            then return $ fromJust $ Map.lookup k m
            else do
                nn <- lift $ newName "x"
                modify $ Map.insert k nn
                return nn

-- | Return all instances of a class.
-- Note that these include only instances that are visible where the
-- function is called!
getInstances :: Name -> Q [InstanceDec]
getInstances typ = do
  ClassI _ instances <- reify typ
  return instances


showInstances :: Name -> Q Exp
showInstances typ = do
  ins <- getInstances typ
  return . LitE . stringL $ show ins


-- Rewrite instances according to rewrite rules.
remakeInstances' :: Name -> RewriteRules -> Q [InstanceDec]
remakeInstances' t rr = do
        ins <- getInstances t
        return $ map (addFlag rr . changeN t rr) ins

remakeInstances :: Name -> Q [InstanceDec]
remakeInstances = flip remakeInstances' defaultRules

-- Change name `toChange` according to the rewrite rule `rr` wherever
-- possible in `input`.
changeN :: Data t => Name -> RewriteRules -> t -> t
changeN toChange rr input = let
    rename :: GenericT
    rename = mkT $ \n -> if n == toChange
        then mkName $ rulePred rr $ nameBase n
        else n
   in everywhere rename input

addFlag :: RewriteRules -> Dec -> Dec
addFlag rr (InstanceD ctx typ decs) = InstanceD ctx (AppT typ flag) decs
    where flag = ConT $ trueType rr
addFlag _ _ = notCDec "addFlag"


-- HFalse case
{-mkNegInst :: Name -> RewriteRules -> Dec-}
{-mkNegInst n rr =-}
        {-let ctx = (ClassP (mkName "TypeCast") [ VarT (mkName "flag")-}
                                              {-, ConT (falseType rr) ] )-}
        {-in InstanceD ctx-}
---------------------------------------------------------------------------
-- Tests, Trials, and Tribulations
---------------------------------------------------------------------------

test1 :: Q [Dec]
test1 = remakeCDec ''Fractional

test2 :: Quasi m => m [InstanceDec]
test2 = runQ $ remakeInstances ''Fractional
