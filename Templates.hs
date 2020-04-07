{-# LANGUAGE LambdaCase, TemplateHaskell, TupleSections, StandaloneDeriving, DeriveLift #-}

module Templates where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Graphics.UI.Gtk
import Data.Char
import Control.Monad
import Data.Maybe
import Data.AppSettings

castFun :: Type -> ExpQ
castFun (ConT n) = varE $ mkName ("castTo" ++ nameBase n)

uncamel :: String -> String
uncamel (c:c':cs) | isUpper c' = c : '-' : uncamel (toLower c' : cs)
uncamel (c:cs) = c : uncamel cs
uncamel s = s

expandField :: Name -> VarBangType -> Q (Name, Name, Stmt)
expandField b (v, _, ty) = do
  v' <- newName (nameBase v)
  stmt <- bindS (varP v')
                (appsE [[|builderGetObject|]
                       ,varE b
                       ,castFun ty
                       ,stringE (uncamel (nameBase v))
                       ])
  return (v, v', stmt)

mkWidgetGetter :: Name -> String -> Q [Dec]
mkWidgetGetter name fNameS = reify name >>= \case
  TyConI (DataD _ _ _ _ [RecC conName fields] _) -> do
    let fName = mkName fNameS
    sig <- sigD fName (arrowT `appT` [t|Builder|] `appT` ([t|IO|] `appT` conT name))
    b <- newName "b"
    (vs, vs', stmts) <- unzip3 <$> mapM (expandField b) fields
    f <- funD fName
              [clause [varP b]
                      (normalB $ doE (map return stmts
                                      ++ [noBindS $ appE [|return|] $ return $ RecConE conName (zip vs (map VarE vs'))]))
                      []]
    return [sig, f]
  _ -> error "foo"

----------------------------------------------------------------

declareSettings :: String -> [(String, TypeQ, ExpQ, ExpQ)] -> Q [Dec]
declareSettings x l = do
    (decs, es) <- unzip <$> mapM f l
    let n = mkName x
    xDec <- funD n [clause [] (normalB (listE es)) []]
    return (join decs ++ [xDec])
  where
    f :: (String, TypeQ, ExpQ, ExpQ) -> Q ([Dec], ExpQ)
    f (s, t, def, fun) = do
      let n = mkName s
      sig <- sigD n ([t|Setting|] `appT` t)
      dec <- funD n [clause [] (normalB ([|Setting|] `appE` stringE (uncamel s) `appE` def)) []]
      return ([sig, dec], fun `appE` varE n)

----------------------------------------------------------------

deriving instance Lift Modifier

declareKeys :: [(String, [Modifier], String, String, ExpQ)] -> Q [Dec]
declareKeys l = do
    dataDec <- dataD
      (return [])
      keysDN
      [PlainTV tv]
      Nothing
      [recC keysDN (map (\a -> (a, Bang NoSourceUnpackedness NoSourceStrictness,) <$> varT tv) names)]
      [derivClause Nothing [[t|Functor|], [t|Foldable|], [t|Traversable|]]]
    funDec <- funD keysN [clause [] (normalB (recConE keysDN $ zipWith f names l)) []]
    return [dataDec, funDec]
  where
    names = map (\(a,_,_,_,_) -> mkName a) l
    settingName a = uncamel (init a) ++ "-key"
    keysDN = mkName "Keys"
    tv = mkName "a"
    keysN = mkName "keys"
--    sig = sigD keysN (varT keysDN `appT` [t|(Setting ([Modifier], KeyVal), String, Maybe (Widgets -> AddHandler ()))|])
    f n (a,b,c,d,e) = do
      expr <- tupE [[|Setting|]
                    `appE` stringE (settingName a)
                    `appE` tupE [lift b, [|keyFromName . fromString|] `appE` stringE c]
                   ,stringE d
                   ,e
                   ]
      return (n, expr)
  
