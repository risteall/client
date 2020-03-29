{-# LANGUAGE LambdaCase, TemplateHaskell, TupleSections #-}

module WidgetGetter where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Graphics.UI.Gtk
import Data.Char
import Control.Monad
import Data.Maybe

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
