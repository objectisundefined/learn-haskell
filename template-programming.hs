{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

makeLengthTH :: [Dec]
makeLengthTH = [typeSig, FunD lengthTH [eq1, eq2]]
  where
    lengthTH = mkName "lengthTH"
    typeSig = SigD lengthTH
      (ForallT
        [PlainTV (mkName "a")]
        []
        (ArrowT `AppT` (AppT ListT (VarT $ mkName "a")) `AppT` (ConT ''Int))
      )
    eq1 = Clause
      [ConP '[] []]
      (NormalB (LitE $ IntegerL 0))
      []
    eq2 = Clause
      [InfixP (VarP $ mkName "x") '(:) (VarP $ mkName "xs")]
      (NormalB (
        InfixE
          (Just (LitE $ IntegerL 1))
          (VarE '(+))
          (Just $ (VarE lengthTH) `AppE` (VarE $ mkName "xs"))
      ))
      []

main :: IO ()
main = putStrLn $ pprint makeLengthTH

{-
in ghci:

*Main> main
lengthTH :: forall a . [a] -> GHC.Types.Int
lengthTH (GHC.Types.[]) = 0
lengthTH (x GHC.Types.: xs) = 1 GHC.Num.+ lengthTH xs

*Main> :set -XTemplateHaskell
*Main> data X; return makeLengthTH
*Main> lengthTH []
0
*Main> lengthTH [1, 2, 3]
3

-}
