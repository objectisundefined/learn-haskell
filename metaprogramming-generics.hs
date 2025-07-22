{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Demonstrates metaprogramming and generics in Haskell
- Template Haskell for compile-time code generation
- Generic programming with GHC.Generics
- Data.Data for reflective programming
- Automatic derivation of type classes
- Lens generation and manipulation
-}

module MetaprogrammingGenerics where

import GHC.Generics
import Data.Data
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Lens

-- Example data types for demonstration
data Person = Person
  { _personName :: String
  , _personAge :: Int
  , _personEmail :: String
  } deriving (Show, Eq, Generic, Data, Typeable)

data Company = Company
  { _companyName :: String
  , _companyEmployees :: [Person]
  , _companyRevenue :: Double
  } deriving (Show, Eq, Generic, Data, Typeable)

-- Automatic lens generation using Template Haskell
makeLenses ''Person
makeLenses ''Company

-- Generic programming with GHC.Generics
class GShow a where
  gshow :: a -> String
  default gshow :: (Generic a, GShow' (Rep a)) => a -> String
  gshow x = gshow' (from x)

class GShow' f where
  gshow' :: f a -> String

instance GShow' U1 where
  gshow' U1 = ""

instance (GShow c) => GShow' (K1 i c) where
  gshow' (K1 x) = gshow x

instance (GShow' f) => GShow' (M1 i c f) where
  gshow' (M1 x) = gshow' x

instance (GShow' f, GShow' g) => GShow' (f :*: g) where
  gshow' (x :*: y) = gshow' x ++ " " ++ gshow' y

instance (GShow' f, GShow' g) => GShow' (f :+: g) where
  gshow' (L1 x) = gshow' x
  gshow' (R1 x) = gshow' x

-- Base instances
instance GShow String where
  gshow = show

instance GShow Int where
  gshow = show

instance GShow Double where
  gshow = show

instance GShow a => GShow [a] where
  gshow xs = "[" ++ unwords (map gshow xs) ++ "]"

-- Automatic instances
instance GShow Person
instance GShow Company

-- Template Haskell: Generate equality functions
generateEq :: Name -> Q [Dec]
generateEq typeName = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify typeName
  let fieldNames = [fieldName | (fieldName, _, _) <- fields]
  
  eqClause <- clause 
    [conP typeName (map varP fieldNames), conP typeName (map (varP . mkName . (++"'") . nameBase) fieldNames)]
    (normalB $ foldr1 (\a b -> [| $a && $b |]) 
      [infixE (Just $ varE fieldName) [| (==) |] (Just $ varE $ mkName $ nameBase fieldName ++ "'") 
       | fieldName <- fieldNames])
    []
  
  return [InstanceD [] (AppT (ConT ''Eq) (ConT typeName)) [FunD '(==) [eqClause]]]

-- Generic serialization
class GSerialize a where
  gserialize :: a -> String
  default gserialize :: (Generic a, GSerialize' (Rep a)) => a -> String
  gserialize = gserialize' . from

class GSerialize' f where
  gserialize' :: f a -> String

instance GSerialize' U1 where
  gserialize' U1 = ""

instance GSerialize c => GSerialize' (K1 i c) where
  gserialize' (K1 x) = gserialize x

instance (Constructor c, GSerialize' f) => GSerialize' (M1 C c f) where
  gserialize' m@(M1 x) = conName m ++ "(" ++ gserialize' x ++ ")"

instance (GSerialize' f) => GSerialize' (M1 D c f) where
  gserialize' (M1 x) = gserialize' x

instance (Selector s, GSerialize' f) => GSerialize' (M1 S s f) where
  gserialize' m@(M1 x) = 
    let sname = selName m
    in if null sname then gserialize' x else sname ++ "=" ++ gserialize' x

instance (GSerialize' f, GSerialize' g) => GSerialize' (f :*: g) where
  gserialize' (x :*: y) = gserialize' x ++ "," ++ gserialize' y

instance (GSerialize' f, GSerialize' g) => GSerialize' (f :+: g) where
  gserialize' (L1 x) = gserialize' x
  gserialize' (R1 x) = gserialize' x

-- Base instances for serialization
instance GSerialize String where
  gserialize s = "\"" ++ s ++ "\""

instance GSerialize Int where
  gserialize = show

instance GSerialize Double where
  gserialize = show

instance GSerialize a => GSerialize [a] where
  gserialize xs = "[" ++ intercalate "," (map gserialize xs) ++ "]"
    where intercalate sep [] = ""
          intercalate sep [x] = x
          intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Automatic instances
instance GSerialize Person
instance GSerialize Company

-- Data.Data examples for reflective programming
showConstructors :: Data a => a -> [String]
showConstructors x = map show (dataTypeConstrs (dataTypeOf x))

countFields :: Data a => a -> Int
countFields = length . gmapQ (const 1)

-- Get all string fields from a data structure
getStringFields :: Data a => a -> [String]
getStringFields = everything (++) ([] `mkQ` (\s -> [s :: String]))

-- Template Haskell: Generate lenses manually
makeLens :: Name -> Name -> Q [Dec]
makeLens typeName fieldName = do
  let lensName = mkName $ nameBase fieldName
  TyConI (DataD _ _ _ _ [RecC conName fields] _) <- reify typeName
  
  let fieldType = case lookup fieldName [(fn, ft) | (fn, _, ft) <- fields] of
                    Just ft -> ft
                    Nothing -> error "Field not found"
  
  signature <- sigD lensName [t| Lens' $(conT typeName) $(return fieldType) |]
  
  definition <- funD lensName [
    clause [] (normalB [| lens $(varE fieldName) 
                              (\s b -> s { $(fieldName) = b }) |]) []
    ]
  
  return [signature, definition]

-- Template Haskell: Create JSON instances
makeJSON :: Name -> Q [Dec]
makeJSON name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let fieldNames = [nameBase fn | (fn, _, _) <- fields]
  
  toJSONClause <- clause [varP (mkName "x")] 
    (normalB [| object $(listE [infixE (Just [| pack $(stringE field) |]) 
                                      [| (.=) |] 
                                      (Just [| $(varE $ mkName field) $(varE $ mkName "x") |])
                               | field <- fieldNames]) |])
    []
  
  let toJSONDec = FunD 'toJSON [toJSONClause]
  let instanceDec = InstanceD [] (AppT (ConT ''ToJSON) (ConT name)) [toJSONDec]
  
  return [instanceDec]

-- Generic zipper for any data structure
data GenericZipper a = GenericZipper
  { focus :: a
  , context :: [a -> a]
  }

-- Move focus to a child (if possible)
down :: Data a => GenericZipper a -> Maybe (GenericZipper a)
down (GenericZipper focus ctx) = 
  case gmapQ id focus of
    [] -> Nothing
    (child:_) -> case cast child of
      Just child' -> Just $ GenericZipper child' ((\f x -> head (gmapT (const f) x)) : ctx)
      Nothing -> Nothing

-- Move focus back up
up :: GenericZipper a -> Maybe (GenericZipper a)
up (GenericZipper _ []) = Nothing
up (GenericZipper focus (restore:ctx)) = Just $ GenericZipper (restore focus) ctx

-- Generic fold using Data.Data
gfoldl' :: Data a => (forall b. Data b => r -> b -> r) -> r -> a -> r
gfoldl' f z x = gfoldl (\acc y -> f acc y) z x

-- Count total number of constructors in a data structure
totalConstructors :: Data a => a -> Int
totalConstructors = everything (+) (1 `mkQ` (\(_ :: String) -> 0))

-- Demonstration functions
demonstrateGenerics :: IO ()
demonstrateGenerics = do
  let person = Person "Alice" 30 "alice@example.com"
      company = Company "TechCorp" [person] 1000000.0
  
  putStrLn "Generic Programming Examples:"
  putStrLn $ "Person (generic show): " ++ gshow person
  putStrLn $ "Company (generic show): " ++ gshow company
  
  putStrLn "\nGeneric Serialization:"
  putStrLn $ "Person serialized: " ++ gserialize person
  putStrLn $ "Company serialized: " ++ gserialize company

demonstrateData :: IO ()
demonstrateData = do
  let person = Person "Bob" 25 "bob@example.com"
  
  putStrLn "\nData.Data Reflection:"
  putStrLn $ "Constructors: " ++ show (showConstructors person)
  putStrLn $ "Field count: " ++ show (countFields person)
  putStrLn $ "String fields: " ++ show (getStringFields person)
  putStrLn $ "Total constructors: " ++ show (totalConstructors person)

demonstrateLenses :: IO ()
demonstrateLenses = do
  let person = Person "Charlie" 35 "charlie@example.com"
      company = Company "StartupInc" [person] 500000.0
  
  putStrLn "\nLens Examples:"
  putStrLn $ "Original person: " ++ show person
  putStrLn $ "Updated name: " ++ show (person & personName .~ "Charles")
  putStrLn $ "Updated age: " ++ show (person & personAge +~ 1)
  
  putStrLn $ "\nOriginal company: " ++ show company
  putStrLn $ "Updated company name: " ++ show (company & companyName .~ "BigCorp")
  putStrLn $ "First employee name: " ++ show (company ^. companyEmployees . ix 0 . personName)

-- Compile-time computation example
type family Factorial (n :: Nat) :: Nat where
  Factorial 0 = 1
  Factorial n = n * Factorial (n - 1)

-- Type-level list processing
type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

main :: IO ()
main = do
  putStrLn "Metaprogramming and Generics in Haskell"
  putStrLn "======================================"
  demonstrateGenerics
  demonstrateData
  demonstrateLenses
  putStrLn "\nHaskell's metaprogramming capabilities enable powerful compile-time abstractions!"