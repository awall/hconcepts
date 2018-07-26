module Main where

import Data.Monoid
import Data.Foldable

import Control.Lens
import Control.Lens.TH

import qualified Data.Set as S

type Version = Int
type Date = Int
type Scalar = Double
type Role = String
type UserName = String

type UserRoles = S.Set Role

data User = User {
  _name :: UserName,
  _roles :: UserRoles
}

data Visibility = Visibility {
  _mustHaveRoles :: UserRoles,
  _cantHaveRoles :: UserRoles
}

data Fact = Fact {
  _version :: Version,
  _scalar :: Scalar,
  _visibility :: Visibility
}

makeLenses ''Fact

instance Semigroup Visibility where
  a <> b = Visibility 
    (S.union (_mustHaveRoles a) (_mustHaveRoles b))
    (S.union (_cantHaveRoles a) (_cantHaveRoles b))
   
instance Monoid Visibility where
  mempty = Visibility S.empty S.empty
  mappend = (<>)

canSee' :: UserRoles -> Visibility -> Bool
canSee' r v = 
  (_mustHaveRoles v) `S.isSubsetOf` r
  && S.null (S.intersection r (_cantHaveRoles v))

canSee :: User -> Fact -> Bool
canSee u f = canSee' (_roles u) (_visibility f)

fempty :: Fact
fempty = Fact { _version = 0, _scalar = 0.0, _visibility = mempty }

foldFacts :: Foldable f => (Scalar -> Scalar -> Scalar) -> f Fact -> Fact
foldFacts f fs =   
  foldr combine fempty fs & version +~ 1
  where combine (Fact v1 s1 vi1) (Fact v2 s2 vi2) = Fact (max v1 v2) (f s1 s2) (mappend vi1 vi2)

sumFacts :: Foldable f => f Fact -> Fact
sumFacts = foldFacts (+)

main :: IO ()
main = do
  putStrLn $ "hello world"
