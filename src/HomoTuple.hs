{-# LANGUAGE TemplateHaskell #-}

module HomoTuple where

import Data.DeriveTH
import Data.Foldable
import Data.Traversable
import GHC.Generics
import Test.QuickCheck
import Control.DeepSeq
import Data.Binary
import Test.SmallCheck.Series as SC

data Tup0 a = Tup0
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup1 a = Tup1 !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup2 a = Tup2 !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup3 a = Tup3 !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup4 a = Tup4 !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup5 a = Tup5 !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup6 a = Tup6 !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup7 a = Tup7 !a !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup8 a = Tup8 !a !a !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data Tup9 a = Tup9 !a !a !a !a !a !a !a !a !a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)


data LzTup0 a = LzTup0
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup1 a = LzTup1 a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup2 a = LzTup2 a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup3 a = LzTup3 a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup4 a = LzTup4 a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup5 a = LzTup5 a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup6 a = LzTup6 a a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup7 a = LzTup7 a a a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup8 a = LzTup8 a a a a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

data LzTup9 a = LzTup9 a a a a a a a a a
  deriving (Show,Ord,Eq,Functor,Foldable,Traversable,Generic)

derive makeArbitrary ''LzTup0
derive makeArbitrary ''LzTup1
derive makeArbitrary ''LzTup2
derive makeArbitrary ''LzTup3
derive makeArbitrary ''LzTup4
derive makeArbitrary ''LzTup5
derive makeArbitrary ''LzTup6
derive makeArbitrary ''LzTup7
derive makeArbitrary ''LzTup8
derive makeArbitrary ''LzTup9
derive makeArbitrary ''Tup0
derive makeArbitrary ''Tup1
derive makeArbitrary ''Tup2
derive makeArbitrary ''Tup3
derive makeArbitrary ''Tup4
derive makeArbitrary ''Tup5
derive makeArbitrary ''Tup6
derive makeArbitrary ''Tup7
derive makeArbitrary ''Tup8
derive makeArbitrary ''Tup9

instance (NFData a) ⇒ NFData (LzTup0 a)
instance (NFData a) ⇒ NFData (LzTup1 a)
instance (NFData a) ⇒ NFData (LzTup2 a)
instance (NFData a) ⇒ NFData (LzTup3 a)
instance (NFData a) ⇒ NFData (LzTup4 a)
instance (NFData a) ⇒ NFData (LzTup5 a)
instance (NFData a) ⇒ NFData (LzTup6 a)
instance (NFData a) ⇒ NFData (LzTup7 a)
instance (NFData a) ⇒ NFData (LzTup8 a)
instance (NFData a) ⇒ NFData (LzTup9 a)
instance (NFData a) ⇒ NFData (Tup0 a)
instance (NFData a) ⇒ NFData (Tup1 a)
instance (NFData a) ⇒ NFData (Tup2 a)
instance (NFData a) ⇒ NFData (Tup3 a)
instance (NFData a) ⇒ NFData (Tup4 a)
instance (NFData a) ⇒ NFData (Tup5 a)
instance (NFData a) ⇒ NFData (Tup6 a)
instance (NFData a) ⇒ NFData (Tup7 a)
instance (NFData a) ⇒ NFData (Tup8 a)
instance (NFData a) ⇒ NFData (Tup9 a)

instance (Binary a) ⇒ Binary (LzTup0 a)
instance (Binary a) ⇒ Binary (LzTup1 a)
instance (Binary a) ⇒ Binary (LzTup2 a)
instance (Binary a) ⇒ Binary (LzTup3 a)
instance (Binary a) ⇒ Binary (LzTup4 a)
instance (Binary a) ⇒ Binary (LzTup5 a)
instance (Binary a) ⇒ Binary (LzTup6 a)
instance (Binary a) ⇒ Binary (LzTup7 a)
instance (Binary a) ⇒ Binary (LzTup8 a)
instance (Binary a) ⇒ Binary (LzTup9 a)
instance (Binary a) ⇒ Binary (Tup0 a)
instance (Binary a) ⇒ Binary (Tup1 a)
instance (Binary a) ⇒ Binary (Tup2 a)
instance (Binary a) ⇒ Binary (Tup3 a)
instance (Binary a) ⇒ Binary (Tup4 a)
instance (Binary a) ⇒ Binary (Tup5 a)
instance (Binary a) ⇒ Binary (Tup6 a)
instance (Binary a) ⇒ Binary (Tup7 a)
instance (Binary a) ⇒ Binary (Tup8 a)
instance (Binary a) ⇒ Binary (Tup9 a)

instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup0 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup1 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup2 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup3 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup4 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup5 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup6 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup7 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup8 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (LzTup9 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup0 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup1 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup2 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup3 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup4 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup5 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup6 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup7 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup8 a)
instance (Monad m,SC.Serial m a) ⇒ SC.Serial m (Tup9 a)
