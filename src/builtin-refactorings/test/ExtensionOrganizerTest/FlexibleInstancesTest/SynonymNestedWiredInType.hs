{-# LANGUAGE FlexibleInstances #-}

module SynonymNestedWiredInType where

import Definitions


instance C1 (TS3 a b Int) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (TS3 a Int c) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (TS3 Int b c) where  {-* FlexibleInstances *-}
  f1 _ = True

instance C1 (Phantom Int a) where  {-* FlexibleInstances *-}
  f1 _ = True

-- False positive
instance C1 (Phantom a Int) where  {-* FlexibleInstances *-}
  f1 _ = True
