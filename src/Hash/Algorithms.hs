module Hash.Algorithms
  ( HashAlgorithm(..)
  , availableHashAlgorithms
  ) where

data HashAlgorithm
  = SHA512
  | SHA384
  | SHA256
  | SHA224
  | SHA1
  | MD5
  deriving (Show, Read, Enum, Bounded)

availableHashAlgorithms :: [HashAlgorithm]
availableHashAlgorithms = [minBound .. maxBound]
