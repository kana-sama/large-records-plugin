module Data.Record.Plugin.Runtime
  ( -- Vector
    Vector,
    vectorFromList,
    vectorToList,
    vectorUnsafeIndex,
    vectorUnsafeUpd,
    -- LargeRecord
    noInlineUnsafeCo,
    dictFor,
    Rep (Rep),
    Dict,
    Generic (Constraints, MetadataOf, from, to, dict, metadata),
    Metadata (Metadata, recordName, recordConstructor, recordSize, recordFieldMetadata),
    repFromVector,
    repToVector,
    rnfVectorAny,
    FieldMetadata (FieldMetadata),
    FieldStrictness (FieldLazy, FieldStrict),
    ThroughLRGenerics (WrapThroughLRGenerics, unwrapThroughLRGenerics),
    geq,
    gshowsPrec,
    gcompare,
    -- Misc
    Any,
    Int,
    unsafeCoerce,
    error,
    HasField (..),
    Type,
    Constraint,
    Proxy (Proxy),
    seq,
    Show (showsPrec),
    Eq ((==)),
    Ord (compare),
  )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Record.Generic
  ( FieldMetadata (FieldMetadata),
    FieldStrictness (FieldLazy, FieldStrict),
    Generic (Constraints, MetadataOf, dict, from, metadata, to),
    Metadata (Metadata, recordConstructor, recordFieldMetadata, recordName, recordSize),
  )
import Data.Record.Generic.Eq (gcompare, geq)
import Data.Record.Generic.GHC (ThroughLRGenerics (WrapThroughLRGenerics, unwrapThroughLRGenerics))
import Data.Record.Generic.Rep.Internal (Rep (Rep))
import Data.Record.Generic.Show (gshowsPrec)
import Data.Record.TH.Runtime (dictFor, noInlineUnsafeCo, repFromVector, repToVector, rnfVectorAny)
import Data.SOP.Dict (Dict)
import Data.Vector (Vector, copy, fromList, toList, unsafeIndex, unsafeUpd)
import GHC.Base (Any)
import GHC.Records.Compat (HasField (..))
import Unsafe.Coerce (unsafeCoerce)

vectorFromList :: [a] -> Vector a
vectorFromList = Data.Vector.fromList
{-# INLINE vectorFromList #-}

vectorToList :: Vector a -> [a]
vectorToList = Data.Vector.toList
{-# INLINE vectorToList #-}

vectorUnsafeIndex :: Vector a -> Int -> a
vectorUnsafeIndex = Data.Vector.unsafeIndex
{-# INLINE vectorUnsafeIndex #-}

vectorUnsafeUpd :: Vector a -> [(Int, a)] -> Vector a
vectorUnsafeUpd = Data.Vector.unsafeUpd
{-# INLINE vectorUnsafeUpd #-}
