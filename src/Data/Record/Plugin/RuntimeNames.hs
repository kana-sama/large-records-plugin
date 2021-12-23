module Data.Record.Plugin.RuntimeNames where

import Data.Record.Plugin.GHC (varE, varT)
import GHC (GhcPs, LHsExpr, LHsType, ModuleName, mkModuleName)
import OccName (mkDataOcc, mkTcOcc, mkVarOcc)
import RdrName (RdrName, mkRdrQual, mkRdrUnqual)

moduleRuntime, moduleGHCGeneric :: ModuleName
moduleRuntime = mkModuleName "Data.Record.Plugin.Runtime"
moduleGHCGeneric = mkModuleName "GHC.Generics"

_typeName :: String -> LHsType GhcPs
_typeName = varT . mkRdrQual moduleRuntime . mkTcOcc

_exprName :: String -> LHsExpr GhcPs
_exprName = varE . mkRdrQual moduleRuntime . mkVarOcc

_conName :: String -> RdrName
_conName = mkRdrQual moduleRuntime . mkDataOcc

-- Vector

_Vector :: LHsType GhcPs
_Vector = _typeName "Vector"

vectorFromList, vectorToList, vectorUnsafeIndex, vectorUnsafeUpd :: LHsExpr GhcPs
vectorFromList = _exprName "vectorFromList"
vectorToList = _exprName "vectorToList"
vectorUnsafeIndex = _exprName "vectorUnsafeIndex"
vectorUnsafeUpd = _exprName "vectorUnsafeUpd"

-- Large Records

_Rep, _Dict, _Generic, _ThroughLRGenerics :: LHsType GhcPs
_Rep = _typeName "Rep"
_Dict = _typeName "Dict"
_Generic = _typeName "Generic"
_ThroughLRGenerics = _typeName "ThroughLRGenerics"

_Constraints, _MetadataOf, from, to, dict, metadata :: RdrName
_Constraints = mkRdrUnqual (mkTcOcc "Constraints")
_MetadataOf = mkRdrUnqual (mkTcOcc "MetadataOf")
from = mkRdrUnqual (mkVarOcc "from")
to = mkRdrUnqual (mkVarOcc "to")
dict = mkRdrUnqual (mkVarOcc "dict")
metadata = mkRdrUnqual (mkVarOcc "metadata")

noInlineUnsafeCo, dictFor, repFromVector, repToVector, rnfVectorAny, unwrapThroughLRGenerics, geq, gshowsPrec, gcompare :: LHsExpr GhcPs
noInlineUnsafeCo = _exprName "noInlineUnsafeCo"
dictFor = _exprName "dictFor"
repFromVector = _exprName "repFromVector"
repToVector = _exprName "repToVector"
rnfVectorAny = _exprName "rnfVectorAny"
unwrapThroughLRGenerics = _exprName "unwrapThroughLRGenerics"
geq = _exprName "geq"
gshowsPrec = _exprName "gshowsPrec"
gcompare = _exprName "gcompare"

_C_Rep, _C_Metadata, _C_FieldMetadata, _C_FieldLazy, _C_FieldStrict, _C_WrapThroughLRGenerics :: RdrName
_C_Rep = _conName "Rep"
_C_Metadata = _conName "Metadata"
_C_FieldMetadata = _conName "FieldMetadata"
_C_FieldLazy = _conName "FieldLazy"
_C_FieldStrict = _conName "FieldStrict"
_C_WrapThroughLRGenerics = _conName "WrapThroughLRGenerics"

_F_recordName, _F_recordConstructor, _F_recordSize, _F_recordFieldMetadata :: RdrName
_F_recordName = mkRdrQual moduleRuntime (mkVarOcc "recordName")
_F_recordConstructor = mkRdrQual moduleRuntime (mkVarOcc "recordConstructor")
_F_recordSize = mkRdrQual moduleRuntime (mkVarOcc "recordSize")
_F_recordFieldMetadata = mkRdrQual moduleRuntime (mkVarOcc "recordFieldMetadata")

-- Misc

_Any, _Int, _HasField, _Type, _Constraint, _Proxy, _Show, _Eq, _Ord :: LHsType GhcPs
_Any = _typeName "Any"
_Int = _typeName "Int"
_HasField = _typeName "HasField"
_Type = _typeName "Type"
_Constraint = _typeName "Constraint"
_Proxy = _typeName "Proxy"
_Show = _typeName "Show"
_Eq = _typeName "Eq"
_Ord = _typeName "Ord"

unsafeCoerce, error, seq :: LHsExpr GhcPs
unsafeCoerce = _exprName "unsafeCoerce"
error = _exprName "error"
seq = _exprName "seq"

_C_Proxy :: RdrName
_C_Proxy = _conName "Proxy"

showsPrec, eq, compare :: RdrName
showsPrec = mkRdrUnqual (mkVarOcc "showsPrec")
eq = mkRdrUnqual (mkVarOcc "==")
compare = mkRdrUnqual (mkVarOcc "compare")

-- GHC

_GHC_Generic :: LHsType GhcPs
_GHC_Generic = varT (mkRdrQual moduleGHCGeneric (mkTcOcc "Generic"))

_GHC_Rep :: RdrName
_GHC_Rep = mkRdrUnqual (mkTcOcc "Rep")

_GHC_from, _GHC_to :: RdrName
_GHC_from = mkRdrUnqual (mkVarOcc "from")
_GHC_to = mkRdrUnqual (mkVarOcc "to")
