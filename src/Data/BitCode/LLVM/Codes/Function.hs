module Data.BitCode.LLVM.Codes.Function where

-- | The function body block (FUNCTION_BLOCK_ID) describes function bodies.  It
-- can contain a constant block (CONSTANTS_BLOCK_ID).
data Function
  = FUN_CODE_UNUSED0
  -- | DECLAREBLOCKS: [n]
  | FUNC_CODE_DECLAREBLOCKS -- 1
  -- | BINOP:      [opcode, ty, opval, opval]
  | FUNC_CODE_INST_BINOP -- 2
  -- | CAST:       [opcode, ty, opty, opval]
  | FUNC_CODE_INST_CAST -- 3
  -- | GEP:        [n x operands]
  | FUNC_CODE_INST_GEP_OLD -- 4
  -- | SELECT:     [ty, opval, opval, opval]
  | FUNC_CODE_INST_SELECT -- 5
  -- | EXTRACTELT: [opty, opval, opval]
  | FUNC_CODE_INST_EXTRACTELT -- 6
  -- | INSERTELT:  [ty, opval, opval, opval]
  | FUNC_CODE_INST_INSERTELT -- 7
  -- | SHUFFLEVEC: [ty, opval, opval, opval]
  | FUNC_CODE_INST_SHUFFLEVEC -- 8
  -- | CMP:        [opty, opval, opval, pred]
  | FUNC_CODE_INST_CMP -- 9
  -- | RET:        [opty,opval<both optional>]
  | FUNC_CODE_INST_RET -- 10
  -- | BR:         [bb#, bb#, cond] or [bb#]
  | FUNC_CODE_INST_BR -- 11
  -- | SWITCH:     [opty, op0, op1, ...]
  | FUNC_CODE_INST_SWITCH -- 12
  -- | INVOKE:     [attr, fnty, op0,op1, ...]
  | FUNC_CODE_INST_INVOKE -- 13
  | FUNC_CODE_UNUSED14
  -- | UNREACHABLE
  | FUNC_CODE_INST_UNREACHABLE -- 15
  -- | PHI:        [ty, val0,bb0, ...]
  | FUNC_CODE_INST_PHI -- 16
  | FUNC_CODE_UNUSED17
  | FUNC_CODE_UNUSED18
  -- | ALLOCA:     [instty, opty, op, align]
  | FUNC_CODE_INST_ALLOCA -- 19
  -- | LOAD:       [opty, op, align, vol]
  | FUNC_CODE_INST_LOAD -- 20
  | FUNC_CODE_UNUSED21
  | FUNC_CODE_UNUSED22
  -- | VAARG:      [valistty, valist, instty]
  | FUNC_CODE_INST_VAARG -- 23
  -- | This store code encodes the pointer type, rather than the value type
  -- this is so information only available in the pointer type (e.g. address
  -- spaces) is retained.
  -- | STORE:      [ptrty,ptr,val, align, vol]
  | FUNC_CODE_INST_STORE_OLD -- 24
  | FUNC_CODE_UNUSED25
  -- | EXTRACTVAL: [n x operands]
  | FUNC_CODE_INST_EXTRACTVAL -- 26
  -- | INSERTVAL:  [n x operands]
  | FUNC_CODE_INST_INSERTVAL -- 27
  -- | fcmp/icmp returning Int1TY or vector of Int1Ty. Same as CMP, exists to
  -- support legacy vicmp/vfcmp instructions.
  -- | CMP2:       [opty, opval, opval, pred]
  | FUNC_CODE_INST_CMP2 -- 28
  -- | new select on i1 or [N x i1]
  -- | VSELECT:    [ty,opval,opval,predty,pred]
  | FUNC_CODE_INST_VSELECT -- 29
  -- | INBOUNDS_GEP: [n x operands]
  | FUNC_CODE_INST_INBOUNDS_GEP_OLD -- 30
  -- | INDIRECTBR: [opty, op0, op1, ...]
  | FUNC_CODE_INST_INDIRECTBR -- 31
  | FUNC_CODE_UNUSED32
  -- | DEBUG_LOC_AGAIN
  | FUNC_CODE_DEBUG_LOC_AGAIN -- 33
  -- | CALL:    [attr, cc, fnty, fnid, args...]
  | FUNC_CODE_INST_CALL -- 34
  -- | DEBUG_LOC:  [Line,Col,ScopeVal, IAVal]
  | FUNC_CODE_DEBUG_LOC -- 35
  -- | FENCE: [ordering, synchscope]
  | FUNC_CODE_INST_FENCE -- 36
  -- | CMPXCHG: [ptrty,ptr,cmp,new, align, vol, ordering, synchscope]
  | FUNC_CODE_INST_CMPXCHG_OLD -- 37
  -- | ATOMICRMW: [ptrty,ptr,val, operation, align, vol,ordering, synchscope]
  | FUNC_CODE_INST_ATOMICRMW -- 38
  -- | RESUME:     [opval]
  | FUNC_CODE_INST_RESUME -- 39
  -- | LANDINGPAD: [ty,val,val,num,id0,val0...]
  | FUNC_CODE_INST_LANDINGPAD_OLD -- 40
  -- | LOAD: [opty, op, align, vol,ordering, synchscope]
  | FUNC_CODE_INST_LOADATOMIC -- 41
  -- | STORE: [ptrty,ptr,val, align, vol, ordering, synchscope]
  | FUNC_CODE_INST_STOREATOMIC_OLD -- 42
  -- | GEP:  [inbounds, n x operands]
  | FUNC_CODE_INST_GEP -- 43
  -- | STORE: [ptrty,ptr,valty,val, align, vol]
  | FUNC_CODE_INST_STORE -- 44
  -- | STORE: [ptrty,ptr,val, align, vol
  | FUNC_CODE_INST_STOREATOMIC -- 45
  -- | CMPXCHG: [ptrty,ptr,valty,cmp,new, align,vol,ordering,synchscope]
  | FUNC_CODE_INST_CMPXCHG -- 46
  -- | LANDINGPAD: [ty,val,num,id0,val0...]
  | FUNC_CODE_INST_LANDINGPAD -- 47
  -- | CLEANUPRET: [val] or [val,bb#]
  | FUNC_CODE_INST_CLEANUPRET -- 48
  -- | CATCHRET: [val,bb#]
  | FUNC_CODE_INST_CATCHRET -- 49
  -- | CATCHPAD: [bb#,bb#,num,args...]
  | FUNC_CODE_INST_CATCHPAD -- 50
  -- | CLEANUPPAD: [num,args...]
  | FUNC_CODE_INST_CLEANUPPAD -- 51
  -- | CATCHSWITCH: [num,args...] or [num,args...,bb]
  | FUNC_CODE_INST_CATCHSWITCH -- 52
  | FUNC_CODE_UNUSED53
  | FUNC_CODE_UNUSED54
  -- | OPERAND_BUNDLE: [tag#, value...]
  | FUNC_CODE_OPERAND_BUNDLE -- 55
  deriving (Show, Enum)
