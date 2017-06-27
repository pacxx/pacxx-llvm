

let TargetPrefix = "pacxx" in {
// Accessing special registers.
multiclass PACXXIDIntrinsic_v4i32<string regname> {
// FIXME: Do we need the 128-bit integer type version?
//    def _r64   : Intrinsic<[llvm_i128_ty],   [], [IntrNoMem]>;

// FIXME: Enable this once v4i32 support is enabled in back-end.
//    def _v4i16 : Intrinsic<[llvm_v4i32_ty], [], [IntrNoMem]>;

  def _x     : Intrinsic<[llvm_i32_ty], [], [IntrNoMem]>,
               GCCBuiltin<"__pacxx_read_" # regname # "_x">;
  def _y     : Intrinsic<[llvm_i32_ty], [], [IntrNoMem]>,
               GCCBuiltin<"__pacxx_read_" # regname # "_y">;
  def _z     : Intrinsic<[llvm_i32_ty], [], [IntrNoMem]>,
               GCCBuiltin<"__pacxx_read_" # regname # "_z">;
  def _w     : Intrinsic<[llvm_i32_ty], [], [IntrNoMem]>,
               GCCBuiltin<"__pacxx_read_" # regname # "_w">;
}

class PACXXIDIntrinsic_r32<string name>
  : Intrinsic<[llvm_i32_ty], [], [IntrNoMem]>,
    GCCBuiltin<"__pacxx_read_" # name>;

class PACXXIDIntrinsic_r64<string name>
  : Intrinsic<[llvm_i64_ty], [], [IntrNoMem]>,
    GCCBuiltin<"__pacxx_read_" # name>;

defm int_pacxx_read_tid : PACXXIDIntrinsic_v4i32<"tid">;
defm int_pacxx_read_ntid : PACXXIDIntrinsic_v4i32<"ntid">;
defm int_pacxx_read_ctaid : PACXXIDIntrinsic_v4i32<"ctaid">;
defm int_pacxx_read_nctaid : PACXXIDIntrinsic_v4i32<"nctaid">;


def int_pacxx_barrier0 : GCCBuiltin<"__pacxx_syncthreads">,
  Intrinsic<[], [], [IntrConvergent]>;

def int_pacxx_offload : GCCBuiltin<"__pacxx_offload">,
  Intrinsic<[], [], [IntrConvergent]>;

def int_pacxx_synchronize : GCCBuiltin<"__pacxx_synchronize">,
  Intrinsic<[], [], [IntrConvergent]>;
}
