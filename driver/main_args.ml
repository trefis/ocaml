(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let mk_a f =
  "-a", Arg.Unit f, " Build a library"
;;

let mk_absname f =
  "-absname", Arg.Unit f, " Show absolute filenames in error messages"
;;

let mk_annot f =
  "-annot", Arg.Unit f, " Save information in <filename>.annot"
;;

let mk_binannot f =
  "-bin-annot", Arg.Unit f, " Save typedtree in <filename>.cmt"
;;

let mk_c f =
  "-c", Arg.Unit f, " Compile only (do not link)"
;;

let mk_cc f =
  "-cc", Arg.String f, "<command>  Use <command> as the C compiler and linker"
;;

let mk_cclib f =
  "-cclib", Arg.String f, "<opt>  Pass option <opt> to the C linker"
;;

let mk_ccopt f =
  "-ccopt", Arg.String f,
  "<opt>  Pass option <opt> to the C compiler and linker"
;;

let mk_compact f =
  "-compact", Arg.Unit f, " Optimize code size rather than speed"
;;

let mk_compat_32 f =
  "-compat-32", Arg.Unit f,
  " Check that generated bytecode can run on 32-bit platforms"
;;

let mk_config f =
  "-config", Arg.Unit f, " Print configuration values and exit"
;;

let mk_custom f =
  "-custom", Arg.Unit f, " Link in custom mode"
;;

let mk_dllib f =
  "-dllib", Arg.String f, "<lib>  Use the dynamically-loaded library <lib>"
;;

let mk_dllpath f =
  "-dllpath", Arg.String f,
  "<dir>  Add <dir> to the run-time search path for shared libraries"
;;

let mk_dtypes f =
  "-dtypes", Arg.Unit f, " (deprecated) same as -annot"
;;

let mk_for_pack_byt f =
  "-for-pack", Arg.String f,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlc -pack -o <ident>.cmo"
;;

let mk_for_pack_opt f =
  "-for-pack", Arg.String f,
  "<ident>  Generate code that can later be `packed' with\n\
  \     ocamlopt -pack -o <ident>.cmx"
;;

let mk_g_byt f =
  "-g", Arg.Unit f, " Save debugging information"
;;

let mk_g_opt f =
  "-g", Arg.Unit f, " Record debugging information for exception backtrace"
;;

let mk_i f =
  "-i", Arg.Unit f, " Print inferred interface"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_impl f =
  "-impl", Arg.String f, "<file>  Compile <file> as a .ml file"
;;

let mk_init f =
  "-init", Arg.String f, "<file>  Load <file> instead of default init file"
;;

let mk_inline f =
  "-inline", Arg.Int f, "<n>  Set aggressiveness of inlining to <n>"
;;

let mk_inlining_stats f =
  "-inlining-stats", Arg.Unit f, " Emit .i files containing inlining statistics"
;;

let mk_rounds f =
  "-rounds", Arg.Int f, "<n>  Set number of simplification rounds to <n>"
;;

let mk_unroll f =
  "-unroll", Arg.Int f, "<n>  Set maximal number of times a function can be unrolled"
;;

let mk_intf f =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let mk_intf_suffix_2 f =
  "-intf_suffix", Arg.String f, "<string>  (deprecated) same as -intf-suffix"
;;

let mk_keep_locs f =
  "-keep-locs", Arg.Unit f, " Keep locations in .cmi files"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_linkall f =
  "-linkall", Arg.Unit f, " Link all modules, even unused ones"
;;

let mk_make_runtime f =
  "-make-runtime", Arg.Unit f,
  " Build a runtime system with given C objects and libraries"
;;

let mk_make_runtime_2 f =
  "-make_runtime", Arg.Unit f, " (deprecated) same as -make-runtime"
;;

let mk_modern f =
  "-modern", Arg.Unit f, " (deprecated) same as -labels"
;;

let mk_no_alias_deps f =
  "-no-alias-deps", Arg.Unit f,
  " Do not record dependencies for module aliases"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_no_float_const_prop f =
  "-no-float-const-prop", Arg.Unit f,
  " Deactivate constant propagation for floating-point operations"
;;

let mk_noassert f =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let mk_noautolink_byt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cma files"
;;

let mk_noautolink_opt f =
  "-noautolink", Arg.Unit f,
  " Do not automatically link C libraries specified in .cmxa files"
;;

let mk_nodynlink f =
  "-nodynlink", Arg.Unit f,
  " Enable optimizations for code that will not be dynlinked"
;;

let mk_noinit f =
  "-noinit", Arg.Unit f,
  " Do not load any init file"

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_noprompt f =
  "-noprompt", Arg.Unit f, " Suppress all prompts"
;;

let mk_nopromptcont f =
  "-nopromptcont", Arg.Unit f,
  " Suppress prompts for continuation lines of multi-line inputs"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let mk_open f =
  "-open", Arg.String f, "<module>  Opens the module <module> before typing"

let mk_output_obj f =
  "-output-obj", Arg.Unit f, " Output a C object file instead of an executable"
;;

let mk_p f =
  "-p", Arg.Unit f,
  " Compile and link with profiling support for \"gprof\"\n\
  \     (not supported on all platforms)"
;;

let mk_pack_byt f =
  "-pack", Arg.Unit f, " Package the given .cmo files into one .cmo"
;;

let mk_pack_opt f =
  "-pack", Arg.Unit f, " Package the given .cmx files into one .cmx"
;;

let mk_pp f =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let mk_ppx f =
  "-ppx", Arg.String f,
  "<command>  Pipe abstract syntax trees through preprocessor <command>"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_remove_unused f =
  "-remove-unused", Arg.Unit f, " Remove unused functions"
;;

let mk_runtime_variant f =
  "-runtime-variant", Arg.String f,
  "<str>  Use the <str> variant of the run-time system"
;;

let mk_S f =
  "-S", Arg.Unit f, " Keep intermediate assembly file"
;;

let mk_safe_string f =
  "-safe-string", Arg.Unit f, " Make strings immutable"
;;

let mk_shared f =
  "-shared", Arg.Unit f, " Produce a dynlinkable plugin"
;;

let mk_short_paths f =
  "-short-paths", Arg.Unit f, " Shorten paths in types"
;;

let mk_stdin f =
  "-stdin", Arg.Unit f, " Read script from standard input"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_thread f =
  "-thread", Arg.Unit f,
  " Generate code that supports the system threads library"
;;

let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let mk_unsafe_string f =
  "-unsafe-string", Arg.Unit f, " Make strings mutable (default)"
;;

let mk_use_runtime f =
  "-use-runtime", Arg.String f,
  "<file>  Generate bytecode for the given runtime system"
;;

let mk_use_runtime_2 f =
  "-use_runtime", Arg.String f,
  "<file>  (deprecated) same as -use-runtime"
;;

let mk_v f =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let mk_verbose f =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vmthread f =
  "-vmthread", Arg.Unit f,
  " Generate code that supports the threads library with VM-level\n\
  \     scheduling"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, " Show description of warning numbers"
;;

let mk_where f =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let mk_nopervasives f =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let mk_use_prims f =
  "-use-prims", Arg.String f, "<file>  (undocumented)"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let mk_dtypedtree f =
  "-dtypedtree", Arg.Unit f, " (undocumented)"
;;

let mk_drawlambda f =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dsource f =
  "-dsource", Arg.Unit f, " (undocumented)"
;;

let mk_dlambda f =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dclambda f =
  "-dclambda", Arg.Unit f, " (undocumented)"
;;

let mk_dflambda f =
  "-dflambda", Arg.Unit f, " (undocumented)"
;;

let mk_dinstr f =
  "-dinstr", Arg.Unit f, " (undocumented)"
;;

let mk_dcmm f =
  "-dcmm", Arg.Unit f, " (undocumented)"
;;

let mk_dsel f =
  "-dsel", Arg.Unit f, " (undocumented)"
;;

let mk_dcombine f =
  "-dcombine", Arg.Unit f, " (undocumented)"
;;

let mk_dcse f =
  "-dcse", Arg.Unit f, " (undocumented)"
;;

let mk_dlive f =
  "-dlive", Arg.Unit f, " (undocumented)"
;;

let mk_dspill f =
  "-dspill", Arg.Unit f, " (undocumented)"
;;

let mk_dsplit f =
  "-dsplit", Arg.Unit f, " (undocumented)"
;;

let mk_dinterf f =
  "-dinterf", Arg.Unit f, " (undocumented)"
;;

let mk_dprefer f =
  "-dprefer", Arg.Unit f, " (undocumented)"
;;

let mk_dalloc f =
  "-dalloc", Arg.Unit f, " (undocumented)"
;;

let mk_dreload f =
  "-dreload", Arg.Unit f, " (undocumented)"
;;

let mk_dscheduling f =
  "-dscheduling", Arg.Unit f, " (undocumented)"
;;

let mk_dlinear f =
  "-dlinear", Arg.Unit f, " (undocumented)"
;;

let mk_dstartup f =
  "-dstartup", Arg.Unit f, " (undocumented)"
;;

let mk_dunused f =
  "-dunused", Arg.Unit f, " (undocumented)"

let mk_opaque f =
  "-opaque", Arg.Unit f,
  " Does not generate cross-module optimization information\n\
  \     (reduces necessary recompilation on module change)"
;;

let mk_strict_formats f =
  "-strict-formats", Arg.Unit f,
  " Reject invalid formats accepted by legacy implementations\n\
  \     (Warning: Invalid formats may behave differently from\n\
  \      previous OCaml versions, and will become always-rejected\n\
  \      in future OCaml versions. You should use this flag\n\
  \      to detect and fix invalid formats.)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

module type Common_options = sig
  val _absname : unit -> unit
  val _I : string -> unit
  val _labels : unit -> unit
  val _no_alias_deps : unit -> unit
  val _no_app_funct : unit -> unit
  val _noassert : unit -> unit
  val _nolabels : unit -> unit
  val _nostdlib : unit -> unit
  val _open : string -> unit
  val _ppx : string -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _strict_sequence : unit -> unit
  val _strict_formats : unit -> unit
  val _unsafe : unit -> unit
  val _unsafe_string : unit -> unit
  val _version : unit -> unit
  val _vnum : unit -> unit
  val _w : string -> unit
  val _warn_error : string -> unit
  val _warn_help : unit -> unit

  val _dsource : unit -> unit
  val _dparsetree : unit -> unit
  val _dtypedtree : unit -> unit
  val _drawlambda : unit -> unit
  val _dlambda : unit -> unit
  val _dflambda : unit -> unit

  val anonymous : string -> unit
end;;

module type Compiler_options = sig
  val _a : unit -> unit
  val _annot : unit -> unit
  val _binannot : unit -> unit
  val _c : unit -> unit
  val _cc : string -> unit
  val _cclib : string -> unit
  val _ccopt : string -> unit
  val _config : unit -> unit
  val _for_pack : string -> unit
  val _g : unit -> unit
  val _i : unit -> unit
  val _impl : string -> unit
  val _intf : string -> unit
  val _intf_suffix : string -> unit
  val _keep_locs : unit -> unit
  val _linkall : unit -> unit
  val _noautolink : unit -> unit
  val _o : string -> unit
  val _output_obj : unit -> unit
  val _pack : unit -> unit
  val _pp : string -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _runtime_variant : string -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _thread : unit -> unit
  val _v : unit -> unit
  val _verbose : unit -> unit
  val _where : unit -> unit
  val _nopervasives : unit -> unit
end
;;

module type Bytecomp_options = sig
  include Common_options
  include Compiler_options
  val _compat_32 : unit -> unit
  val _custom : unit -> unit
  val _dllib : string -> unit
  val _dllpath : string -> unit
  val _make_runtime : unit -> unit
  val _vmthread : unit -> unit
  val _use_runtime : string -> unit

  val _dinstr : unit -> unit

  val _use_prims : string -> unit
end;;

module type Bytetop_options = sig
  include Common_options
  val _init : string -> unit
  val _noinit : unit -> unit
  val _noprompt : unit -> unit
  val _nopromptcont : unit -> unit
  val _stdin : unit -> unit

  val _dinstr : unit -> unit
end;;

module type Optcommon_options = sig
  val _compact : unit -> unit
  val _inline : int -> unit
  val _inlining_stats : unit -> unit
  val _rounds : int -> unit
  val _unroll : int -> unit
  val _remove_unused : unit -> unit

  val _dclambda : unit -> unit
  val _dcmm : unit -> unit
  val _dsel : unit -> unit
  val _dcombine : unit -> unit
  val _dcse : unit -> unit
  val _dlive : unit -> unit
  val _dspill : unit -> unit
  val _dsplit : unit -> unit
  val _dinterf : unit -> unit
  val _dprefer : unit -> unit
  val _dalloc : unit -> unit
  val _dreload : unit -> unit
  val _dscheduling :  unit -> unit
  val _dlinear :  unit -> unit
  val _dstartup :  unit -> unit
  val _dunused : unit -> unit
end;;

module type Optcomp_options = sig
  include Common_options
  include Compiler_options
  include Optcommon_options
  val _no_float_const_prop : unit -> unit
  val _nodynlink : unit -> unit
  val _p : unit -> unit
  val _pp : string -> unit
  val _S : unit -> unit
  val _shared : unit -> unit
  val _opaque :  unit -> unit
end;;

module type Opttop_options = sig
  include Common_options
  include Optcommon_options
  val _init : string -> unit
  val _noinit : unit -> unit
  val _noprompt : unit -> unit
  val _nopromptcont : unit -> unit
  val _S : unit -> unit
  val _stdin : unit -> unit
end;;

module type Ocamldoc_options = sig
  include Common_options
  val _impl : string -> unit
  val _intf : string -> unit
  val _intf_suffix : string -> unit
  val _pp : string -> unit
  val _principal : unit -> unit
  val _rectypes : unit -> unit
  val _safe_string : unit -> unit
  val _short_paths : unit -> unit
  val _thread : unit -> unit
  val _v : unit -> unit
  val _verbose : unit -> unit
  val _vmthread : unit -> unit
end;;

module type Arg_list = sig
    val list : (string * Arg.spec * string) list
end;;

module Make_bytecomp_options (F : Bytecomp_options) =
struct
  let list = [
    mk_a F._a;
    mk_absname F._absname;
    mk_annot F._annot;
    mk_binannot F._binannot;
    mk_c F._c;
    mk_cc F._cc;
    mk_cclib F._cclib;
    mk_ccopt F._ccopt;
    mk_compat_32 F._compat_32;
    mk_config F._config;
    mk_custom F._custom;
    mk_dllib F._dllib;
    mk_dllpath F._dllpath;
    mk_dtypes F._annot;
    mk_for_pack_byt F._for_pack;
    mk_g_byt F._g;
    mk_i F._i;
    mk_I F._I;
    mk_impl F._impl;
    mk_intf F._intf;
    mk_intf_suffix F._intf_suffix;
    mk_intf_suffix_2 F._intf_suffix;
    mk_keep_locs F._keep_locs;
    mk_labels F._labels;
    mk_linkall F._linkall;
    mk_make_runtime F._make_runtime;
    mk_make_runtime_2 F._make_runtime;
    mk_modern F._labels;
    mk_no_alias_deps F._no_alias_deps;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_noautolink_byt F._noautolink;
    mk_nolabels F._nolabels;
    mk_nostdlib F._nostdlib;
    mk_o F._o;
    mk_open F._open;
    mk_output_obj F._output_obj;
    mk_pack_byt F._pack;
    mk_pp F._pp;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_runtime_variant F._runtime_variant;
    mk_safe_string F._safe_string;
    mk_short_paths F._short_paths;
    mk_strict_sequence F._strict_sequence;
    mk_strict_formats F._strict_formats;
    mk_thread F._thread;
    mk_unsafe F._unsafe;
    mk_unsafe_string F._unsafe_string;
    mk_use_runtime F._use_runtime;
    mk_use_runtime_2 F._use_runtime;
    mk_v F._v;
    mk_verbose F._verbose;
    mk_version F._version;
    mk_vmthread F._vmthread;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk_where F._where;
    mk__ F.anonymous;

    mk_nopervasives F._nopervasives;
    mk_use_prims F._use_prims;
    mk_dsource F._dsource;
    mk_dparsetree F._dparsetree;
    mk_dtypedtree F._dtypedtree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_dflambda F._dflambda;
    mk_dinstr F._dinstr;
  ]
end;;

module Make_bytetop_options (F : Bytetop_options) =
struct
  let list = [
    mk_absname F._absname;
    mk_I F._I;
    mk_init F._init;
    mk_labels F._labels;
    mk_no_alias_deps F._no_alias_deps;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_noinit F._noinit;
    mk_nolabels F._nolabels;
    mk_noprompt F._noprompt;
    mk_nopromptcont F._nopromptcont;
    mk_nostdlib F._nostdlib;
    mk_open F._open;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_safe_string F._safe_string;
    mk_short_paths F._short_paths;
    mk_stdin F._stdin;
    mk_strict_sequence F._strict_sequence;
    mk_strict_formats F._strict_formats;
    mk_unsafe F._unsafe;
    mk_unsafe_string F._unsafe_string;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk__ F.anonymous;

    mk_dsource F._dsource;
    mk_dparsetree F._dparsetree;
    mk_dtypedtree F._dtypedtree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_dflambda F._dflambda;
    mk_dinstr F._dinstr;
  ]
end;;

module Make_optcomp_options (F : Optcomp_options) =
struct
  let list = [
    mk_a F._a;
    mk_absname F._absname;
    mk_annot F._annot;
    mk_binannot F._binannot;
    mk_c F._c;
    mk_cc F._cc;
    mk_cclib F._cclib;
    mk_ccopt F._ccopt;
    mk_compact F._compact;
    mk_config F._config;
    mk_dtypes F._annot;
    mk_for_pack_opt F._for_pack;
    mk_g_opt F._g;
    mk_i F._i;
    mk_I F._I;
    mk_impl F._impl;
    mk_inline F._inline;
    mk_inlining_stats F._inlining_stats;
    mk_rounds F._rounds;
    mk_unroll F._unroll;
    mk_intf F._intf;
    mk_intf_suffix F._intf_suffix;
    mk_keep_locs F._keep_locs;
    mk_labels F._labels;
    mk_linkall F._linkall;
    mk_no_alias_deps F._no_alias_deps;
    mk_no_app_funct F._no_app_funct;
    mk_no_float_const_prop F._no_float_const_prop;
    mk_noassert F._noassert;
    mk_noautolink_opt F._noautolink;
    mk_nodynlink F._nodynlink;
    mk_nolabels F._nolabels;
    mk_nostdlib F._nostdlib;
    mk_o F._o;
    mk_open F._open;
    mk_output_obj F._output_obj;
    mk_p F._p;
    mk_pack_opt F._pack;
    mk_pp F._pp;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_remove_unused F._remove_unused;
    mk_runtime_variant F._runtime_variant;
    mk_S F._S;
    mk_safe_string F._safe_string;
    mk_shared F._shared;
    mk_short_paths F._short_paths;
    mk_strict_sequence F._strict_sequence;
    mk_strict_formats F._strict_formats;
    mk_thread F._thread;
    mk_unsafe F._unsafe;
    mk_unsafe_string F._unsafe_string;
    mk_v F._v;
    mk_verbose F._verbose;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk_where F._where;
    mk__ F.anonymous;

    mk_nopervasives F._nopervasives;
    mk_dsource F._dsource;
    mk_dparsetree F._dparsetree;
    mk_dtypedtree F._dtypedtree;
    mk_drawlambda F._drawlambda;
    mk_dlambda F._dlambda;
    mk_dclambda F._dclambda;
    mk_dflambda F._dflambda;
    mk_dcmm F._dcmm;
    mk_dsel F._dsel;
    mk_dcombine F._dcombine;
    mk_dcse F._dcse;
    mk_dlive F._dlive;
    mk_dspill F._dspill;
    mk_dsplit F._dsplit;
    mk_dinterf F._dinterf;
    mk_dprefer F._dprefer;
    mk_dalloc F._dalloc;
    mk_dreload F._dreload;
    mk_dscheduling F._dscheduling;
    mk_dlinear F._dlinear;
    mk_dstartup F._dstartup;
    mk_dunused F._dunused;
    mk_opaque F._opaque;
  ]
end;;

module Make_opttop_options (F : Opttop_options) = struct
  let list = [
    mk_absname F._absname;
    mk_compact F._compact;
    mk_I F._I;
    mk_init F._init;
    mk_inline F._inline;
    mk_inlining_stats F._inlining_stats;
    mk_rounds F._rounds;
    mk_unroll F._unroll;
    mk_labels F._labels;
    mk_no_alias_deps F._no_alias_deps;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_noinit F._noinit;
    mk_nolabels F._nolabels;
    mk_noprompt F._noprompt;
    mk_nopromptcont F._nopromptcont;
    mk_nostdlib F._nostdlib;
    mk_open F._open;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_S F._S;
    mk_safe_string F._safe_string;
    mk_short_paths F._short_paths;
    mk_stdin F._stdin;
    mk_strict_sequence F._strict_sequence;
    mk_strict_formats F._strict_formats;
    mk_unsafe F._unsafe;
    mk_unsafe_string F._unsafe_string;
    mk_version F._version;
    mk_vnum F._vnum;
    mk_w F._w;
    mk_warn_error F._warn_error;
    mk_warn_help F._warn_help;
    mk__ F.anonymous;

    mk_dsource F._dsource;
    mk_dparsetree F._dparsetree;
    mk_dtypedtree F._dtypedtree;
    mk_drawlambda F._drawlambda;
    mk_dclambda F._dclambda;
    mk_dflambda F._dflambda;
    mk_dcmm F._dcmm;
    mk_dsel F._dsel;
    mk_dcombine F._dcombine;
    mk_dcse F._dcse;
    mk_dlive F._dlive;
    mk_dspill F._dspill;
    mk_dsplit F._dsplit;
    mk_dinterf F._dinterf;
    mk_dprefer F._dprefer;
    mk_dalloc F._dalloc;
    mk_dreload F._dreload;
    mk_dscheduling F._dscheduling;
    mk_dlinear F._dlinear;
    mk_dstartup F._dstartup;
    mk_dunused F._dunused;
  ]
end;;

module Make_ocamldoc_options (F : Ocamldoc_options) =
struct
  let list = [
    mk_absname F._absname;
    mk_I F._I;
    mk_impl F._impl;
    mk_intf F._intf;
    mk_intf_suffix F._intf_suffix;
    mk_intf_suffix_2 F._intf_suffix;
    mk_labels F._labels;
    mk_modern F._labels;
    mk_no_alias_deps F._no_alias_deps;
    mk_no_app_funct F._no_app_funct;
    mk_noassert F._noassert;
    mk_nolabels F._nolabels;
    mk_nostdlib F._nostdlib;
    mk_open F._open;
    mk_pp F._pp;
    mk_ppx F._ppx;
    mk_principal F._principal;
    mk_rectypes F._rectypes;
    mk_safe_string F._safe_string;
    mk_short_paths F._short_paths;
    mk_strict_sequence F._strict_sequence;
    mk_strict_formats F._strict_formats;
    mk_thread F._thread;
    mk_unsafe_string F._unsafe_string;
    mk_v F._v;
    mk_verbose F._verbose;
    mk_version F._version;
    mk_vmthread F._vmthread;
    mk_vnum F._vnum;
    mk_w F._w;
    mk__ F.anonymous;
  ]
end;;
