package hada;
   function byte abs8(byte a);
      return a >= 0 ? a : -a;
   endfunction // abs8
   function shortint abs16(shortint a);
      return a >= 0 ? a : -a;
   endfunction // abs16
   function int abs32(int a);
      return a >= 0 ? a : -a;
   endfunction // abs32
   function longint abs64(longint a);
      return a >= 0 ? a : -a;
   endfunction // abs64

   function byte signum8(byte a);
      return a > 0 ? byte'(1) : a == 0 ? byte'(0) :byte'(-1);
   endfunction
   function shortint signum16(shortint a);
      return a > 0 ? shortint'(1) : a == 0 ? shortint'(0) : shortint'(-1);
   endfunction
   function int signum32(int a);
      return a > 0 ? int'(1) : a == 0 ? int'(0) : int'(-1);
   endfunction
   function longint signum64(longint a);
      return a > 0 ? longint'(1) : a == 0 ? longint'(0) : longint'(-1);
   endfunction

   function byte unsigned signumU8(byte unsigned a);
      return a == 0 ? byte'(0) : byte'(1);
   endfunction // signumU8
   function shortint unsigned signumU16(shortint unsigned a);
      return a == 0 ? shortint'(0) : shortint'(1);
   endfunction // signumU16
   function int unsigned signumU32(int unsigned a);
      return a == 0 ? int'(0) : int'(1);
   endfunction // signumU32
   function longint unsigned signumU64(longint unsigned a);
      return a == 0 ? longint'(0) : longint'(1);
   endfunction // signumU64

   // Constructors for unboxed integer types
   function byte consI8(longint a);
      return byte'(a);
   endfunction // consI8
   function shortint consI16(longint a);
      return shortint'(a);
   endfunction // consI16
   function int consI32(longint a);
      return int'(a);
   endfunction // consI32
   function longint consI64(longint a);
      return a;
   endfunction // consI64
   function longint consI(longint a);
      return a;
   endfunction // consI
   
   typedef byte unsigned ubyte;
   typedef shortint unsigned ushortint;
   typedef int unsigned      uint;
   typedef longint unsigned  ulongint;

   
   function ubyte consW8(ulongint a);
      return ubyte'(a);
   endfunction // consW8
   function ushortint consW16(ulongint a);
      return ushortint'(a);
   endfunction // consW16
   function uint consW32(ulongint a);
      return uint'(a);
   endfunction // consW32
   function ulongint consW64(ulongint a);
      return a;
   endfunction // consW64
   function ulongint consW(ulongint a);
      return a;
   endfunction // consW

   // For pattern matching of unboxed values
   function longint matchI8(byte v);
      return longint'(v);
   endfunction // matchI8
   function longint matchI16(shortint v);
      return longint'(v);
   endfunction // matchI16
   function longint matchI32(int v);
      return longint'(v);
   endfunction // matchI32
   function longint matchI64(longint v);
      return v;
   endfunction // matchI64
   function longint matchI(longint v);
      return v;
   endfunction
   function ulongint matchW8(ubyte v);
      return ulongint'(v);
   endfunction // matchW8
   function ulongint matchW16(ushortint v);
      return ulongint'(v);
   endfunction // matchW16
   function ulongint matchW32(uint v);
      return ulongint'(v);
   endfunction // matchW32
   function ulongint matchW64(ulongint v);
      return v;
   endfunction // matchW64
   function ulongint matchW(ulongint v);
      return v;
   endfunction // matchW

   function bit tagToEnumBool(bit v);
      return v;
   endfunction // tagToEnumBool
endpackage // hada
   
