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

   function bit tagToEnumBool(bit v);
      return v;
   endfunction // tagToEnumBool
endpackage // hada
   
