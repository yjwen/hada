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
endpackage // hada
   
