module abs (a, b, out);
   input [0 : 63] a;
   input [0 : 63] b;
   output [0 : 63] out;
   assign out = (a < b) ? b - a : a - b;
endmodule
