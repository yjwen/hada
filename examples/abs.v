module abs (input [63 : 0] a,
            input [63 : 0] b,
            output [63 : 0] out);
assign out = (a < b) ? b - a : a - b;
endmodule
