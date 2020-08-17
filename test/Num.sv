module mul (output longint o_a12L, 
            input longint i_a12M, 
            input longint i_a12N);
  always_comb o_a12L = i_a12M * i_a12N;
endmodule
module minus (output longint o_a12O, 
              input longint i_a12P, 
              input longint i_a12Q);
  always_comb o_a12O = i_a12P - i_a12Q;
endmodule
module plus (output longint o_a12R, 
             input longint i_a12S, 
             input longint i_a12T);
  always_comb o_a12R = i_a12S + i_a12T;
endmodule
module mulW (output longint unsigned o_a12U, 
             input longint unsigned i_a12V, 
             input longint unsigned i_a12W);
  always_comb o_a12U = i_a12V * i_a12W;
endmodule
module minusW (output longint unsigned o_a12X, 
               input longint unsigned i_a12Y, 
               input longint unsigned i_a12Z);
  always_comb o_a12X = i_a12Y - i_a12Z;
endmodule
module plusW (output longint unsigned o_a130, 
              input longint unsigned i_a131, 
              input longint unsigned i_a132);
  always_comb o_a130 = i_a131 + i_a132;
endmodule
module mul8 (output byte o_a133, 
             input byte i_a134, 
             input byte i_a135);
  always_comb o_a133 = i_a134 * i_a135;
endmodule
module minus8 (output byte o_a136, 
               input byte i_a137, 
               input byte i_a138);
  always_comb o_a136 = i_a137 - i_a138;
endmodule
module plus8 (output byte o_a12I, 
              input byte i_a12J, 
              input byte i_a12K);
  always_comb o_a12I = i_a12J + i_a12K;
endmodule