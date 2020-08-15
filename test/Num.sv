module mul (output longint o_aL3, 
            input longint i_aL4, 
            input longint i_aL5);
  always_comb o_aL3 = i_aL4 * i_aL5;
endmodule
module minus (output longint o_aL6, 
              input longint i_aL7, 
              input longint i_aL8);
  always_comb o_aL6 = i_aL7 - i_aL8;
endmodule
module plus (output longint o_aL9, 
             input longint i_aLa, 
             input longint i_aLb);
  always_comb o_aL9 = i_aLa + i_aLb;
endmodule
module mulW (output longint unsigned o_aLc, 
             input longint unsigned i_aLd, 
             input longint unsigned i_aLe);
  always_comb o_aLc = i_aLd * i_aLe;
endmodule
module minusW (output longint unsigned o_aLf, 
               input longint unsigned i_aLg, 
               input longint unsigned i_aLh);
  always_comb o_aLf = i_aLg - i_aLh;
endmodule
module plusW (output longint unsigned o_aL0, 
              input longint unsigned i_aL1, 
              input longint unsigned i_aL2);
  always_comb o_aL0 = i_aL1 + i_aL2;
endmodule