module minus (output longint o_aJl, 
              input longint i_aJm, 
              input longint i_aJn);
  always_comb o_aJl = i_aJm - i_aJn;
endmodule
module plus (output longint o_aJi, 
             input longint i_aJj, 
             input longint i_aJk);
  always_comb o_aJi = i_aJj + i_aJk;
endmodule