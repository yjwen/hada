module plus (output longint o_aJz, 
             input longint eta_B2, 
             input longint eta_B1);
  always_comb o_aJz = eta_B2 + eta_B1;
endmodule