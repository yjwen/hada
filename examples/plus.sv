module plus (output longint o_a1by, 
             input longint eta_B2, 
             input longint eta_B1);
  always_comb o_a1by = eta_B2 + eta_B1;
endmodule