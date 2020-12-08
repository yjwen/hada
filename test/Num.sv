module plus (output longint o_a1r2, 
             input longint eta_B2, 
             input longint eta_B1);
  always_comb o_a1r2 = eta_B2 + eta_B1;
endmodule