module mul (output longint o_a1r5, 
            input longint i_a1r6, 
            input longint i_a1r7);
  always_comb o_a1r5 = i_a1r6 * i_a1r7;
endmodule
module minus (output longint o_a1r8, 
              input longint i_a1r9, 
              input longint i_a1ra);
  always_comb o_a1r8 = i_a1r9 - i_a1ra;
endmodule
module plus (output longint o_a1rb, 
             input longint i_a1rc, 
             input longint i_a1rd);
  always_comb o_a1rb = i_a1rc + i_a1rd;
endmodule
module mulW (output longint unsigned o_a1re, 
             input longint unsigned i_a1rf, 
             input longint unsigned i_a1rg);
  always_comb o_a1re = i_a1rf * i_a1rg;
endmodule
module minusW (output longint unsigned o_a1rh, 
               input longint unsigned i_a1ri, 
               input longint unsigned i_a1rj);
  always_comb o_a1rh = i_a1ri - i_a1rj;
endmodule
module plusW (output longint unsigned o_a1rk, 
              input longint unsigned i_a1rl, 
              input longint unsigned i_a1rm);
  always_comb o_a1rk = i_a1rl + i_a1rm;
endmodule
module mul8 (output byte o_a1rn, 
             input byte i_a1ro, 
             input byte i_a1rp);
  always_comb o_a1rn = i_a1ro * i_a1rp;
endmodule
module minus8 (output byte o_a1rq, 
               input byte i_a1rr, 
               input byte i_a1rs);
  always_comb o_a1rq = i_a1rr - i_a1rs;
endmodule
module plus8 (output byte o_a1rt, 
              input byte i_a1ru, 
              input byte i_a1rv);
  always_comb o_a1rt = i_a1ru + i_a1rv;
endmodule
module mulW8 (output byte unsigned o_a1rw, 
              input byte unsigned i_a1rx, 
              input byte unsigned i_a1ry);
  always_comb o_a1rw = i_a1rx * i_a1ry;
endmodule
module minusW8 (output byte unsigned o_a1rz, 
                input byte unsigned i_a1rA, 
                input byte unsigned i_a1rB);
  always_comb o_a1rz = i_a1rA - i_a1rB;
endmodule
module plusW8 (output byte unsigned o_a1rC, 
               input byte unsigned i_a1rD, 
               input byte unsigned i_a1rE);
  always_comb o_a1rC = i_a1rD + i_a1rE;
endmodule
module mul16 (output shortint o_a1rF, 
              input shortint i_a1rG, 
              input shortint i_a1rH);
  always_comb o_a1rF = i_a1rG * i_a1rH;
endmodule
module minus16 (output shortint o_a1rI, 
                input shortint i_a1rJ, 
                input shortint i_a1rK);
  always_comb o_a1rI = i_a1rJ - i_a1rK;
endmodule
module plus16 (output shortint o_a1rL, 
               input shortint i_a1rM, 
               input shortint i_a1rN);
  always_comb o_a1rL = i_a1rM + i_a1rN;
endmodule
module mulW16 (output shortint unsigned o_a1rO, 
               input shortint unsigned i_a1rP, 
               input shortint unsigned i_a1rQ);
  always_comb o_a1rO = i_a1rP * i_a1rQ;
endmodule
module minusW16 (output shortint unsigned o_a1rR, 
                 input shortint unsigned i_a1rS, 
                 input shortint unsigned i_a1rT);
  always_comb o_a1rR = i_a1rS - i_a1rT;
endmodule
module plusW16 (output shortint unsigned o_a1rU, 
                input shortint unsigned i_a1rV, 
                input shortint unsigned i_a1rW);
  always_comb o_a1rU = i_a1rV + i_a1rW;
endmodule
module mul32 (output int o_a1rX, 
              input int i_a1rY, 
              input int i_a1rZ);
  always_comb o_a1rX = i_a1rY * i_a1rZ;
endmodule
module minus32 (output int o_a1s0, 
                input int i_a1s1, 
                input int i_a1s2);
  always_comb o_a1s0 = i_a1s1 - i_a1s2;
endmodule
module plus32 (output int o_a1s3, 
               input int i_a1s4, 
               input int i_a1s5);
  always_comb o_a1s3 = i_a1s4 + i_a1s5;
endmodule
module mulW32 (output int unsigned o_a1s6, 
               input int unsigned i_a1s7, 
               input int unsigned i_a1s8);
  always_comb o_a1s6 = i_a1s7 * i_a1s8;
endmodule
module minusW32 (output int unsigned o_a1s9, 
                 input int unsigned i_a1sa, 
                 input int unsigned i_a1sb);
  always_comb o_a1s9 = i_a1sa - i_a1sb;
endmodule
module plusW32 (output int unsigned o_a1sc, 
                input int unsigned i_a1sd, 
                input int unsigned i_a1se);
  always_comb o_a1sc = i_a1sd + i_a1se;
endmodule
module mul64 (output longint o_a1sf, 
              input longint i_a1sg, 
              input longint i_a1sh);
  always_comb o_a1sf = i_a1sg * i_a1sh;
endmodule
module minus64 (output longint o_a1si, 
                input longint i_a1sj, 
                input longint i_a1sk);
  always_comb o_a1si = i_a1sj - i_a1sk;
endmodule
module plus64 (output longint o_a1sl, 
               input longint i_a1sm, 
               input longint i_a1sn);
  always_comb o_a1sl = i_a1sm + i_a1sn;
endmodule
module mulW64 (output longint unsigned o_a1so, 
               input longint unsigned i_a1sp, 
               input longint unsigned i_a1sq);
  always_comb o_a1so = i_a1sp * i_a1sq;
endmodule
module minusW64 (output longint unsigned o_a1sr, 
                 input longint unsigned i_a1ss, 
                 input longint unsigned i_a1st);
  always_comb o_a1sr = i_a1ss - i_a1st;
endmodule
module plusW64 (output longint unsigned o_a1r2, 
                input longint unsigned i_a1r3, 
                input longint unsigned i_a1r4);
  always_comb o_a1r2 = i_a1r3 + i_a1r4;
endmodule