#include <verilated.h>
#include <cstdint>
#include "Vplus.h"
extern "C" {
  int64_t plus (int64_t eta_B2, 
                int64_t eta_B1) {
    Vplus *top = new Vplus;
    top->eta_B2 = eta_B2;
    top->eta_B1 = eta_B1;
    top->eval();
    int64_t o_aIV = top->o_aIV;
    top->final();
    delete top
    return o_aIV;
  }
}
