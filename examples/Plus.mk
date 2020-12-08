VERILATOR ?= $(shell which verilator)

ifeq ($(VERILATOR),)
$(error Cannot find verilator binary)
endif

VERILATOR-CFLAGS ?= $(shell pkg-config --cflags verilator)
ifeq ($(VERILATOR-CFLAGS),)
$(error Cannot determine C flags for compiling verilated CPP source. Please specify the necessary C flags to VERILATOR-CFLAGS)
endif

plus.sv Wplus.cpp Wplus.hs: Plus.hs
	cd .. && cabal run hada -- examples/$^ -o examples/plus.sv -w examples/Wplus -t plus

obj_dir/Vplus__ALL.a obj_dir/Vplus.h: plus.sv
	$(VERILATOR) -cc --build -sv $^
Wplus.cpp.o: Wplus.cpp obj_dir/Vplus.h
	$(CXX) $(VERILATOR-CFLAGS) -I./obj_dir -c $< -o $@
obj_dir/verilated.o: obj_dir/Vplus__ALL.a
	$(MAKE) -C obj_dir -f Vplus.mk verilated.o

TestPlus : TestPlus.hs Wplus.hs Wplus.cpp.o obj_dir/verilated.o obj_dir/Vplus__ALL.a
	ghc $^ -lstdc++ -o $@
runTestPlus : TestPlus
	./TestPlus
