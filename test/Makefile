build_dir = build
HADA-BIN = ../dist-newstyle/build/x86_64-linux/ghc-8.8.4/hada-0.1.0.0/x/hada/build/hada/hada

NumBinds = plus minus mul neg abs_ sig eq neq $(foreach d,W 8 W8 16 W16 32 W32 64 W64,plus$(d) minus$(d) mul$(d) neg$(d) abs$(d) sig$(d) eq$(d) neq$(d))
VERILATOR ?= $(shell which verilator)

ifeq ($(VERILATOR),)
$(error Cannot find verilator binary)
endif

VERILATOR-CFLAGS ?= $(shell pkg-config --cflags verilator)
ifeq ($(VERILATOR-CFLAGS),)
$(error Cannot determine C flags for compiling verilated CPP source. Please specify the necessary C flags to VERILATOR-CFLAGS)
endif

NumSVs := $(NumBinds:%=$(build_dir)/%.sv)
NumVLibs := $(foreach b,$(NumBinds),$(build_dir)/$b/V$(b)__ALL.a)
NumCPPObjs := $(NumBinds:%=$(build_dir)/W%.cpp.o)
NumWHS := $(NumBinds:%=$(build_dir)/W%.hs) # Haskell wrapper
HADA-PACKAGE := ../lib/hada.sv

$(build_dir):
	mkdir $<

define NumRules
$(build_dir)/$1.sv $(build_dir)/W$1.cpp $(build_dir)/W$1.hs : Num.hs $(HADA-BIN) $(build_dir)
	$(HADA-BIN) $$< -o $(build_dir)/$1.sv -w $(build_dir)/W$1 -t $1
$(build_dir)/$1/V$(1)__ALL.a $(build_dir)/$1/V$1.h: $(HADA-PACKAGE) $(build_dir)/$1.sv $(build_dir)/W$1.cpp
	$(VERILATOR) -cc --build -sv --Mdir $$(dir $$@) --prefix V$1 $$^ 
$(build_dir)/W$1.cpp.o: $(build_dir)/W$1.cpp $(build_dir)/$1/V$1.h
	$(CXX) $(VERILATOR-CFLAGS) -I./$(build_dir)/$1 -c $$< -o $$@
endef

$(foreach b,$(NumBinds),$(eval $(call NumRules,$(b))))

FirstNumBind := $(firstword $(NumBinds))
VerilatedObj := $(build_dir)/$(FirstNumBind)/verilated.o
$(VerilatedObj) : $(build_dir)/$(FirstNumBind)/V$(FirstNumBind)__ALL.a
	$(MAKE) -C $(build_dir)/$(FirstNumBind) -f V$(FirstNumBind).mk verilated.o 

testNum : TestNum.hs $(NumWHS) $(NumCPPObjs) $(VerilatedObj) $(NumVLibs)
	ghc -i$(build_dir) $^ -lstdc++ -o $@

runTestNum : testNum
	./$^

