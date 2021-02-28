build_dir = build
HADA-BIN = ../dist-newstyle/build/x86_64-linux/ghc-8.8.4/hada-0.1.0.0/x/hada/build/hada/hada

VERILATOR ?= $(shell which verilator)

ifeq ($(VERILATOR),)
$(error Cannot find verilator binary)
endif

VERILATOR-CFLAGS ?= $(shell pkg-config --cflags verilator)
ifeq ($(VERILATOR-CFLAGS),)
$(error Cannot determine C flags for compiling verilated CPP source. Please specify the necessary C flags to VERILATOR-CFLAGS)
endif

$(build_dir):
	mkdir $<

HADA-PACKAGE := ../lib/hada.sv
# $1: Bind name
# $2: Test name
# Defined variables:
#  $(2)VLibs: Collection of the verilated .a libraries for each bind
#  $(2)CPPObjs: Collection of the object files for each bind
#  $(2)WHS: Collection of the wrapper Haskell file for each bind
define TestRules
$(build_dir)/$1.sv $(build_dir)/W$1.cpp $(build_dir)/W$1.hs : $2.hs $(HADA-BIN) $(build_dir)
	$(HADA-BIN) $$< -o $(build_dir)/$1.sv -w $(build_dir)/W$1 -t $1
$(build_dir)/$1/V$(1)__ALL.a $(build_dir)/$1/V$1.h: $(HADA-PACKAGE) $(build_dir)/$1.sv $(build_dir)/W$1.cpp
	$(VERILATOR) -cc --build -sv --Mdir $$(dir $$@) --prefix V$1 $$^ 
$(build_dir)/W$1.cpp.o: $(build_dir)/W$1.cpp $(build_dir)/$1/V$1.h
	$(CXX) $(VERILATOR-CFLAGS) -I./$(build_dir)/$1 -c $$< -o $$@
$(2)VLibs += $(build_dir)/$1/V$(1)__ALL.a
$(2)CPPObjs += $(build_dir)/W$(1).cpp.o
$(2)WHS += $(build_dir)/W$(1).hs
endef

NumBinds = plus minus mul neg abs_ sig eq neq lt le gt ge band bor bxor bneg
NumBinds += $(foreach d,U 8 U8 16 U16 32 U32 64 U64,plus$(d) minus$(d) mul$(d) neg$(d) abs$(d) sig$(d) eq$(d) neq$(d) lt$(d) le$(d) gt$(d) ge$(d) band$(d) bor$(d) bxor$(d) bneg$(d))
$(foreach b,$(NumBinds),$(eval $(call TestRules,$(b),Num)))

FirstBind := $(firstword $(NumBinds))
VerilatedObj := $(build_dir)/$(FirstBind)/verilated.o
$(VerilatedObj) : $(build_dir)/$(FirstBind)/V$(FirstBind)__ALL.a
	$(MAKE) -C $(build_dir)/$(FirstBind) -f V$(FirstBind).mk verilated.o

testNum : TestNum.hs $(NumWHS) $(NumCPPObjs) $(VerilatedObj) $(NumVLibs)
	ghc -i$(build_dir) $^ -lstdc++ -o $@
runTestNum : testNum
	./$^

PrimBinds = $(subst _,,$(foreach d,_ U 8 U8 16 U16 32 U32 64 U64,plusOne$(d) lshc$(d) rshc$(d)))
$(foreach b,$(PrimBinds),$(eval $(call TestRules,$(b),Prim)))

testPrim : TestPrim.hs $(PrimWHS) $(PrimCPPObjs) $(VerilatedObj) $(PrimVLibs)
	ghc -i$(build_dir) $^ -lstdc++ -o $@
runTestPrim : testPrim
	./$^

PrecedenceBinds = negSum mulSum sumSub sumSum eqBand bandXor xorBor
$(foreach b,$(PrecedenceBinds),$(eval $(call TestRules,$(b),Precedence)))

testPrecedence : TestPrecedence.hs $(PrecedenceWHS) $(PrecedenceCPPObjs) $(VerilatedObj) $(PrecedenceVLibs)
	ghc -i$(build_dir) $^ -lstdc++ -o $@
runTestPrecedence : testPrecedence
	./$^

runAllTests: testNum testPrim testPrecedence
	./testNum && ./testPrim && ./testPrecedence
