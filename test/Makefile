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

# $1: Test suite name
# $2: Binds
define testSuite
$$(foreach b,$(2),$$(eval $$(call TestRules,$$(b),$(1))))
$(1)FirstBind := $$(firstword $(2))
$(1)VerilatedObj := $$(build_dir)/$$($(1)FirstBind)/verilated.o
$$($(1)VerilatedObj) : $$(build_dir)/$$($(1)FirstBind)/V$$($(1)FirstBind)__ALL.a
	$(MAKE) -C $$(build_dir)/$$($(1)FirstBind) -f V$$($(1)FirstBind).mk verilated.o
test$(1) : Test$(1).hs $$($(1)WHS) $$($(1)CPPObjs) $$($(1)VerilatedObj) $$($(1)VLibs)
	ghc -i$(build_dir) $$^ -lstdc++ -o $$@
runTest$(1) : test$(1)
	@echo "Testing $(1):" && ./$$^
RunAllTestSuites += $$(runTest$(1))
endef

NumBinds = plus minus mul neg abs_ sig eq neq lt le gt ge band bor bxor bneg
NumBinds += $(foreach d,U 8 U8 16 U16 32 U32 64 U64,plus$(d) minus$(d) mul$(d) neg$(d) abs$(d) sig$(d) eq$(d) neq$(d) lt$(d) le$(d) gt$(d) ge$(d) band$(d) bor$(d) bxor$(d) bneg$(d))
$(eval $(call testSuite,Num,$(NumBinds)))

PrimBinds = $(subst _,,$(foreach d,_ U 8 U8 16 U16 32 U32 64 U64,plusOne$(d) lshc$(d) rshc$(d)))
$(eval $(call testSuite,Prim,$(PrimBinds)))

PrecedenceBinds = negSum mulSum sumSub sumSum eqBand bandXor xorBor
$(eval $(call testSuite,Precedence,$(PrecedenceBinds)))

CaseBinds = andOrBool
$(eval $(call testSuite,Case,$(CaseBinds)))

runAllTests: $(RunAllTestSuites)

