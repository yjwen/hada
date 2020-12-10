build_dir = build

NumBinds = plus minus mul $(foreach d,W 8 W8 16 W16 32 W32 64 W64,plus$(d) minus$(d) mul$(d))
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
define NumRules
$(build_dir)/$1.sv $(build_dir)/W$1.cpp $(build_dir)/W$1.hs : Num.hs
	[ -d $(build_dir) ] || mkdir $(build_dir)
	cd .. && cabal run hada --\
	test/$< -o test/$(build_dir)/$1.sv -w test/$(build_dir)/W$1 -t $1
$(build_dir)/$1/V$(1)__ALL.a $(build_dir)/$1/V$1.h: $(build_dir)/$1.sv $(build_dir)/W$1.cpp
	$(VERILATOR) -cc --build -sv --Mdir $$(dir $$@) $$^
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

