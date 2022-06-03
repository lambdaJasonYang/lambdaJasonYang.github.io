---
title: lean makefile
tags: prog, C, QuickCode, OS
---

filepaths
  * `$(addsuffix .c,foo bar)` returns foo.c bar.c
  * `$(addprefix $(OUT)/testcpp/,$(CPP_SRCS:.cpp=.o))` returns path appended object, home/testcpp/myfuns.o
    *`$(CPP_SRCS:.cpp=.o)` replaces extension .cpp with .o  
	CPP_SRCS is myfuncs.cpp meaning this replaces myfuncs.cpp to myfuncs.o


```yaml
# tells us $(CXX) means g++ compiler
CXX ?= c++
# -O3 is a g++ flag for code optimization
CPPFLAGS = -O3
# first make the lean.mk makefile
include lean.mk


CPP_SRCS = myfuns.cpp
CPP_OBJS = $(addprefix $(OUT)/testcpp/,$(CPP_SRCS:.cpp=.o))
 

all: run_test run_interp

$(OUT)/testcpp/%.o: %.cpp
	@mkdir -p "$(@D)"
	$(CXX) -std=c++14 -c -o $@ $< $(CPPFLAGS) `leanc --print-cflags`
   #g++    -std=c++14 -c -o something/testcpp/myfuns.o myfunc.cpp -O3 `leanc --print-cflags`

# $< refers to %.cpp
# $@ refers to $(OUT)/testcpp/%.o


# to avoid conflicts between the system C++ stdlib needed by the above object file and the internal one used in the Lean runtime,
# we need to dynamically link the Lean runtime.

ifeq ($(OS),Windows_NT)
# make S.so find testcpp.so
  export PATH := $(BIN_OUT):$(PATH)
else
# find libleanshared.so
  TEST_SHARED_LINK_FLAGS := -Wl,-rpath,`lean --print-prefix`/lib/lean
endif

$(BIN_OUT)/testcpp.so: $(CPP_OBJS) | $(BIN_OUT)
	$(CXX) -shared -o $@ $^ `leanc -shared --print-ldflags`
   # g++ -shared -o $(BIN_OUT)/testcpp.so $(CPP_OBJS) `leanc -shared --print-ldflags`

$(BIN_OUT)/test: $(LIB_OUT)/libMain.a $(CPP_OBJS) | $(BIN_OUT)
	$(CXX) -o $@ $^ `leanc -shared --print-ldflags` -lleanshared $(TEST_SHARED_LINK_FLAGS)

run_test: $(BIN_OUT)/test
	$^

# also test interpreter; see doc/dev/ffi.md
$(BIN_OUT)/S.so: $(C_OUT)/Main/S.c $(BIN_OUT)/testcpp.so
	leanc -shared -o $@ $^

run_interp: $(BIN_OUT)/S.so
	lean --load-dynlib=$^ --run Main.lean
```

```yaml
out: a.req b.req c.req
	cat a.req b.req c.req > out

%.req: %.in
	cp $< $@
	echo Bleh >> $@
```

* `$<` refers to %.in which **first prereq** means all .in dependencies are processed
* `$@` refers to %.req which means **output** all the output are .req

result is shown below

```yaml
cp a.in a.req
echo Bleh >> a.req
cp b.in b.req
echo Bleh >> b.req
cp c.in c.req
echo Bleh >> c.req

cat a.req b.req c.req > out
```

