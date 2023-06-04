CURDIR		:= $(shell pwd)
BASEDIR		:= $(abspath $(dir $(CURDIR)))
PROJECT		?= egit
PROJECT		:= $(strip $(PROJECT))

ERL_CXXFLAGS ?= $(shell erl -noshell -noinput -eval "io:format(\"-I~ts/erts-~ts/include -I~ts\", [code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)]), halt(0).")
ERL_LDFLAGS  ?= $(shell erl -noshell -noinput -eval "io:format(\"-L~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

DEBUG     ?= 0
NIF_DEBUG ?= $(DEBUG)

# System type and C compiler/flags.

ifeq ($(NIF_DEBUG),0)
	OPTIMIZE = -O3 -DNDEBUG
else
	OPTIMIZE = -O0 -g
endif

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Linux)
	CXX = g++
	CXXFLAGS ?= -mavx2 -finline-functions -Wall -std=c++20 
else ifeq ($(UNAME_SYS), Darwin)
	CXX = g++
	CXXFLAGS ?= -mavx2 -arch x86_64 -finline-functions -Wall -std=c++20 -pie
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CXX = g++
	CXXFLAGS ?= -mavx2 -finline-functions -Wall -std=c++20
endif

CXXFLAGS   += $(SIMDJSON_FLAGS) $(OPTIMIZE) -fPIC $(ERL_CXXFLAGS)
LDFLAGS    += $(ERL_LDFLAGS) -lei -lgit2 -shared

SRC_DIR     = $(CURDIR)
SO_OUTPUT  ?= $(BASEDIR)/priv/$(PROJECT).so

SOURCES    := $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS     = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

# Targets

all: $(SO_OUTPUT)

clean:
	rm -f $(SO_OUTPUT) *.o

$(SO_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $(SO_OUTPUT)

%.o: %.cpp $(wildcard *.hpp)
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<
