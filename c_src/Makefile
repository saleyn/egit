CURDIR		:= $(shell pwd)
BASEDIR		:= $(abspath $(dir $(CURDIR)))
PROJECT		?= git
PROJECT		:= $(strip $(PROJECT))

ERL_CXXFLAGS ?= $(shell erl -noshell -noinput -eval "io:format(\"-I~ts/erts-~ts/include -I~ts\", [code:root_dir(), erlang:system_info(version), code:lib_dir(erl_interface, include)]), halt(0).")
ERL_LDFLAGS  ?= $(shell erl -noshell -noinput -eval "io:format(\"-L~ts\", [code:lib_dir(erl_interface, lib)]), halt(0).")

DEBUG     ?= 0
NIF_DEBUG ?= $(DEBUG)

NIF_PRINT := $(if $(NIF_PRINT),-DNIF_DEBUG)

CXX       ?= g++

CXX_VSN   ?= $(shell $(CXX) --version | sed -n '1s/^[^0-9]\+\([0-9]\+\)\(.[0-9-]\)\+.*$$/\1/p')

ifeq ($(STD_FMT),)
  ifeq ($(basename $(CXX)),g++)
    ifeq ($(shell expr $(CXX_VSN) \>= 13),1)
      STD_FMT=1
    else
      STD_FMT=0
    endif
  else ifeq ($(basename $(CXX)),clang++)
    ifeq ($(shell expr $(CXX_VSN) \>= 15),1)
      STD_FMT=1
    else
      STD_FMT=0
    endif
  else
    STD_FMT=0
  endif
endif

ifeq ($(STD_FMT),0)
  $(warning ==> $(CXX) major version: $(CXX_VSN))
  CPPFLAGS += -DFMT_HEADER_ONLY -DUSE_FMT_LIB=1 -Ifmt/include
endif

# System type and C compiler/flags.

ifeq ($(NIF_DEBUG),0)
  OPTIMIZE = -O3 -DNDEBUG
else
  OPTIMIZE = -O0 -g
endif

UNAME_SYS  := $(shell uname -s)
ifeq ($(UNAME_SYS), Linux)
  CXXFLAGS ?= -mavx2 -finline-functions -Wall -std=c++20
else ifeq ($(UNAME_SYS), Darwin)
  CXXFLAGS ?= -mavx2 -arch x86_64 -finline-functions -Wall -std=c++20 -pie
  LDFLAGS  ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
  CXXFLAGS ?= -mavx2 -finline-functions -Wall -std=c++20
endif

CXXFLAGS   += $(OPTIMIZE) -fPIC $(ERL_CXXFLAGS)
LDFLAGS    += $(ERL_LDFLAGS) -lei -lgit2 -shared

SRC_DIR     = $(CURDIR)
SO_OUTPUT  ?= $(BASEDIR)/priv/$(PROJECT).so

SOURCES    := $(wildcard $(SRC_DIR)/*.cpp)
OBJECTS     = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_CPP = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(NIF_PRINT) -c

# Targets

all: $(SO_OUTPUT)

clean:
	rm -f $(SO_OUTPUT) *.o

info:
	@echo NIF_PRINT=$(NIF_PRINT)
	@echo NIF_DEBUG=$(NIF_DEBUG)
	@echo COMPILE_CPP=$(COMPILE_CPP)

$(SO_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(CXX) $(OBJECTS) $(LDFLAGS) -o $(SO_OUTPUT)

%.o: %.cpp $(wildcard *.hpp)
	@if [ $(STD_FMT) -eq 0 ]; then \
	  echo "==> Updating fmt submodule"; \
		git config --global --add safe.directory '*'; \
		git submodule init; \
		git submodule update --remote; \
		ls -l *; \
	fi
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<
