.PHONY: all clean repl

default: all

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
COMPILED_PS = ../src/compiled_ps
ERL_MODULES_VERSION = $(shell dhall-to-json < packages.dhall | jq '."erl-modules".version')

all: $(COMPILED_PS)

$(COMPILED_PS): output
	mkdir -p $(COMPILED_PS)
	cp -r output/* $(COMPILED_PS)
	@touch $(COMPILED_PS)

output: $(PS_SOURCEFILES) $(PS_ERL_FFI) .spago
	spago build
	@touch output

.spago: spago.dhall packages.dhall
	spago install
	touch .spago

clean:
	rm -rf $(OUTPUT)

