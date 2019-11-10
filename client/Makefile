.PHONY: all clean


PS_SRC = src
OUTPUT = output
SOURCES = $(shell find -L $(PS_SRC) -name *.purs)

all: bundle.js

bundle.js: output
	purs bundle 'output/**/*.js' -o bundle.js -m 'Pure.Main' --main 'Pure.Main'
	cp bundle.js ../priv/www/assets/

output: $(SOURCES) .psc-package
	psc-package sources | xargs purs compile --codegen js  'src/**/*.purs' 
	touch output

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)/*

