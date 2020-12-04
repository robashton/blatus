.PHONY: all

PUT = output
BUNDLE_NAME = bundle.js
PS_SOURCEFILE = ./dist/${BUNDLE_NAME}
JS_ASSET_FOLDER = ../priv/www/assets/

all: spago parcel

spago: $(PS_SOURCEFILE) 

parcel: $(JS_ASSET_FOLDER)$(BUNDLE_NAME)

$(PS_SOURCEFILE): $(shell find -L src -type f)
	spago bundle-app --main Pure.Main --to $(PS_SOURCEFILE)

$(JS_ASSET_FOLDER)$(BUNDLE_NAME): $(PS_SOURCEFILE) node_modules
	echo B $(BUNDLE_NAME), P $(PS_SOURCEFILE)
	node_modules/.bin/parcel build --out-dir $(JS_ASSET_FOLDER) --out-file $(BUNDLE_NAME) $(PS_SOURCEFILE) --no-source-maps --no-cache

watch-spago:
	spago bundle-app --watch --main Pure.Client --to $(PS_SOURCEFILE)

watch-parcel: node_modules
	./node_modules/.bin/parcel watch --out-dir $(JS_ASSET_FOLDER) --out-file $(BUNDLE_NAME) $(PS_SOURCEFILE) --no-source-maps

clean-all:
	rm -rf node_modules output .spago .cache dist

node_modules: package.json
	npm install
