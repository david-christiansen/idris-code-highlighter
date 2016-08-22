PKG   := idris-code-highlighter
IDRIS ?= @idris

.PHONY: build clean highlight install rebuild

all: build

build: idrishl

idrishl: $(wildcard src/Highlight/*.idr)
	$(IDRIS) --build $(PKG).ipkg

clean:
	$(IDRIS) --clean $(PKG).ipkg
	@find src \! -name "*.idr" -type f -delete

highlight: rebuild
	cd src && find . -name "*.idh" -exec ../idrishl {} \;

install:
	$(IDRIS) --install $(PKG).ipkg

rebuild: clean build
