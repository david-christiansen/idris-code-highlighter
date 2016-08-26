IPKG  := idris-code-highlighter.ipkg
PKG   := idrishighlighter
IDRIS ?= @idris

IDR_SRCS := $(wildcard src/Highlight/*.idr)
IDH_SRCS := $(addprefix Highlight/,$(notdir $(IDR_SRCS:.idr=.idh)))

.PHONY: build clean highlight install rebuild

all: build

build: idrishl

idrishl: $(wildcard src/Highlight/*.idr) $(IPKG)
	$(IDRIS) --build $(IPKG)

clean:
	$(IDRIS) --clean $(IPKG)
	@find src \! -name "*.idr" -type f -delete

highlight: idrishl
	@cd src && $(foreach idh,$(IDH_SRCS),../idrishl $(idh);)

install:
	$(IDRIS) --install $(IPKG)

rebuild: clean build
