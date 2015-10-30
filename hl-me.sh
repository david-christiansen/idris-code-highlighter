#!/bin/bash

idris --clean idris-code-highlighter.ipkg
idris --build idris-code-highlighter.ipkg
cd src
find . -name "*.idh" -exec ../idrishl {} \;
