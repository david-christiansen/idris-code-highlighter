#!/bin/bash

idris --clean idris-code-highlighter.ipkg
idris --build idris-code-highlighter.ipkg
find . -name "*.idh" -exec ./idrishl {} \;
