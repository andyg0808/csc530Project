.PHONY: run clean deepclean
# We use a wrapper script for SBT to ensure it's launched with the necessary
# properties on Linux
SBT = "./sbtw"
run: z3/build/com.microsoft.z3.jar
	$(SBT) run

z3/build/com.microsoft.z3.jar:
	git submodule init
	git submodule update
	cd z3; python3 ./scripts/mk_make.py --java
	cd z3/build; $(MAKE)

# This is needed to run the Antlr-generated code
lib/antlr-4.6-complete.jar:
	curl -o $@ https://github.com/antlr/website-antlr4/raw/gh-pages/download/antlr-4.6-complete.jar

# Basic clean for everyday use
clean:
	$(SBT) clean

# Clean z3 as well
deepclean: clean
	cd z3; rm -r build
	cd z3; git clean -xdf
