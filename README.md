# Use:
  To run the existing application: `make run`

  To run SBT with the correct settings (on Linux):
  `./sbtw`

  This script will accept arguments and pass them to SBT. It will also
  automatically clone and build the Z3 submodule (using the Makefile) if
  `z3/build/com.microsoft.z3.jar` is not present.

  Some sample input files are found in the `input/` directory. In addition, some 
  demo files named `demo.mini` and `draw.mini` (and which are intended to be
  realisticish demos of the PACT tester) are found in the root directory.

# Documentation:
See http://rise4fun.com/z3/tutorial/guide for a guide to the basic Z3 features.
The full repo is at https://github.com/Z3Prover/z3
