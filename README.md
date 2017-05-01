# Use:
  To run the existing application: `make run`

  To run SBT with the correct settings (on Linux):
  `./sbtw`

  This script will accept arguments and pass them to SBT. It will also
  automatically clone and build the Z3 submodule (using the Makefile) if
  `z3/build/com.microsoft.z3.jar` is not present.

