run:
	# Seems that we have to set LD_LIBRARY_PATH to make ld happy for loading the
	# additional .so files needed, and that we have to set java.library.path
	# because it looks for liblibz3java.so on linux, which doesn't exist. But
	# that's just a guess, and could be completely wrong.
	LD_LIBRARY_PATH=../z3/build  sbt -Djava.library.path=../z3/build/libz3java.so run
