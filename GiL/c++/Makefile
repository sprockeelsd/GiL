all: so dylib

so:
	g++ -fPIC gecode_problem.cpp space_wrapper.cpp -std=c++11 -shared -lgecodedriver -lgecodeflatzinc -lgecodefloat -lgecodeint -lgecodekernel -lgecodeminimodel -lgecodesearch -lgecodeset -lgecodesupport -o ../sources/libgecode.so -O3 -ffast-math

dylib: gecode_problem.cpp
	g++  gecode_problem.cpp space_wrapper.cpp -std=c++11 -dynamiclib -fPIC -F/Library/Frameworks -framework gecode -o ../sources/libgecode.dylib  -ffast-math -O3
	install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode ../sources/libgecode.dylib

clean:
	rm -f *.o *.so *.dylib ../sources/*.so ../sources/*.dylib