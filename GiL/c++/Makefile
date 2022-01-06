all: dylib

so:
	g++  gecode_wrapper.cpp space_wrapper.cpp -std=c++11 -dynamiclib -fPIC -F/Library/Frameworks -framework gecode -o ../sources/libgecode.so  -ffast-math -O3 

dylib: gecode_wrapper.cpp
	g++  gecode_wrapper.cpp space_wrapper.cpp -std=c++11 -dynamiclib -fPIC -F/Library/Frameworks -framework gecode -o ../sources/libgecode.dylib  -ffast-math -O3