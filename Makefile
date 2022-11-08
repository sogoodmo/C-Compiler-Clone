CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option -fno-rtti -ferror-limit=250 

mccomp: mccomp.cpp
	$(CXX) mccomp.cpp $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp