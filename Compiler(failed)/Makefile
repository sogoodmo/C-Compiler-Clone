CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option -ferror-limit=250 

SRC= ast_helper.cpp codegen_helper.cpp codegen.cpp lexer.cpp parser_helper.cpp parser.cpp ast_to_string.cpp mccomp.cpp
mccomp: mccomp.cpp
	$(CXX) $(SRC) $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp