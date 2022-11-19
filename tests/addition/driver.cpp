#include <iostream>
#include <cstdio>

// clang++ driver.cpp addition.ll -o add

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT int print_int(int X) {
  fprintf(stderr, "%d\n", X);
  return 0;
}
extern "C" DLLEXPORT int print_int(int X, int Y) {
  fprintf(stderr, "%d %d\n", X, Y);
  return 0;
}

extern "C" DLLEXPORT float print_float(float X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

extern "C" {
    int add();
}

int main() {
    // print_int(add());
    std::cout << add() << std::endl;
    // if(add(1) == 5) 
      // std::cout << "PASSED Result: " << add(1) << std::endl;
  	// else 
  	  // std::cout << "FALIED Result: " << add(1) << std::endl;
}