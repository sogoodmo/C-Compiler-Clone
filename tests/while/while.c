// MiniC program to test while loop

extern int foo(int x, int y);
extern int bar();

int test;
float f;
bool b;

int While(int n){
  int result;
  test = 11;
  result = 0;
  print_int(test); 
  while(result < 10){
    result = result + 1;  
    print_int(3+2, test+test, -result, result, print_int(3+2));    
  }
  return;
}