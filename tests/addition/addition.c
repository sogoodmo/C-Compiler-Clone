// MiniC program to test addition

extern int print_int(int X);
int a;


void bob () {
    print_int(a);
    a = a + 1;
}
int foo() {
    print_int(a);
    a = a + 1;
    //ADDED
    {
        int a;
        a = 69;
        print_int(a);
        bob();
        print_int(a);

    }
    print_int(a);
    return a;
}

int global() {
    a = 5;
    return foo();
}