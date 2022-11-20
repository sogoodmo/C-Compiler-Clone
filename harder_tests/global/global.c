// extern int print_int(int X);
// int a;

// int foo() {
//     a = a + 1;
//     return a;
// }

// int global() {
//     a = 5;
//     return foo();
// }

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
        a = -bob();
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