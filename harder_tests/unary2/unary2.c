// Test written by Toby
extern int print_int(int X);
// extern float print_float(float X);

int unary2() {
    // int z;
    // int z2;
    // bool X;
    // X = true;
    // z2 = -!f;

    float f;
    int i;
    int i2;
    bool b;
    bool b2;
    int z;


    // i2 = 0;
    f = 0.0;
    i = 1;
    b = -!f + i; //-1 + 1
    b2 = true || b; // true or 0
    i2 = b2; //

    return i2;
}