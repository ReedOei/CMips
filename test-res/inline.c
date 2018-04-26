mips int f(int x) {
    mul $v0, $a0, $a0 # Comment here to test comment parsing.
    jr $ra
}

mips int f2(int *x) {
    lw $v0, 0($a0)
    jr $ra
}

// This function would not be inlined with the lax rules, but we should find that it
// is worth inlining with the more aggressive inlining.
int h(int k, int l) {
    return k * l * l + l - k*l;
}

int g(int y, int z) {
    return h(y,z);
}

int m(int y, int z) {
    // Create a bunch of local variables so that there would be so much stack usage that
    // inlining wouldn't be worth it.
    int a = y+z;
    int b = y*z;
    int c = y - z;
    int d = a*b;
    int e = a-b + c;
    int f = a*b*c*d*e+a;
    int g = a+b+c-d*e/a;
    int h = b*d/g;
    int i = b*d*d*d*d;
    int j = a*b*c*d*e*f*g*h*i*j;
    int k = j+i+h;
    int l = j*k-a*c;
    int m = k*l-e+f;
    return a/b*c/d*e/f*g/h+i-j;
}

int main() {
    int x = f(10);

    int *y = malloc(4);
    *y = 4;

    printf("%d %d %d", x, f2(y), g(*y, 7));

    return 0;
}

