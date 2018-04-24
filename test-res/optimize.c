int g(int x) {
    return x*x;
}

int f(int x) {
    int y = 40;
    int z = 40 + y;

    if (x == 1) {
        x += z;
    }

    return x + 60 + g(60);
}

int h(int x) {
    return (x % 2) == 0;
}

int f2(int x) {
    return 40 + x;
}

int f3(int x) {
    return 40 - 40;
}

int main() {
    printf("%d", g(3) + f(10) + f2(h(f3(20))));
}

