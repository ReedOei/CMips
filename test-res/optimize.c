int g(int x) {
    return x*x;
}

int f(int x) {
    int y = 40;
    int z = y + 50;

    if (x == 1) {
        x += z;
    }

    return x + 60 + g(60);
}

int h(int x) {
    return (x % 2) == 0;
}

