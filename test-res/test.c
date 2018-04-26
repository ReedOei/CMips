struct T {
    int *x;
};

int g(T *t, int i) {
    t->x[i] = 40;
    return t->x[i];
}

int f(int x) {
    return x * x + x;
}

int main() {
    printf("%d", f(3));

    return 0;
}

