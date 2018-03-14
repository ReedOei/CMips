int square(int x) {
    return x*x;
}

int caller(int (*f)(int), int (*g)(int), int y) {
    return f(y) + g(y);
}

int main() {
    printf("%d", caller(square, square, 10));

    return 0;
}

