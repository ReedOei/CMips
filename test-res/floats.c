float g(float x, float y) {
    return x * y + y - x;
}

float f(float x) {
    return x * x * g(x + 4.5, x - 6.5);
}

int main() {
    printf("%f", f(9.4));

    return 0;
}

