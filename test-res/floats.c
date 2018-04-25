float g(float x, float y) {
    return x * y + y - x;
}

float f(float x) {
    return x * x * g(x + 4.5, x - 6.5);
}

float sum(float *arr, int len) {
    float result = 0;

    for (int i = 0; i < len; i++) {
        result += arr[i];
    }

    return result;
}

int main() {
    printf("%f\n", f(9.4));

    int floored = f(10.5);

    printf("%d\n", floored);

    float *arr = malloc(40);

    for (int i = 0; i < 10; i++) {
        arr[i] = i*i;
    }

    printf("%f\n", sum(arr, 10));

    return 0;
}

