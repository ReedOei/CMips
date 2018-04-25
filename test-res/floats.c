float g(float x, float y) {
    return x * y + y - x;
}

float f(float x) {
    return x * x * g(x + 4.5, x - 6.5);
}

float sum(float *arr, int len) {
    float result = 0;

    for (int i = 1; i < len; i++) {
        if (arr[i] > i * arr[i - 1]) {
            result += arr[i];
        } else if (arr[i] <= 10.5) {
            result *= arr[i];
        }
    }

    return result;
}

float h(float x, float y) {
    return x + y;
}

int main() {
    printf("%f\n", f(9.4));

    int floored = f(10.5);

    printf("%d\n", floored);

    float *arr = malloc(400);

    for (int i = 0; i < 100; i++) {
        arr[i] = i*i;
    }

    printf("%f\n", sum(arr, 10));

    printf("%f\n", h(1, 4));

    return 0;
}

