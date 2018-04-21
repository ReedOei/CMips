struct List {
    void *value;
    List *tail;
};

int square(int x) {
    return x * x;
}

int sum(List *list) {
    if (list == NULL) {
        return 0;
    }

    return list->value + sum(list->tail);
}

List* map(void (*f)(void), List *list) {
    if (list == NULL) {
        return NULL;
    }

    list->value = f(list->value);
    list->tail = map(f, list->tail);

    return list;
}

int main() {
    List *list = malloc(8);
    list->value = 0;

    List *cur = list;
    for (int i = 0; i < 10; i++) {
        List *next = malloc(8);
        next->value = i;
        cur->tail = next;
        cur = next;
    }

    printf("%d", sum(map(square, list)));

    return 0;
}
