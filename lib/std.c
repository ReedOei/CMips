struct List {
    List *value; // Because we don't have real typecasting...
    List *tail;
};

void *car(List *list) {
    return list->value;
}

List *cdr(List *list) {
    return list->tail;
}

List *cons(void *value, List *list) {
    List *newNode = malloc(8);
    newNode->value = value;
    newNode->tail = list;

    return newNode;
}

int empty(List *list) {
    return list == NULL;
}

int length(List *list) {
    int i = 0;
    for (List *cur = list; cur != NULL; cur = cur->tail) {
        i++;
    }

    return i;
}

List *append(void *value, List *list) {
    if (list == NULL) {
        return cons(value, NULL);
    }

    return cons(list->value, append(value, list->tail));
}

void *apply(List *args) {
    void *(*f)(List *) = args->value;
    int argcount = args->tail->value;
    List *argVals = args->tail->tail;

    if (length(argVals) >= argcount) {
        return f(argVals);
    }

    return args;
}

