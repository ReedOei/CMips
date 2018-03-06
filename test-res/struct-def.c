struct node_t {
    int * data; // pointer to an integer
    int x;
    int y;
    node_t * next; // pointer to another node_t
};

int sum(node_t *node) {
    return node->next;
}

