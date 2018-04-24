.data
d2: .asciiz "]"
d1: .asciiz "["
d0: .asciiz ","

.text
# struct List


car:
    # void * car(List * list);
    
    # return (*list).value;
    lw $t0, 0($a0)
    move $v0, $t0
    jr $ra

cdr:
    # List * cdr(List * list);
    
    # return (*list).tail;
    lw $t0, 4($a0)
    move $v0, $t0
    jr $ra

cons:
    # List * cons(void * value,List * list);
    sub $sp, $sp, 12
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    
    move $s0, $a0
    move $s1, $a1
    
    # List * newNode = malloc(8);
    li $v0, 9
    li $a0, 8
    syscall
    move $t0, $v0
    
    # (*newNode).value = value;
    move $t1, $s0
    sw $t1, 0($t0)
    
    # (*newNode).tail = list;
    move $t1, $s1
    sw $t1, 4($t0)
    
    # return newNode;
    move $v0, $t0
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    add $sp, $sp, 12
    jr $ra

empty:
    # int empty(List * list);
    
    # return (list == NULL);
    li $t0, 1
    beq $a0, $0, end_eq_test
    li $t0, 0
    end_eq_test:
    move $v0, $t0
    jr $ra

length:
    # int length(List * list);
    
    # int i = 0;
    li $t0, 0
    
    # for (List * cur = list; (cur != NULL); cur = (*cur).tail)
    
    # List * cur = list;
    
    # while ((cur != NULL))
    while:
    beq $a0, $0, while_end
    
    # i++;
    add $t0, $t0, 1
    
    # cur = (*cur).tail;
    lw $t1, 4($a0)
    move $a0, $t1
    j while
    while_end:
    
    # return i;
    move $v0, $t0
    jr $ra

append:
    # List * append(void * value,List * list);
    sub $sp, $sp, 16
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    sw $s2, 12($sp)
    
    move $s0, $a0
    move $s1, $a1
    
    # if ((list == NULL))
    bne $s1, $0, if_end
    
    # return cons(value, NULL);
    move $a0, $s0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $v0, $t0
    j append_end
    if_end:
    
    # return cons((*list).value, append(value, (*list).tail));
    lw $s2, 0($s1)
    lw $t0, 4($s1)
    move $a0, $s0
    move $a1, $t0
    jal append
    move $t0, $v0
    move $a0, $s2
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $v0, $t0
    append_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    lw $s2, 12($sp)
    add $sp, $sp, 16
    jr $ra

apply:
    # void * apply(List * args);
    sub $sp, $sp, 20
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    sw $s2, 12($sp)
    sw $s3, 16($sp)
    
    move $s0, $a0
    
    # void *(List *) f = (*args).value;
    lw $t0, 0($s0)
    move $s1, $t0
    
    # int argcount = (*(*args).tail).value;
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    move $s2, $t1
    
    # List * argVals = (*(*args).tail).tail;
    lw $t0, 4($s0)
    lw $t1, 4($t0)
    move $s3, $t1
    
    # if ((length(argVals) >= argcount))
    move $a0, $s3
    jal length
    move $t0, $v0
    blt $t0, $s2, if_end_1
    
    # return f(argVals);
    move $a0, $s3
    jalr $s1
    move $t0, $v0
    move $v0, $t0
    j apply_end
    if_end_1:
    
    # return args;
    move $v0, $s0
    apply_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    lw $s2, 12($sp)
    lw $s3, 16($sp)
    add $sp, $sp, 20
    jr $ra

build:
    # List * build(List * args);
    sub $sp, $sp, 12
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    
    
    # if (((*args).value > (*(*args).tail).value))
    lw $t0, 0($a0)
    lw $t1, 4($a0)
    lw $t2, 0($t1)
    ble $t0, $t2, if_end_2
    
    # return NULL;
    li $v0, 0
    j build_end
    j else_end
    if_end_2:
    
    # else
    
    # return cons((*args).value, build(cons(((*args).value + 1), cons((*(*args).tail).value, NULL))));
    lw $s0, 0($a0)
    lw $t0, 0($a0)
    add $s1, $t0, 1
    lw $t0, 4($a0)
    lw $t1, 0($t0)
    move $a0, $t1
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal build
    move $t0, $v0
    move $a0, $s0
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $v0, $t0
    j build_end
    else_end:
    
    build_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    add $sp, $sp, 12
    jr $ra

filter:
    # List * filter(List * args);
    sub $sp, $sp, 16
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    sw $s2, 12($sp)
    
    move $s0, $a0
    
    # if (empty((*(*args).tail).value))
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    move $a0, $t1
    jal empty
    move $t0, $v0
    beq $t0, $0, if_end_3
    
    # return NULL;
    li $v0, 0
    j filter_end
    j else_end_1
    if_end_3:
    
    # else
    
    # void * temp_exec_f_name0 = (*args).value;
    lw $t0, 0($s0)
    move $t1, $t0
    
    # List * temp_exec_f_args5 = append((*(*(*args).tail).value).value, temp_exec_f_name0);
    lw $t0, 4($s0)
    lw $t2, 0($t0)
    lw $t0, 0($t2)
    move $a0, $t0
    move $a1, $t1
    jal append
    move $t0, $v0
    move $t1, $t0
    
    # if (apply(temp_exec_f_args5))
    move $a0, $t1
    jal apply
    move $t0, $v0
    beq $t0, $0, if_end_4
    
    # return cons((*(*(*args).tail).value).value, filter(cons((*args).value, cons((*(*(*args).tail).value).tail, NULL))));
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    lw $s1, 0($t1)
    lw $s2, 0($s0)
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    lw $t0, 4($t1)
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s2
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal filter
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $v0, $t0
    j filter_end
    j else_end_2
    if_end_4:
    
    # else
    
    # return filter(cons((*args).value, cons((*(*(*args).tail).value).tail, NULL)));
    lw $s1, 0($s0)
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    lw $t0, 4($t1)
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal filter
    move $t0, $v0
    move $v0, $t0
    j filter_end
    else_end_2:
    else_end_1:
    
    filter_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    lw $s2, 12($sp)
    add $sp, $sp, 16
    jr $ra

divides:
    # void * divides(List * args);
    
    # return (((*(*args).tail).value % (*args).value) == 0);
    lw $t0, 4($a0)
    lw $t1, 0($t0)
    lw $t0, 0($a0)
    rem $t2, $t1, $t0
    li $t0, 1
    beq $t2, $0, end_eq_test_1
    li $t0, 0
    end_eq_test_1:
    move $v0, $t0
    jr $ra

primecheck:
    # int primecheck(List * args);
    sub $sp, $sp, 12
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    
    move $s0, $a0
    
    # if (((*args).value == 1))
    lw $t0, 0($s0)
    li $t1, 1
    bne $t0, $t1, if_end_5
    
    # return 0;
    li $v0, 0
    j primecheck_end
    j else_end_3
    if_end_5:
    
    # else
    
    # if ((((*(*args).tail).value * (*(*args).tail).value) > (*args).value))
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    lw $t0, 4($s0)
    lw $t2, 0($t0)
    mul $t0, $t1, $t2
    lw $t1, 0($s0)
    ble $t0, $t1, if_end_6
    
    # return 1;
    li $v0, 1
    j primecheck_end
    j else_end_4
    if_end_6:
    
    # else
    
    # if (divides(cons((*(*args).tail).value, cons((*args).value, NULL))))
    lw $t0, 4($s0)
    lw $s1, 0($t0)
    lw $t0, 0($s0)
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal divides
    move $t0, $v0
    beq $t0, $0, if_end_7
    
    # return 0;
    li $v0, 0
    j primecheck_end
    j else_end_5
    if_end_7:
    
    # else
    
    # return primecheck(cons((*args).value, cons(((*(*args).tail).value + 1), NULL)));
    lw $s1, 0($s0)
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    add $t0, $t1, 1
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal primecheck
    move $t0, $v0
    move $v0, $t0
    j primecheck_end
    else_end_5:
    else_end_4:
    else_end_3:
    
    primecheck_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    add $sp, $sp, 12
    jr $ra

prime_2:
    # int(void *,void *) prime(List * args);
    sub $sp, $sp, 8
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    
    
    # return primecheck(cons((*args).value, cons(2, NULL)));
    lw $s0, 0($a0)
    li $a0, 2
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s0
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal primecheck
    move $t0, $v0
    move $v0, $t0
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    add $sp, $sp, 8
    jr $ra

printhelper:
    # void * printhelper(List * args);
    sub $sp, $sp, 8
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    
    move $s0, $a0
    
    # if ((1 == (*args).value))
    li $t0, 1
    lw $t1, 0($s0)
    bne $t0, $t1, if_end_8
    
    # return printf("%d", (*(*args).tail).value);
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    move $a0, $t1
    li $v0, 1
    syscall
    move $v0, $t0
    j printhelper_end
    j else_end_6
    if_end_8:
    
    # else
    
    # return printf("%d,", (*(*args).tail).value);
    lw $t0, 4($s0)
    lw $t1, 0($t0)
    move $a0, $t1
    li $v0, 1
    syscall
    la $a0, d0
    li $v0, 4
    syscall
    move $v0, $t0
    j printhelper_end
    else_end_6:
    
    printhelper_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    add $sp, $sp, 8
    jr $ra

printmain:
    # int printmain(List * args);
    sub $sp, $sp, 12
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    sw $s1, 8($sp)
    
    move $s0, $a0
    
    # printhelper(cons(length((*args).value), cons((*(*args).value).value, NULL)));
    lw $t0, 0($s0)
    move $a0, $t0
    jal length
    move $s1, $v0
    lw $t0, 0($s0)
    lw $t1, 0($t0)
    move $a0, $t1
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal printhelper
    
    # if ((1 == length((*args).value)))
    li $s1, 1
    lw $t0, 0($s0)
    move $a0, $t0
    jal length
    move $t0, $v0
    bne $s1, $t0, if_end_9
    
    # return 0;
    li $v0, 0
    j printmain_end
    j else_end_7
    if_end_9:
    
    # else
    
    # return printmain(cons((*(*args).value).tail, NULL));
    lw $t0, 0($s0)
    lw $t1, 4($t0)
    move $a0, $t1
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal printmain
    move $t0, $v0
    move $v0, $t0
    j printmain_end
    else_end_7:
    
    printmain_end:
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    lw $s1, 8($sp)
    add $sp, $sp, 12
    jr $ra

printlist:
    # void * printlist(List * args);
    sub $sp, $sp, 8
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    
    move $s0, $a0
    
    # printf("[");
    la $a0, d1
    li $v0, 4
    syscall
    
    # printmain(cons((*args).value, NULL));
    lw $t0, 0($s0)
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal printmain
    
    # return printf("]");
    la $a0, d2
    li $v0, 4
    syscall
    move $v0, $t0
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    add $sp, $sp, 8
    jr $ra

main:
    # void *(void *) main(List * args);
    sub $sp, $sp, 8
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    
    
    # void *() temp_exec_f35 = prime;
    la $s0, prime_2
    
    # int temp_exec_f_argcount11 = 1;
    
    # List * temp_exec_f_args12 = NULL;
    
    # return printlist(cons(filter(cons(apply(cons(temp_exec_f35, cons(temp_exec_f_argcount11, temp_exec_f_args12))), cons(build(cons(1, cons(100, NULL))), NULL))), NULL));
    li $a0, 1
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s0
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal apply
    move $s0, $v0
    li $a0, 100
    li $a1, 0
    jal cons
    move $t0, $v0
    li $a0, 1
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal build
    move $t0, $v0
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $s0
    move $a1, $t0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal filter
    move $t0, $v0
    move $a0, $t0
    li $a1, 0
    jal cons
    move $t0, $v0
    move $a0, $t0
    jal printlist
    move $t0, $v0
    move $v0, $t0
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    add $sp, $sp, 8
    li $v0, 10
    syscall
    jr $ra