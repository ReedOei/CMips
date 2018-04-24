.text
f:
    # int f(int x);

    # return ((x * x) + x);
    mul $t0, $a0, $a0
    add $t1, $t0, $a0
    move $v0, $t1
    jr $ra

main:
    # int main();
    sub $sp, $sp, 4
    sw $ra, 0($sp)


    # printf("%d", f(3));
    li $a0, 3
    jal f
    move $t0, $v0
    move $a0, $t0
    li $v0, 1
    syscall

    # return 0;
    li $v0, 0
    lw $ra, 0($sp)
    add $sp, $sp, 4
    li $v0, 10
    syscall
    jr $ra
