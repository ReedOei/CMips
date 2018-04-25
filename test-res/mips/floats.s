.text
g:
    # float g(float x,float y);
    move $t0, $a0
    move $t1, $a1
    
    # return (((x * y) + y) - x);
    mtc1 $t0, $f0
    mtc1 $t1, $f1
    mul.s $f2, $f0, $f1
    mfc1 $t2, $f2
    mtc1 $t2, $f0
    mtc1 $t1, $f1
    add.s $f2, $f0, $f1
    mfc1 $t1, $f2
    mtc1 $t1, $f0
    mtc1 $t0, $f1
    sub.s $f2, $f0, $f1
    mfc1 $v0, $f2
    jr $ra

f:
    # float f(float x);
    sub $sp, $sp, 8
    sw $ra, 0($sp)
    sw $s0, 4($sp)
    
    move $t0, $a0
    
    # return ((x * x) * g((x + 4.5), (x - 6.5)));
    mtc1 $t0, $f0
    mtc1 $t0, $f1
    mul.s $f2, $f0, $f1
    mfc1 $s0, $f2
    li.s $f0, 4.5
    mfc1 $t1, $f0
    mtc1 $t0, $f0
    mtc1 $t1, $f1
    add.s $f2, $f0, $f1
    mfc1 $a0, $f2
    li.s $f0, 6.5
    mfc1 $t1, $f0
    mtc1 $t0, $f0
    mtc1 $t1, $f1
    sub.s $f2, $f0, $f1
    mfc1 $a1, $f2
    jal g
    mtc1 $s0, $f0
    mtc1 $v0, $f1
    mul.s $f2, $f0, $f1
    mfc1 $v0, $f2
    lw $ra, 0($sp)
    lw $s0, 4($sp)
    add $sp, $sp, 8
    jr $ra

main:
    # int main();
    sub $sp, $sp, 4
    sw $ra, 0($sp)
    
    
    # printf("%f", f(9.4));
    li.s $f0, 9.4
    mfc1 $a0, $f0
    jal f
    mtc1 $v0, $f12
    li $v0, 2
    syscall
    
    # return 0;
    li $v0, 0
    lw $ra, 0($sp)
    add $sp, $sp, 4
    li $v0, 10
    syscall
    jr $ra