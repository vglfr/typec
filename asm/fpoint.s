global main
extern printf

section .data
        FST: db "%.2f", 10, 0
        OP1: dq 1.0
        OP2: dq 2.0

section .bss
        RES: resq 1

section .text
main:
        ; 1 + 2
        fld         qword [OP1]
        fld         qword [OP2]
        faddp 
        fstp        qword [RES]
        ; printf
        push        rbp
        movsd       xmm0, qword [RES]
        mov         rdi, FST
        mov         rax, 1
        call        printf
        pop         rbp
        xor         rax, rax
        ret
        ; exit
        mov         rax, 60
        xor         rdi, rdi
        syscall
