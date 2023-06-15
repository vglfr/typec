global main
extern printf

section .data
        FST:        db "%i", 10, 0

section .bss
        RES:        resq 1

section .text
boilerplate:
        push        rbp

        mov         rdi, FST
        mov         rsi, [RES]
        call        printf
        pop         rbp

        xor         rax, rax
        ret

        ; exit
        mov         rax, 60
        xor         rdi, rdi
        syscall

f:
        ; mov         rbx, []
        ; mov         rax, []
        mov         rax, 2
        add         rax, 1
        ; add         rax, rbx
        mov         [RES], rax
        ; xor         rax, rax
        ; ret

main:
        ; push        2
        ; push        4

        push        rbp
        call        f
        pop         rbp
        ret
        ; mov         qword [RES], -6
        jmp         boilerplate
