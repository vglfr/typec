global main
extern printf

FSTR db "%i", 10, 0

section .text
main:
        push        rbp

        mov         rdi, FSTR
        mov         rsi, -8
        xor         rax, rax
        call        printf

        pop         rbp

        xor         rax, rax
        ret

        mov         rax, 60
        xor         rdi, rdi
        syscall
