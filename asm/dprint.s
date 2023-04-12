global _start
extern printf

FSTR db "%i", 10, 0

section .text
_start:
        ; 6 / 1 = 6
        ; mov         rax, 6
        ; mov         rbx, 1
        ; idiv        rbx

        ; 6 * 2 = 12
        ; mov         rbx, 2
        ; imul        rbx

        ; 4 - 12 = -8
        mov         rbx, 4
        sub         rbx, rax

        ; printf("%i", -8)
        mov         rdi, FSTR
        mov         rsi, rbx
        xor         rax, rax
        call        printf

        mov         rax, 60
        xor         rdi, rdi
        syscall
