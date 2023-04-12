global _start
extern printf

FSTR db "%i", 10, 0

section .text
_start:
        mov         rdi, FSTR
        mov         rsi, -8
        xor         rax, rax
        call        printf

        mov         rax, 60
        xor         rdi, rdi
        syscall
