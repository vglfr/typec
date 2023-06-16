global _exit

section .text
_exit:
        mov         rax, 60
        xor         rdi, rdi
        syscall
        ret
