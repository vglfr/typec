global _exit

section .text
_exit:
        push        rbp
        mov         rbp, rsp

        mov         rax, 60
        mov         rdi, [rbp+16]

        syscall
        pop         rbp
        ret
