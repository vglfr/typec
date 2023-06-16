global _print_i8

section .bss
        RES:        resb 1

section .text
_print_i8:
        push        rbp
        mov         rbp, rsp

        mov         rax, [rbp+16]
        mov         [RES], rax

        mov         rax, 1
        mov         rdi, 1
        mov         rsi, RES
        mov         rdx, 1

        syscall
        pop         rbp
        ret
