global _add

section .text
_add:
        push        rbp
        mov         rbp, rsp

        mov         rax, [rbp+24]
        mov         rbx, [rbp+16]
        add         rax, rbx

        pop         rbp
        ret
