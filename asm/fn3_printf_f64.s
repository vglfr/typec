global _printf_f64
extern printf

section .data
        FST:        db "%i", 10, 0
        ; FST:        db "%.2f", 10, 0
        ; C1:         dq 3.3333333

section .text
_printf_f64:
        push        rbp
        mov         rbp, rsp

        mov         rdi, FST
        mov         rsi, [rbp+16]
        xor         rax, rax
        ; movsd       xmm0, qword [C1]
        ; mov         rax, 1
        call        printf

        pop         rbp
        xor         rax, rax
        ret
