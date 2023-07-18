global main

extern _printf_f64

section .data
        C1:         dq 1.1111111
        C2:         dq 2.2222222

section .bss
        RES         resq 1

section .text
main:
        ; C1 + C2
        fld         qword [C1]
        fadd        qword [C2]

        fstp        qword [RES]
        mov         rax, [RES]

        push        rax
        call        _printf_f64
        add         rsp, 8
