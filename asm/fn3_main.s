global main

extern _exit
extern _fadd
extern _printf_f64

section .data
        C1:         dq 1.1111111
        C2:         dq 2.2222222

section .text
main:
        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        push        qword [C2]
        call        _fadd
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        push        qword [C2]
        push        qword [C2]
        call        _fadd
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        qword [C1]
        call        _printf_f64
        add         rsp, 8

        push        0
        call        _exit
        add         rsp, 8
