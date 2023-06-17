global main

extern _exit
extern _fadd
extern _printf_f64

section .text
main:
        push        24
        push        24
        call        _fadd        ; 48 = '0'
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        50           ; 50 = '2'
        push        48           ; 48 = '0'
        call        _fadd        ; 98 = 'b'
        add         rsp, 16

        push        rax
        call        _printf_f64
        add         rsp, 8

        push        52
        call        _printf_f64
        add         rsp, 8

        push        50
        call        _printf_f64
        add         rsp, 8

        push        0
        call        _exit
        add         rsp, 8
