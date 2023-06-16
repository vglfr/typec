global _start

extern _add
extern _exit
extern _print_i8

section .text
_start:
        push        24
        push        24
        call        _add         ; 48 = '0'
        add         rsp, 16

        push        rax
        call        _print_i8
        add         rsp, 8

        push        50           ; 50 = '2'
        push        48           ; 48 = '0'
        call        _add         ; 98 = 'b'
        add         rsp, 16

        push        rax
        call        _print_i8
        add         rsp, 8

        push        52
        call        _print_i8
        add         rsp, 8

        push        50
        call        _print_i8
        add         rsp, 8

        push        0
        call        _exit
        add         rsp, 8
