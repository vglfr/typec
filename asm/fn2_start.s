global _start
extern _exit

section .bss
        RES:        resb 1

section .text
_print:
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, RES
        mov         rdx, 1
        syscall
        ret

_add:
        mov         rax, [rsp+16]
        mov         rbx, [rsp+8]
        add         rax, rbx
        mov         [RES], rax
        ret
        
_start:
        push        50           ; 50 = '2'
        push        48           ; 48 = '0'
        call        _add         ; 98 = 'b'
        call        _print
        call        _exit
