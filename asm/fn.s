global _start

section .bss
        RES:        resb 1

section .text
; _copy:
;         mov         rax, [rsp+16]
;         mov         [RES], rax

_print:
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, RES
        mov         rdx, 1
        syscall
        ret

_exit:
        mov         rax, 60
        xor         rdi, rdi
        syscall
        ret

_add:
        mov         rax, [rsp+16]
        mov         rbx, [rsp+8]
        add         rax, rbx
        mov         [RES], rax
        ret
        
_start:
        ; mov         rax, 50
        ; add         rax, 2
        ; add         rax, rbx
        ; mov         [RES], rax
        ; mov         byte [RES], 52 ; 4

        ; push        50 ; 50 = '2'
        ; push        48 ; 48 = '0'
        ; call        _copy

        push        50   ; 50 = '2'
        push        48   ; 48 = '0'
        call        _add ; 98 = 'b'
        call        _print
        call        _exit
