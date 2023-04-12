section .text
global  _start

        hello   db "Hello, world", 0x0A
        hello_l equ $ - hello

_start:
        mov     rax, 1
        mov     rdi, 1
        mov     rsi, hello
        mov     rdx, hello_l
        syscall

        mov     rax, 60
        xor     rdi, rdi
        syscall
