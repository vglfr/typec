global _start
; extern printf

; FSTR db "%i", 10, 0

section .text
_start:
        ; 6 / 1 = 6
        mov         rax, 6
        mov         rbx, 1
        cqo
        idiv        rbx

        ; 6 * 2 = 12
        mov         rbx, 2
        imul        rbx

        ; 14 - 12 = 2
        mov         rbx, 14
        sub         rbx, rax

        ; 2 * 3 = 6
        mov         rax, 3
        imul        rbx

        push        rax

        ; 2 - 3 = -1
        mov         rax, 2
        sub         rax, 3

        push        rax

        ; 1 + 2 = 3
        mov         rbx, 1
        add         rbx, 2

        ; 3 / 3 = 1
        mov         rax, 3
        cqo
        idiv        rbx

        ; 1 * -1 = -1
        pop         rbx
        imul        rbx

        ; 1 / -1 = -1
        mov         rbx, rax
        mov         rax, 1
        cqo
        idiv        rbx

        ; 6 + -1 = 5
        pop         rbx
        add         rax, rbx

        ; printf("%i", -8)
        ; mov         rdi, FSTR
        ; mov         rsi, rbx
        ; xor         rax, rax
        ; call        printf

        mov         rdi, rax
        mov         rax, 60
        syscall
