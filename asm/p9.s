global main
extern printf

section .data
        FST:        db "%.2f", 10, 0
        C2:         dq 1.0
        C1:         dq 2.0
        C0:         dq 3.0
        C3:         dq 4.0
        C4:         dq 5.0

section .bss
        RES:        resq 1
        x:          resq 1
        a:          resq 1
        b:          resq 1
        y:          resq 1
        d:          resq 1
        c:          resq 1
        z:          resq 1

section .text
main:
        ; C4
        fld         qword [C4]
        ; x
        fstp        qword [x]
        ; x - C0
        fld         qword [x]
        fsub        qword [C0]
        ; a
        fstp        qword [a]
        ; C4 * a
        fld         qword [C4]
        fmul        qword [a]
        ; [-1] - a
        fsub        qword [a]
        ; b
        fstp        qword [b]
        ; x * C1
        fld         qword [x]
        fmul        qword [C1]
        ; y
        fstp        qword [y]
        ; a                   ;;;
        fld         qword [a] ;;;
        ; d
        fstp        qword [d]
        ; d + C3
        fld         qword [d]
        fadd        qword [C3]
        ; [-1] - x
        fsub        qword [x]
        ; c
        fstp        qword [c]
        ; C3 * y
        fld         qword [C3]
        fmul        qword [y]
        ; x + [-1]
        fld         qword [x]
        fxch
        faddp
        ; [-1] + b
        fadd        qword [b]
        ; z
        fstp        qword [z]
        ; x + C2
        fld         qword [x]
        fadd        qword [C2]
        ; y / a
        fld         qword [y]
        fdiv        qword [a]
        ; [-2] - [-1]
        fsubp
        ; C1 * z
        fld         qword [C1]
        fmul        qword [z]
        ; [-2] - [-1]
        fsubp
        ; b * C0
        fld         qword [b]
        fmul        qword [C0]
        ; [-1] / c
        fdiv        qword [c]
        ; [-3] + [-1]
        faddp
        ; fstp
        fstp        qword [RES]
        ; printf
        push        rbp
        movsd       xmm0, qword [RES]
        mov         rdi, FST
        mov         rax, 1
        call        printf
        pop         rbp
        xor         rax, rax
        ret
        ; exit
        mov         rax, 60
        xor         rdi, rdi
        syscall