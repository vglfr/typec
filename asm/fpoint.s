global main
extern printf

section .data
        FST: db "%.2f", 10, 0
        C1: dq 2.0
        C2: dq 6.0
        C3: dq 1.0
        C4: dq 4.0
        C5: dq 3.0

section .bss
        RES: resq 1

section .text
main:
        ; 2.0 * 6.0  = 12.0   [12.0]
        fld         qword [C1]
        fmul        qword [C2]
        ; [-1] / 1.0 = 12.0   [12.0]
        fdiv        qword [C3]
        ; 4.0 - [-1] = -8.0   [-8.0]
        fld         qword [C4]
        fxch
        fsubp
        ; [-1] * 3.0 = -24.0  [-24.0]
        fmul        qword [C5]
        ; 2.0 - 3.0  = -1.0   [-1.0, -24.0]
        fld         qword [C1]
        fsub        qword [C5]
        ; [-1] * 3.0 = -3.0   [-3.0, -24.0]
        fmul        qword [C5]
        ; 1.0 + 2.0  = 3.0    [3.0, -3.0, -24.0]
        fld         qword [C3]
        fadd        qword [C1]
        ; [-2] / [-1] = -1.0  [-1.0, -24.0]
        fdivp
        ; 1.0 / [-1]  = -1.0  [-1.0, -24.0]
        fld         qword [C3]
        fxch
        fdivp
        ; [-6] + [-1] = -25.0 [-25.0]
        faddp
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
