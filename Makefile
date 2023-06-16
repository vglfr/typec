.PHONY : asm

cpath = /nix/store/8xk4yl1r3n6kbyn05qhan7nbag7npymx-glibc-2.35-224/lib

clean:
	@rm -f \
		asm/temp.s asm/temp.o asm/a.out \
		asm/printf.o asm/printf \
		asm/p9.o asm/p9 \
		asm/hello.o asm/hello \
		asm/fpoint.o asm/fpoint \
		asm/fn2_add.o asm/fn2_exit.o asm/fn2_print_i8.o asm/fn2_start.o asm/fn2 \
		asm/fn.o asm/fn \
		asm/dprint.o asm/dprint

dprint:
	@nasm -felf64 asm/dprint.s -o asm/dprint.o
	@ld asm/dprint.o -o asm/dprint 
	@./asm/dprint

fn:
	@nasm -felf64 asm/fn.s -o asm/fn.o
	@ld asm/fn.o -o asm/fn 
	@./asm/fn

fn2:
	@nasm -felf64 asm/fn2_add.s -o asm/fn2_add.o
	@nasm -felf64 asm/fn2_exit.s -o asm/fn2_exit.o
	@nasm -felf64 asm/fn2_print_i8.s -o asm/fn2_print_i8.o
	@nasm -felf64 asm/fn2_start.s -o asm/fn2_start.o
	@ld asm/fn2_add.o asm/fn2_exit.o asm/fn2_print_i8.o asm/fn2_start.o -o asm/fn2
	@./asm/fn2

fpoint:
	@nasm -felf64 asm/fpoint.s -o asm/fpoint.o
	@gcc -z noexecstack asm/fpoint.o -o asm/fpoint
	@./asm/fpoint

hello:
	@nasm -felf64 asm/hello.s -o asm/hello.o
	@ld asm/hello.o -o asm/hello 
	@./asm/hello

p9:
	@nasm -felf64 asm/p9.s -o asm/p9.o
	@gcc -z noexecstack asm/p9.o -o asm/p9
	@./asm/p9

printf:
	@nasm -felf64 asm/printf.s -o asm/printf.o
	@gcc -z noexecstack asm/printf.o -o asm/printf
	@./asm/printf

temp:
	@gcc -S -O0 -masm=intel -fno-asynchronous-unwind-tables asm/temp.c -o asm/temp.s
	@as asm/temp.s -o asm/temp.o
	@ld -lc -o asm/a.out \
		$(cpath)/crti.o \
		$(cpath)/crt1.o \
		asm/temp.o \
		$(cpath)/crtn.o
	@./asm/a.out
