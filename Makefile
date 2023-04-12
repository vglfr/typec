.PHONY : asm

cpath = /nix/store/8xk4yl1r3n6kbyn05qhan7nbag7npymx-glibc-2.35-224/lib
gpath = /nix/store/p975i9blgmkjfxpnlvdmm0xvjg573b6l-gcc-12.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.2.0

clean:
	@rm -f \
		asm/temp.s asm/temp.o asm/a.out \
		asm/hello.o asm/hello \
		asm/dprint.o asm/dprint \
		asm/printf.o asm/printf

dprint:
	@nasm -felf64 asm/dprint.s -o asm/dprint.o
	@ld -lc asm/dprint.o -o asm/dprint 
	@./asm/dprint

hello:
	@nasm -felf64 asm/hello.s -o asm/hello.o
	@ld asm/hello.o -o asm/hello 
	@./asm/hello

printf:
	@nasm -felf64 asm/printf.s -o asm/printf.o
	@ld -lc asm/printf.o -o asm/printf 
	@./asm/printf

temp:
	@gcc -S -O0 -masm=intel -fno-asynchronous-unwind-tables asm/temp.c -o asm/temp.s
	@as asm/temp.s -o asm/temp.o
	@ld -lc -o asm/a.out \
		$(gpath)/crtend.o \
		$(gpath)/crtbegin.o \
		$(cpath)/crti.o \
		$(cpath)/crt1.o \
		asm/temp.o \
		$(cpath)/crtn.o
	@./asm/a.out
