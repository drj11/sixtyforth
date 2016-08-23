BITS 64

sys_write equ 1

SECTION .data
arg db "For64", 10
arglen equ $-arg

section .text
global _start
_start:
mov rdi, 1 ; stdout
mov rsi, arg
mov rdx, arglen
mov rax, sys_write
syscall
mov rdi, 0
mov rax, 60
syscall
