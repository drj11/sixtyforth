BITS 64

sys_read equ 0
sys_write equ 1

SECTION .bss
buf RESB 8192
buflen equ $-buf

SECTION .data

section .text
global _start
_start:

mov rdi, 0 ; stdin
mov rsi, buf
mov rdx, buflen
mov rax, sys_read
syscall

; write out the same number of bytes as read
mov rdx, rax
mov rdi, 1 ; stdout
mov rsi, buf
mov rax, sys_write
syscall

mov rdi, 0
mov rax, 60
syscall
