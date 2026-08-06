BITS 64
default rel
; Convert address of Code Field to address of Link Field
%define CtoL(a) DQ (a)-24
; Or (using «|») into Length Field to create IMMEDIATE word.
%define Immediate (2<<32)

extern GetStdHandle
extern WriteFile
extern VirtualAlloc
extern DebugBreak
extern GetLastError
extern OutputDebugStringA
extern ExitProcess
extern ReadFile
extern GetConsoleWindow
extern SetConsoleMode
extern GetConsoleMode

extern pushrax
extern next

global _get_stdout
global _get_stdin
global getch_buff
global win_writefile
global trace
global win_init

 section .rodata
msg: db "hello", 10, 0
msg_len : dq 6
cr : db 10, 0
trace_bad:db "oops", 10, 0
trace_good db "ok",10,0


section .data
console_mode: dw 0
getch_buff: dq 0
read_len: dq 0
console_handle: dq 0

section .text

trace:
;ret
    push rax
    call _get_stdout
    mov rcx, rax
    pop rdx
    call win_writefile
    ret

tracex: 

    ; write to stdout, rax = message, r8 = length
    push rdx
    push rax
    mov  rcx, 0fffffff5h ; stdout
    sub  rsp, 32
    ; get stdout handle into rax
    call GetStdHandle
    add  rsp,32

    mov rcx, rax
    pop rax
    mov rdx, rax ; msg
    mov r9, 0
    push qword 0
    sub  rsp, 32
    call    WriteFile
    add rsp,40
    pop rdx
  
    ret


win_readfile:
; rcx = handle
; rdx = buffer
; r8 = length to read
;
; rax 0-> OK, !0 = GetLastError
; r8 read length
 
    mov r9,read_len
    push 0
    sub rsp, 32
    call ReadFile
    test rax,rax
    jnz .read_ok
    call GetLastError
    add rsp,40
    ret
.read_ok:
    mov r8,[ read_len]
    xor rax,rax
    add rsp, 40
    ret

win_writefile:
; rcx = handle
; rdx = buffer
; r8 = length to write
;
; rax 0-> OK, !0 = GetLastError


    mov r9, 0 ; write count
    push qword 0 ; flags
    sub  rsp, 32
    call WriteFile
    add rsp,40
    ;pop rdx
  
    test rax,rax
    jnz .write_ok
    ;call DebugBreak
    sub rsp,32
    call GetLastError
    add rsp,32
    ret
.write_ok:
    mov r8,[ read_len]
    xor rax,rax
   ; add rsp, 40
    ret

%define STDOUT_HANDLE 0fffffff5h
%define STDIN_HANDLE -10
%define STDERR_HANDLE 0fffffff3h

_get_stdout:
    mov rcx, STDOUT_HANDLE
    jmp _get_std_handle
_get_stdin:
    mov rcx, STDIN_HANDLE
    jmp _get_std_handle
_get_stderr:
    mov rcx, STDERR_HANDLE
_get_std_handle:
    sub rsp, 32
    call GetStdHandle
    add rsp, 32
    ret


_get_console_window:
    sub rsp,32
    call GetConsoleWindow
    add rsp, 32

_set_console_mode:
    sub rsp, 32
    call SetConsoleMode
    add rsp, 32
    ret

_get_console_mode:
    sub rsp, 32
    call GetConsoleMode
    mov [console_mode], ax
    add rsp, 32
    ret

win_set_terminal:
;call DebugBreak
    call _get_stdin
    mov rcx, rax
    mov rdx, console_mode
    call _get_console_mode
    mov edx, [console_mode] 
    and edx, 0xfffffff9
    call _set_console_mode
    ret

win_init:
    call win_set_terminal
    ret