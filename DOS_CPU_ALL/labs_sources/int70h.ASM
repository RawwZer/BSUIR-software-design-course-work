org 100h
push ds
push 0
pop ds
mov word[70h*4], Int70h
mov word[70h*4+2],cs
pop ds
mov cl,1
int 70h

ret


Int70h:
readCycle:
mov ah, 01h
int 21h
cmp al, 'a'
jne readCycle
entryPoint:
    mov ah, 0fh
    int 10h
    mov [bOldMode], al
    mov [boldPage], bh
    mov ax, $0013
    int 10h
    push $A000
    pop es
    mov cx, 200
    xor di, di
    xor ax,ax
    xor bx,bx
    mov bx, 64000
    mov ax,20h
loopAgain:
    push cx
    mov cx, 320
start:
   mov ax,cx
    and bx, 0FFh
    stosb
    loop  start

    pop cx
    inc ax
    and ax, 0FFh
    loop loopAgain

    mov ax, $0c08
    int 21h
    test al, al
    jnz  @F


    mov ah, 08
    int 21h
@@:
   mov ah, 0
   mov al,[bOldMode]
   int 10h
   mov ah, 05
   mov al, [boldPage]
   int 10h

iret


bOldMode: db ?
boldPage: db ?