org 100h
mov ah, 09
mov dx, askFirstHex
int 21h

mov ah, 09
mov dx, nextstr
int 21h

mov ah, 0ah
mov dx, GetHex1
int 21h

mov ah, 09
mov dx, nextstr
int 21h

mov ah, 09
mov dx, askSecHex
int 21h

mov ah, 09
mov dx, nextstr
int 21h

mov ah, 0aH
mov dx, GetHex2
int 21h

mov bx, GetHex1
inc bx

push bx
call GettingValues
mov [firstNum], di
mov bx, GetHex2
inc bx
push bx
call GettingValues
push di
mov bx, [firstNum]
push bx
call multiply
push bx
call intoStr
mov ah, 09
mov dx, nextstr
int 21h

mov ah, 09
mov dx, outputstr
int 21h
mov di,10
push di
mov bx, [remainder]
push bx
call intoStr

mov ah, 09
mov dx, nextstr
int 21h

mov ah, 09
mov dx, remainstr
int 21h

mov ah, 09
mov dx, outputstr
int 21h
mov ah, 08
int 21h
add sp,10

ret
GettingValues:
push bp
mov bp, sp
mov bx, [bp+4]
xor al,al
mov cx,5
add bx,cx
xor dx,dx
loopstart:
cmp byte[bx], 30h
jl endloop
mov al, byte[bx]
cbw
sub ax, 30h
push ax
inc dl
endloop:
dec bx
loop loopstart
mov al,dl
cbw
mov cx, ax
dec cx
push ax
mov ax, 1
mov di, 10
cmp cx, 0
jng ender
tenPower:
mul di
loop tenPower
mov dx, ax
pop cx
xor di, di
PopingValues:
mov dx, ax
pop ax
push dx
mul dx
pop dx
add di, ax
mov ax, dx
xor dx, dx
div word[multi]
loop PopingValues
ender:
pop bp
ret  2

multiply:
push bp
mov bp, sp
mov bx, [bp+6]
mov di, [bp+4]
mov ax, di
xor dx, dx
div bx
mov bx,ax
mov [remainder], dx
pop bp
ret


intoStr:
push bp
mov bp, sp
mov bx, [bp+4]
xor dx,dx
xor cx,cx
mov  ax,bx
loopster:
div word [multi]
inc cx
push dx
xor dx, dx
cmp ax, 0
jg loopster
mov di,  outputstr
loopOutput:
pop ax
add ax, 30h
stosb
loop loopOutput
mov al, '$'
stosb
pop bp
ret



askFirstHex: db 'Give me the first number$'
askSecHex: db 'Give me the second number$'
remainstr: db 'remainder is $'
nextstr: db 10,13, '$'
GetHex1: db 6,0,6 dup(0)
GetHex2: db 6,0,6 dup(0)
multi: dw 10
firstNum: dw ?
remainder: dw  ?
outputstr: db 6,0,6 dup(?)