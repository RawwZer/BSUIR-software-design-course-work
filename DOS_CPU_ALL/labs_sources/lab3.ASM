     org 100h
     mov ah, 09     ; giving the starting message
     mov dx, start
     int 21h

     mov ah, 09
     mov dx, strnext  ; going onto next line
     int 21h

     mov ah, 0ah
     mov dx, array     ; reading the array
     int 21h

     mov ah, 09
     mov dx, strnext  ; going onto next line
     int 21h
     mov di, array
     add di, 2
     mov bh, [di]
     inc di
     mov ah, [di]
     mov si, di
     mov cx, 4  ; we are already here

loopStart:
     cmp ah, '0'
     jge  fine1
     jmp wrong
fine1:
     cmp ah, '9'
     jbe fine2    ; checking whether the symbol is a number
     jmp wrong
fine2:
     cmp ah, [si]
     jnb nochange  ; change the address of the smallest
     mov si, di

nochange:
    mov al, ah
    inc di
    mov ah, [di]
loop loopStart


    mov ax,array+1
  ;  add ax,word[array+1]
    sub di, si
    mov cx, di
    mov ax, 0

loopStart2:
    add ah, byte[si]
    sub ah, '0'
    inc si
loop loopStart2
    add ah, '0'
    mov [sum], ah
    mov dx,word[ sum]
    mov ah, 02
    int 21h
    mov ah, 08
    int 21h
 ret
 wrongstring:
 db 'incorrect input$'
 wrong:
 mov dx, wrongstring
 mov ah, 09
 int 21h
 mov ah, 08
 int 21h
 ret


start : db 'hello, gimme the array$'
strnext: db 10, 13, '$'
array: db  06h, 0, 5 dup(?)
sum: db 1, 0, 1 dup(?), '$'