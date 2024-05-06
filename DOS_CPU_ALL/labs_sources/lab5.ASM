   org 100h

GettingValue:
        mov ah, $09
        mov dx, Getting
        int 21h

        mov ah, 0ah
        mov dx, Value
        int 21h

        mov ah, $09
        mov dx, Next
        int 21h

        mov ah, 0ah
        mov dx, Symbol
        int 21h

        mov ah, $09
        mov dx, Next
        int 21h

Finding:
        ; For loop
        mov cl, [Value + 1]
        ; Moving to ax Symbol
        mov dl, byte[Symbol+2]
        cmp dl, 'A'
        jb .Nothing
        cmp dl, 'Z'
        jb .HighCase
        cmp dl, 'a'
        jb .Nothing
        cmp dl, 'z'
        jb .LowCase

.Nothing:
        xor dl, dl
        jmp lop
.HighCase:
        mov dl, 20h
        jmp lop
.LowCase:
        mov dl, -20h

lop:
        ; Taking an element
        mov bx, 2
        add bl, byte[Value+1]
        sub bl, cl
        mov al, byte[Symbol+2]

SameElements:
        ; Moving to es:di address of element
        mov di, Value
        add di, bx

        ; Comparing the element and symbol
        scasb
        jnz looplop
        add byte[Value+bx], dl

looplop:
        loop lop


Output:
        mov ah, $09
        mov dx, Outputting
        int 21h

        mov ah, $09
        mov dx, Next
        int 21h

        mov ah, $09
        mov dx, Value + 2
        add bx, Value+1
        mov al, '$'
        mov byte[bx], al
        int 21h

        mov ah, $08
        int 21h

        ret


Getting db 'Enter the array:  $'
Next db 10, 13, '$'
Value db 11, 0, 11 dup(?), '$'
Outputting db 'The massiv with another letter case: $'
Symbol db 2,0, ?