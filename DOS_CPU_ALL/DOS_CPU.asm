format PE GUI 4.0
entry WinMain
LINES_03 = 25
POS_03 = 80
fontMemSize = 4096
colorsFile = 1024     
fileSize = 10000h
IDT_INSERT = 235
bmSize = 35000
include "includes\win32w.inc"


macro catchAddr{
                 pop ecx
                 push ebp
                 mov ebp, [retValPointer] 
                 mov dword[retVal+ebp],ecx
                 add dword[retValPointer],4
                 pop ebp 
}

macro Pop_Stack{ 
         xor eax,eax
     xor ebx,ebx
     mov bx,word[SegmentRegs+4]
     shl ebx,4
     mov ax, word[GeneralRegs+8]
     add ebx,eax
     add word[GeneralRegs+8],2
}

macro PushSEG val{
         sub word[GeneralRegs+8],2
         push esi
         xor esi,esi
         xor eax,eax
         mov si, word[SegmentRegs+4]
         shl esi, 4
         mov ax,word[GeneralRegs+8]
         add esi,eax
         mov bx, [SegmentRegs+val]                  
         mov word[esi+RAM],bx
         pop esi
         inc esi
}

macro PopSEG val{
         push esi
         xor esi,esi
         mov si, word[SegmentRegs+4]
         shl esi, 4
         add si,word[GeneralRegs+8]
         xor ebx,ebx
         mov bx, [ESI+RAM]
         mov word[SegmentRegs+val], bx
         pop ESI
         add word[GeneralRegs+8],2
         inc ESI
}
        
macro Push_Stack{
         sub word[GeneralRegs+8],2
     xor ebx,ebx
     xor eax,eax
     mov bx,word[SegmentRegs+4]
     shl ebx,4
     mov ax, word[GeneralRegs+8]
     add ebx,eax
}


macro retAddr{
        push ebp
        mov ebp, [retValPointer] 
        mov ebx, [retVal+ebp-4]
        pop ebp
        push ebx
        sub dword[retValPointer],4
        xor ebx,ebx
}

macro No_REG Num{
        xor edx,edx
        mov dl, [esi+1]
        mov dh,dl
    and dl, 000111b
    and dh, 11000000b
    shr dh, 3
    add dl,dh
    xor dh,dh
    shl edx,2
    mov ebx,edx
    add edx,MODR_M_VALUES
    mov edx,[edx]
    push Num
    call edx
    pop edi
    cmp edi,1
    jne .noadd
    mov edi,RAM
    push eax
    xor eax,eax
    mov ax, [SegmentRegs+2]
    shl eax,4
    add edi,eax
    pop eax
    .noadd:
    pop ebx
    add ebx,edi
    add sp,8
    mov edi,ebx
}

macro No_REGW{
        xor edx,edx
        mov dl, [esi+1]
        mov dh,dl
    and dl, 000111b
    and dh, 11000000b
    shr dh, 3
    add dl,dh
    xor dh,dh
    shl edx,2
    mov ebx,edx
    add edx,MODR_M_VALUES
    mov edx,[edx]
    push 2
    call edx
    pop edi
    cmp edi,1
    jne .noadd
    mov edi,RAM
    push eax
    xor eax,eax
    mov ax, [SegmentRegs+2]
    shl eax,4
    add edi,eax
    pop eax
    .noadd:
    pop ebx
    add ebx,edi
    add esp,8
}       
        

; ---S E C T I O N  O F  C O D E---
.code:

       ; ********************
        ; *   INTERAPTIONS   *
        ; ********************
        int08h:
                ret
        int10h:
                    push ebx
                push edx
                
                mov dl, [GeneralRegs+1] ; --getting value of emulated AH 
                
                cmp dl, 0fh
                je .h0f
                cmp dl, 00h
                jnz int21h.end_INT
 
                .h00:
                                                mov dl, [GeneralRegs]
                                                cmp dl, 3
                                                jne .Mode13
                                                mov [currVideoMode], btm_03h
                                                jmp int21h.end_INT
                                                .Mode13:
                                                        mov [currVideoMode], btm_13h
                        jmp int21h.end_INT
                .h0f:     
                                                mov edx, [currVideoMode]
                                                mov dl, byte[edx]
                                                mov byte[GeneralRegs], dl
                                                mov byte[GeneralRegs+2*3+1], 0
                                                mov edx, [currVideoMode + 1 + BITMAPINFOHEADER.biWidth]
                                                mov byte[GeneralRegs+1], dl                                     
                        jmp int21h.end_INT
        int16h:
                                jmp int21h.end_INT
        int20h:
                mov [isProgram], 0
                mov eax, 00000400h          ; --loading the interrupt vector table with specific value
                mov edi, RAM
                mov ecx, 255
                rep stosd 
                mov dword[RAM+400h], 9Bh
                mov byte[RAM+419h], 0
                mov word[RAM+41Ah], 41Eh
                mov word[RAM+41Ch], 41Eh
                mov [repaintFull], 1
           ;     mov esp, [prevESP]
                invoke InvalidateRect, dword[hWnd], 0, 0
                call newLine
                ret
        int21h:
                push ebx
                push edx
 
                mov dl, [GeneralRegs+1] ; --getting value of emulated AH 
 
                cmp dl, 2ch
                je .h2c
                cmp dl, 01h
                je .h01
                cmp dl, 02h
                je .h02
                cmp dl, 08h
                je .h08
                cmp dl, 09h
                je .h09
                cmp dl, 0ah
                je .h0a
                cmp dl, 0ch
                je .h0c
                cmp dl, 2ah
                je .h2a
                cmp dl, 00h
                jne .end_INT
 
                .h00:           
 
                        jmp .end_INT
                .h01:           ; reads (waits for) a char from StInput; echoes to the StOutput     <<EXTENDED KEYSTROKES>>
                        mov byte[GeneralRegs], 0
                        mov dx, [RAM+41Ah]
                        cmp dx, [RAM+41Ch]
                        ja .read
                        .waiting: ; --waiting while symbol isn't written into buffer
                                mov byte[isWaitingInput], 1
                                jmp .end_INT
                        .read:
                                xor eax, eax
                                call readSymbol
                                mov [GeneralRegs], al
                                cmp byte[GeneralRegs+1], 08h
                                jz .end_h08
                        .echoIt:
                                call newLine
                                mov byte[currPos], 0
                                stdcall DrawSymb, eax, 0, 8
                        jmp .end_INT
 
                .h02:           ; sends the char to the StOutput; handles backspace
                        xor edx, edx
                        mov dl, [GeneralRegs+4]
                        cmp dl, 8             
                        jne .noBackspace
                        stdcall prevPosition
                        jmp .end_INT
                        .noBackspace:
                                stdcall DrawSymb, edx, 0, 8
                        jmp .end_INT
 
                .h08:           ; reads (waits for) a char from StInput; return it in AL             <<EXTENDED KEYSTROKES>>
                        jmp .h01
                        .end_h08:
                                stdcall DrawSymb, eax, 0, 8
                                jmp .end_INT
 
                .h09:
                        xor eax, eax    ; --calculating address of DS:DX
                        mov ax, word[SegmentRegs+6]
                        shl eax, 4
                        xor edx, edx
                        mov dx, word[GeneralRegs+4]
                        add eax, edx
                        add eax, RAM
                        stdcall outputString, eax, 0
                        cmp [currPos], 0
                        je .end_INT
                        mov ax, [currPos]
                        sub ax, 2
                        mov [startPos], ax
                jmp .end_INT
 
                .h0a:
                        xor eax, eax    ; --calculating address of DS:DX
                        mov ax, word[SegmentRegs+6]
                        shl eax, 4
                        xor edx,edx
                        mov dx, word[GeneralRegs+4]
                        add eax, edx
                        add eax, RAM
                        mov edx, eax
 
                        .readSymb:
                                call readSymbol
                                cmp eax, 0
                                je .wait
                                stdcall DrawSymb, eax, 0, 8
                                movzx ebx, byte[edx+1]
                                mov byte[edx+ebx+2], al
                                inc byte[edx+1]
                                mov bl, byte[edx+1]
                                cmp bl, byte[edx]
                                je .readyString
                                jmp .readSymb
 
                        .wait:
                              ;  inc edx
                               ; mov byte[edx], bl
                                mov [isWaitingInput], 1
                                jmp .end_INT    
 
                        .readyString:
                                movzx ebx, byte[edx+1]
                                mov byte[edx+ebx+2], 13
                        jmp .end_INT
 
                .h0c:
                        jmp .end_INT
                .h2a:
                        jmp .end_INT
                .h2c:
                ; <<43H CREATE>>
                .end_INT:      
                        pop edx
                        pop ebx
                        jmp calculateNewAdress
        int22h:
                ret
        int23h:
                ret 
        int24h: 
                ret 
        int25h:
                ret
        int43h:
                ret  
 
        calculateNewAdress:
                push eax 
               ; push esi
                push edx
                movzx eax, word[SegmentRegs+2*2] ; SS
                        shl eax, 4
                        movzx esi, word[GeneralRegs+4*2]; SP
                        add eax, RAM  ;SS*16+SP+RAM
                        add eax, esi
 
 
                        movzx esi, word[eax+2]
                        shl esi, 4
                        movzx edx, word[eax+0]
                        add esi, edx
                        add esi, RAM ;esi
                        mov [IPpointer], esi
 
                        mov bx, word[eax+4]
                        mov [FLAGSr], bx
 
                        add word[GeneralRegs+4*2], 6
 
                pop edx
               ; pop esi
                pop edx
                ret   
 
 
 
        ; ********************
        ; *     WINMAIN      *
        ; ********************
        WinMain:
                ;PREPARING OPERATING SYSTEM'S MEMORY
                mov eax, 00000400h          ; --loading the interrupt vector table with specific value
                mov edi, RAM
                mov ecx, 255
                rep stosd 
                mov dword[RAM+400h], 9Bh
                mov byte[RAM+419h], 0
                mov word[RAM+41Ah], 41Eh
                mov word[RAM+41Ch], 41Eh
 
 
                invoke CreateFile, _fontName, GENERIC_READ, FILE_SHARE_READ, NULL,\ ; --loading the font
                                                   OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL
 
                mov [hFontFile], eax
                invoke ReadFile, dword[hFontFile], fontMem, fontMemSize, fontReaded, NULL 
                invoke CloseHandle, dword[hFontFile]         
                
                invoke CreateFile, _paletteName, GENERIC_READ, FILE_SHARE_READ, NULL,\ ; --loading the font
                                                   OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL

                mov [hFontFile], eax
                invoke ReadFile, dword[hFontFile], btm_13h.palette, colorsFile, fontReaded, NULL
                invoke CloseHandle, dword[hFontFile]              
                
                mov esi, btm_13h.palette
                mov ecx,255
                loopStart:
                                   mov ebx, [esi]
                                   mov dword[swapper],ebx
                                   mov bl, byte[swapper+0]
                                   mov bh, byte[swapper+2]
                                   mov byte[swapper+2],bl
                                   mov byte[swapper+0],bh
                                   mov ebx, dword[swapper]
                                   mov dword[esi], ebx
                                   add esi,4
                                   loop loopStart                                       
 
                invoke GetSystemMetrics, SM_CXSCREEN ; --finding screen resolution for creating a window on whole user screen
                mov [screenWidth], eax  
                invoke GetSystemMetrics, SM_CYSCREEN
                mov [screenHeight], eax
 
                invoke GetModuleHandle, 0 ; --creating window
                mov [wcex.hInstance], eax
                invoke RegisterClassEx, wcex
                invoke CreateWindowEx, 0, dword[wcex.lpszClassName], _title,\
                                                           WS_OVERLAPPEDWINDOW + WS_VISIBLE, 0,\
                                                           0, dword[screenWidth], dword[screenHeight],\
                                                           0, 0, dword[wcex.hInstance], 0
                mov dword[hWnd], eax
 
                invoke GetDC, dword[hWnd] 
                mov dword[hDC], eax
 
                invoke SetTimer, dword[hWnd], IDT_INSERT, 500, NULL 
        ; MESSAGE LOOP
        message_loop:
                invoke PeekMessage, mesg, dword[hWnd], 0, 0, PM_NOREMOVE  ;             while true
                cmp eax, 0                                                ;                     if PeekMessage then                                                                     
                jz .noMessageInQueue                                      ;                     get....
                invoke GetMessage, mesg, dword[hWnd], 0, 0                ;                     else if !waitMessage
                cmp eax, 0                                                ;                             if isProgram
                je end_d                                                                                  ;                             cpuTact
                invoke TranslateMessage, mesg                                                     ;                             else
                invoke DispatchMessage, mesg                                                      ;                                     break
                jmp message_loop
                .noMessageInQueue:
                        cmp [isWaitingInput], 1                                                         
                        je message_loop        
                        cmp [isProgram], 1
                        jne message_loop        
                        call cpuTact
                        mov dword[IPpointer],esi
                        inc byte[amountCPU]
                        cmp byte[amountCPU], 2
                        jne message_loop
                        invoke InvalidateRect, [hWnd], 0, 0
                jmp message_loop
 
 
        end_d:
                invoke ExitProcess, 0
 
; ********************
; *    WINDOWPROC    *
; ********************
proc WindowProc uses edx ebx ecx esi edi, hwnd, wmsg, wparam, lparam
                locals
                        ps              PAINTSTRUCT
                        rcClient        RECT
                endl  
 
                ;cmp [wmsg], WM_SYSKEYDOWN ; --analys key (namely, alt + smth)
                ;je .inputKey
                cmp [wmsg], WM_KEYDOWN ; --analys key (namely, not alphabet and digits)
                je .inputKey
                ;cmp [wmsg], WM_KEYUP ; --analys key for flags only
                ;je  .changeFlags
                cmp [wmsg], WM_CHAR ; --analys key (namely, ASCII symbols)
                je .inputTextASCII
                cmp [wmsg], WM_TIMER ; --insert cursor
                je .paintInsert
                cmp [wmsg], WM_PAINT ; --"cleaning" window
                je .paintWindow
                cmp [wmsg], WM_DESTROY
                je .destroyWindow
 
        ; START OF PROCCESING SOME KIND OF MESSAGE      
        .defProc:
                invoke DefWindowProc, dword[hwnd], dword[wmsg], dword[wparam], dword[lparam]
                ret
 
        .inputKey: ; KEY/SYSKEYDOWN
                xor ecx, ecx
                cmp [wparam],  VK_RSHIFT ; -- analys shift keys and alt for keyboard buffer
                jz .movKeyboardFlags
                inc cl
                cmp [wparam],  VK_LSHIFT
                je .movKeyboardFlags
                inc cl
                cmp [wparam],  VK_CONTROL
                je .movKeyboardFlags
                inc cl
                cmp [wparam],  VK_MENU 
                je .movKeyboardFlags
                inc cl
                cmp [wparam],  VK_SCROLL
                je .movKeyboardFlags
                inc cl
                cmp [wparam],  VK_CAPITAL
                je .movKeyboardFlags  
                inc cl
                cmp [wparam],  VK_INSERT
                je .changeInsert 
 
                cmp [wparam], VK_CANCEL ;Ctrl+BREAK
                call int23h
 
                cmp [wparam], VK_BACK
                je .clearSymbol
                cmp [wparam], VK_DELETE
                je .clearSymbol
                cmp [wparam], VK_RETURN
                je .pressedEnter
                jmp end_wp
 
                .movKeyboardFlags:
                        mov al, 0000001b
                        rol al, cl
                        or byte[RAM + 417h], al
                        jmp end_wp
                                .changeInsert:
                                            push ebx
                                                mov bl, byte[insertType]
                                                xor bl, -1 ; changing state
                                                mov byte[insertType], bl
                                                jmp end_wp
                .clearSymbol:
                        cmp [isProgram], 0
                        je .notProgram
                        cmp [isWaitingInput], 1
                        jne .stopClear
                        
                                cmp byte[GeneralRegs+1], 01h
                                jz .h01Clear
                        .clearPlace:
                                    movzx eax, [currPos]
                                    cmp [startPos], ax
                                    jnb .stopClear
                                stdcall nextPosition
                                stdcall clearPos
                                stdcall clearPos
                                cmp byte[GeneralRegs+1], 0ah
                                jnz .changeBP
                                xor ebx, ebx    ; --calculating address of DS:DX
                                                            mov bx, word[SegmentRegs+6]
                                                                shl ebx, 4
                                                                xor edx,edx
                                                                mov dx, word[GeneralRegs+4]
                                                                add ebx, edx
                                                                add ebx, RAM
                                                                call readSymbol
                                cmp eax, 0
                                jz .noSymb
                                    movzx edx, byte[ebx+1]
                                                                        mov byte[ebx+edx+1], al
                                                                        stdcall DrawSymb, eax, 0, 8
                                                                        ;cmp word[RAM+41Ah], 41eh
                                    ;jz end_wp
                                    ;sub word[RAM+41Ah], 2  ; <<MAKE CHECK OF OVERFLOW>>
                                    jmp end_wp
                                .noSymb:
                                                                        dec byte[ebx+1]
                                                .changeBP:
                                cmp word[RAM+41Ah], 41eh
                                jz end_wp
                                sub word[RAM+41Ah], 2
                                cmp word[RAM+41Ch], 41eh
                                jz end_wp
                                sub word[RAM+41Ch], 2
                                jmp end_wp
                        .isNewLine:
                                movzx eax, word[ptrBufferInput]
                                add ax, word[startPos]
                                cmp ax, 79
                                ja .clearPlace
                                jmp .stopClear
                        .notProgram:
                                    movzx eax, [currPos]
                                                                sub eax, 2
                                                                cmp ax, [startPos]
                                                                jna .isNewLine
                                    stdcall nextPosition
                                stdcall clearPos
                                stdcall clearPos
                                dec [ptrBufferInput]
                                                                jmp end_wp
                                                .h01Clear:
                                                                mov byte[GeneralRegs], 0
                                                                jmp .changeBP
                        .stopClear:
                                jmp end_wp
 
 
                .pressedEnter:
                        cmp [isProgram], 0
                        jnz .program
                        cmp [ptrBufferInput], 0
                        jz end_wp
                        stdcall nextPosition
                        stdcall clearPos
                        stdcall newLine
                        stdcall findCommand
                        mov al, 0
                        mov ecx, 128
                        mov edi, bufferInput
                        rep stosb 
                        mov [ptrBufferInput], 0
                        jmp end_wp
                        .program:
                                cmp [isWaitingInput], 1
                                jne end_wp
                                stdcall nextPosition
                                stdcall clearPos
                                .int21h:
                                        xor edx, edx
                                        mov dl, [GeneralRegs+1]
                                        cmp dl, 08h
                                        jnz .nextF
                                        mov [isWaitingInput], 0
                                        call newLine
                                        .nextF:
                                              cmp dl, 01h
                                              jne .h0a
                                              .h01:
                                             ;                                                     call newLine
                                             ;                                                     mov [currPos], 0
                                             ;                                                     movzx edx, byte[GeneralRegs]
                                             ;                                                    ; stdcall DrawSymb, edx, 0, 8
                                             ;                                                     mov [isWaitingInput], 0    
                                                                                                  jmp end_wp
                                              .h0a:   
                                                                                                        .readyString:
                                                                                                                        xor eax, eax    ; --calculating address of DS:DX
                                                                                                                        mov ax, word[SegmentRegs+6]
                                                                                                                        shl eax, 4
                                                                                                                        xor edx,edx
                                                                                                                        mov dx, word[GeneralRegs+4]
                                                                                                                        add eax, edx
                                                                                                                        add eax, RAM
 
                                                                                                                        xor ebx, ebx
                                                                                                                        movzx ebx, byte[eax+1]
                                                                                                                        mov byte[eax+ebx+2], 13
                                                                                        mov [isWaitingInput], 0                         
                                                                        jmp end_wp
                jmp end_wp      
 
        .inputTextASCII: ; CHAR       ~I hope, that immediately after KEYDOWN is CHAR~
                                invoke KillTimer, dword[hWnd], IDT_INSERT
                cmp byte[lparam+24], 0 ; --checking is this an extended key
                je .end_defProc
                cmp byte[wparam], 0Dh ; --skip ENTER
                je .end_it
                cmp byte[wparam], 8 ; --skip BACKSPACE
                je .end_it
                cmp [isProgram], 0
                je .notProg
                stdcall incReadPointer, 1
                cmp eax, 0
                jnz .end_defProc
                xor ebx, ebx
                movzx ebx, word[RAM+41Ah]
                movzx eax, byte[wparam] ; --setting ASCII code of non-extended symb
                mov byte[RAM+ebx+1], al
                cmp [isWaitingInput], 1
                jne .changePointer
                stdcall incReadPointer, 0
                .i21h:
                        mov dl, [GeneralRegs+1]
                        cmp dl, 0ah
                        je .ha
                        .h1:
                                cmp byte[GeneralRegs], 0
                                jne .isWritten
                                call readSymbol
                                mov [GeneralRegs], al
                                cmp byte[GeneralRegs+1], 08h
                                jz .changePointer
                                stdcall DrawSymb, eax, 0, 8
                                mov [isWaitingInput], 0 
                                jmp .changePointer
                                
                                ;stdcall prevPosition                                
                                .isWritten:
                                     invoke Beep, 783, 270
                                jmp .changePointer
                        .ha:    
                                xor eax, eax    ; --calculating address of DS:DX
                                mov ax, word[SegmentRegs+6]
                                shl eax, 4
                                xor edx,edx
                                mov dx, word[GeneralRegs+4]
                                add eax, edx
                                add eax, RAM
                                mov ebx, eax
 
                                mov dl, [eax]
                                dec dl
                                cmp byte[eax+1], dl
                                jnae .below
                                .above:
                                        invoke Beep, 783, 270
                                        jmp .changePointer
                                .below:
                                        inc byte[ebx+1] ; --inc the actual length
                                        movzx edx, byte[ebx+1]
                                        call readSymbol
                                        mov byte[ebx+1+edx], al ; --adding new symbol
                                        stdcall DrawSymb, eax, 0, 8
                                        ;stdcall prevPosition 
                                                                            ;mov dl, al
                                        ;xor ebx, ebx
                                        ;mov bl, byte[ebx+1]
                                        ;add ebx, ebx
                                         ;xor eax, eax
 
               .changePointer: ; --checking is this an end of the buffer
 
                                                 ;mov ax, [RAM+41Ch] ; --checking isn't it the end of keyboard buffer     
                         ;sub ax, 41Eh
                         ;add ax, 2
                         ;mov dl, 32
                         ;div dl
                         ;cmp ah, 0
                         ;je .goToStart
                         ;add word[RAM+41Ch], 2
               ;         mov ax, [RAM+41Ah] ; --checking is this an overflow
               ;         cmp ax, [RAM+41Ch]
               ;         jae .goToStart
                        ;invoke Beep, 783, 270
                        ; either goToStart or do not write to buffer until it's read an character       
               ;         jmp .end_defProc    
                ;        .goToStart:
                ;               mov word[RAM+41Ch], 41Eh                        
                jmp .end_defProc
                .notProg:
                        cmp [ptrBufferInput], 127
                        jnae .notWhole
                        invoke Beep, 783, 270
                        jmp .end_defProc
                        .notWhole:
                                xor eax, eax
                                xor ebx, ebx
                                movzx eax, word[wparam]
                                movzx ebx, byte[ptrBufferInput]
                                mov word[bufferInput+ebx*2], ax
                                stdcall DrawSymb, eax, 0, 8
                                inc byte[ptrBufferInput]
                        jmp .end_defProc
 
                .end_defProc:
                                        invoke SetTimer, dword[hWnd], IDT_INSERT, 500, NULL 
                                        mov [insertState], -1
                                        jmp .defProc
                                .end_it:
                                        invoke SetTimer, dword[hWnd], IDT_INSERT, 500, NULL 
                                        mov [insertState], -1
                                        jmp end_wp                                      
        .changeFlags: ; KEYUP
                mov cl, 0
                cmp [wparam],  VK_RSHIFT ; --analys shift keys and alt for keyboard buffer
                jz .movKeyboardFlag
                inc cl
                cmp [wparam],  VK_LSHIFT
                je .movKeyboardFlag
                inc cl
                cmp [wparam],  VK_CONTROL
                je .movKeyboardFlag
                inc cl
                cmp [wparam],  VK_MENU 
                je .movKeyboardFlag
                inc cl
                cmp [wparam],  VK_SCROLL
                je .movKeyboardFlag
                inc cl
                cmp [wparam],  VK_CAPITAL
                je .movKeyboardFlag  
                inc cl
                cmp [wparam],  VK_INSERT
                je .movKeyboardFlag 
                jmp end_wp
 
                .movKeyboardFlag:
                        mov al, 1111110b
                        rol al, cl
                        and byte[RAM + 417h], al
                        jmp end_wp
 
        .paintInsert: ; TIMER
               cmp [isProgram], 0
               jne .programI
               .paintI:
                                        call paintIns
               jmp end_wp
               .programI:
                                        cmp [isWaitingInput], 1
                                        jz .paintI
                                        jmp end_wp
 
 
        .paintWindow: ; PAINT
                lea edx, [rcClient]
                invoke GetClientRect, [hWnd], edx
                lea ebx, [ps]
                invoke BeginPaint, [hWnd], ebx
                ;mov edx, btm_03h.header
                mov edx, [currVideoMode]
                inc edx
                                mov ecx, [edx + BITMAPINFOHEADER.biHeight]
                                not ecx    
                mov ebx, edx
                add ebx, sizeof.BITMAPINFOHEADER
                add ebx, 1024
                
                mov ebx, btm_03h.videoModeMem
                cmp byte[edx-1], 3
                je .paintWind
                mov ebx, [btm_13h.videoModeMem]
                .paintWind:
                        invoke StretchDIBits, [hDC], 0, 0, [rcClient.right], [rcClient.bottom], \
                                                                  0, 0,  [edx + BITMAPINFOHEADER.biWidth], ecx,\
                                                                  ebx, edx, 0, SRCCOPY       
                invoke EndPaint, [hWnd], ebx
                cmp byte[repaintFull], 1
                jnz .end_pw
                cmp [isProgram], 1
                jz .end_pw_p
                        invoke GetCurrentDirectory, 79, startString ; <<MAKE EVERY TIME IT GOES TO THE NEX LINE (ALMOST)>>
                        invoke GetCurrentDirectory, 0, 0
                        sub eax, 2
                        mov [startPos], ax
                        mov byte[startString+eax*2+2], '>'
                        stdcall outputString, startString, 1
                .end_pw_p:
                        mov [repaintFull], 0
                .end_pw:
                        xor eax, eax
                        jmp end_wp
 
        .destroyWindow: ; DESTROY
                invoke PostQuitMessage, NULL
                invoke ExitProcess, 0
                xor eax, eax
        end_wp:
                ret
endp
 
 
; ********************
; *    FUNCTIONS     *
; ********************
; INCREASING THE HEAD OF THE KEYBOARD BUFFER
proc incReadPointer uses ebx edx, isChecking
            mov ax, [RAM+41Ah] ; getting head   
        sub ax, 41Eh
        add ax, 2                  ; 'inc' it
        mov dl, 32
        div dl
                cmp ah, 0          ; is it the end of the buffer
            jz .isTailEqu
            xor eax, eax
            cmp [isChecking], 1
            jz .end_ir
            add word[RAM+41Ah], 2 ; if it is all okey with inc of the head
            jmp .end_ir
.isTailEqu:
                mov ax, [RAM+41Ah]
                cmp ax, [RAM+41Ch]
                jz .toStartBuf    
                invoke Beep, 783, 270  ; it is the end of buf end the head isn't equ the tail
                mov eax, 1
                jmp .end_ir
.toStartBuf:
            xor eax, eax
                cmp [isChecking], 1
            jz .end_ir
                mov word[RAM+41Ah], 41Eh
.end_ir:
                ret
endp
 
; LOAD EXECUTABLE PROGRAM INTO THE RAM
proc loadProgram uses eax ebx edx edi
        locals
                file_path du 120 dup(0)
                file_handle dd ?
        endl
 
        lea edi, [file_path]
        mov esi, bufferInput
        movzx edx, [ptrBufferInput]
.loadPath:
        movsw
        dec edx
        cmp edx, 0
        jnz .loadPath
 
        lea edi, [file_path]
 
        invoke CreateFile, edi, GENERIC_READ, FILE_SHARE_READ, \
                           0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
        mov dword[file_handle], eax
        cmp eax, INVALID_HANDLE_VALUE
        je .noFile    
 
.settingRegs:
        mov word[SegmentRegs], 8000h
        mov word[SegmentRegs+2*1], 8000h
        mov word[SegmentRegs+2*2], 8000h
        mov word[SegmentRegs+2*3], 8000h
        mov dword[IPpointer], 80000h+100h+RAM
 
.loadingIntoMemory:
        invoke ReadFile, [file_handle], dword[IPpointer], fileSize, fontReaded, NULL 
        invoke CloseHandle, [file_handle]   
 
.settingStack:
        xor eax, eax
        movzx eax, word[SegmentRegs+2*2] 
        shl eax, 4
        add eax, RAM+0FFFEh
        mov word[eax], 0
        mov word[GeneralRegs+2*4], 0FFFEh
 
        mov [isProgram], 1
 
.settingPSP:
        mov esi, [IPpointer]
        mov word[esi-100h], 020cdh  
        jmp .end_lp    
 
.noFile:
        call outputError
.end_lp:
        ret
endp
 
; READ SYMBOL FROM KEYBOARD BUFFER
readSymbol:
        push ebx
        push edx
        xor ebx, ebx
        xor eax, eax
        mov bx, word[RAM+41Ch]
        cmp bx, word[RAM+41Ah]
        je .clearBuffer
        mov bl, byte[RAM + ebx + 1] ; --getting a symbol
        add word[RAM+41Ch], 2
        mov ax, word[RAM+41Ch] ; --checking isn't it the end of keyboard buffer 
        sub ax, 41Eh
        mov dl, 32
        div dl
        cmp ah, 0
        jne .end_r
.goToStart:
        mov word[RAM+41Ch], 41Eh        
.end_r:
        movzx eax, bl 
.clearBuffer:
                pop edx
            pop ebx
        ret
 
; PAINT INSERT CURSOR
paintIns:
        push ebx
        mov bl, byte[insertState]
        xor bl, -1 ; changing state
        mov byte[insertState], bl
 
        cmp bl, -1
        jnz .drawInsert
        stdcall nextPosition
        stdcall clearPos ; if it's neccessary to clear
        jmp .end_p
.drawInsert:
                mov dx, word[currPos]
        mov bh, byte[currLine]
        mov edi, btm_03h.videoModeMem
 
        xor eax, eax ; --calculate videoModeMem address
        mov al, bh
        mov cl, 80
        mul cl
        shl eax, 4
        add ax, dx
        add edi, eax
        
        cmp [insertType], 0
        jne .wideInsert
        mov ecx, 17
                .loadMem:
                                xor eax, eax
                                mov al, 10000000b
                                stosb
                                add edi, 79 ; Going to the next line
                                loop .loadMem
                        jmp .end_of_paint
                .wideInsert:            
                        mov ecx, 14
                        .loadMemNull:
                                        xor eax, eax
                                        stosb
                                        add edi, 79 ; Going to the next line
                                        loop .loadMemNull
                                        
                        mov ecx, 2
                        .loadMemBottom:
                                        xor eax, eax
                                        mov al, 11111111b
                                        stosb
                                        add edi, 79 ; Going to the next line
                                        loop .loadMemBottom
                         
          .end_of_paint:
                        stdcall nextPosition
                        invoke InvalidateRect, [hWnd], 0, 0              
       ; stdcall DrawSymb, 0B3h, 0, 8
                        stdcall prevPosition
.end_p:        
        pop ebx
        ret
 
; CLEAR SYMBOL
proc clearPos uses ebx edx ecx
        ;cmp word[RAM+41Ah], 41Eh
        ;jz .preparing
    ;dec word[RAM+41Ah]
 
.preparing:
    stdcall prevPosition
 
    mov dx, word[currPos]
    mov bh, byte[currLine]
    mov edi, btm_03h.videoModeMem
 
    xor eax, eax ; --calculate videoModeMem address
    mov al, bh
    mov cl, 80
    mul cl
    shl eax, 4
    add ax, dx
    add edi, eax
 
    mov ecx, 17
    xor eax, eax
.loadMem:
    stosb
    add edi, 79 ; --going to the next line
    loop .loadMem
 
    invoke InvalidateRect, dword[hWnd], 0, 0
    ret
endp    
 
; NEXT POSITION
proc nextPosition uses ebx edx
        mov dx, word[currPos]
        mov bh, byte[currLine]
 
        inc dx
        cmp dx, 79
        jna .noChangeLine
        xor dx, dx ; Set 0 to currPos
        inc bh ; Inc currLine
        cmp bh, 25
        jne .noRepaintFull
        mov byte[repaintFull], 1
        xor bh, bh
.noRepaintFull:
        mov byte[currLine], bh
.noChangeLine:
        mov word[currPos], dx
        ret
endp
 
; PREV POSITION
proc prevPosition uses ebx edx
        mov dx, word[currPos]
        mov bh, byte[currLine]
 
        cmp dx, 0
        jne .noChangeLine
        mov dx, 79 ; Set 79 to currPos
        dec bh ; Dec currLine
        cmp bh, 0
        jne .noRepaintFull
       ; mov byte[repaintFull], 1
.noRepaintFull:
        mov byte[currLine], bh
        jmp .end_pp
.noChangeLine:
        dec dx
.end_pp:
        mov word[currPos], dx
        ret
endp
 
; DRAW SYMBOL
proc DrawSymb uses ebx edx ecx esi edi, symb, clrBack, clrSymb
        mov dx, word[currPos]
        mov bh, byte[currLine]
        mov edi, btm_03h.videoModeMem
        mov esi, fontMem
 
        xor eax, eax ; --calculate videoModeMem address
        mov al, bh
        mov cl, 80
        mul cl
        shl eax, 4
        add ax, dx
        add edi, eax
 
        xor eax, eax ; --calculagte fontMem address
        mov eax, [symb]
        shl eax, 4
 
        add esi, eax
 
        mov ecx, 17
.loadMem:
        ;xor al, al
        movsb
        add edi, 79 ; Going to the next line
        loop .loadMem
 
        stdcall nextPosition
 
        invoke InvalidateRect, [hWnd], 0, 0
        ret
endp
 
; GOING TO THE NEXT LINE
newLine:
        mov word[currPos], 0
        mov word[startPos], 0
        inc byte[currLine]
        cmp byte[currLine], 24
        jne .noRepaintFull
        mov byte[repaintFull], 1
        .noRepaintFull:
        ret
 
; OUTPUTTING STRING AT THE SCREEN
proc outputString uses ecx ebx edx, stringL, isUnicode
            mov ecx, [stringL]
            cmp [isUnicode], 1
            jnz .isAscii
                        mov edx, 2
                        jmp .drawSymbol
.isAscii:
                        mov edx, 1
.drawSymbol:
        cmp byte[ecx], '$'
        jz .end_o
        cmp word[ecx], 0
        jz .end_o
        cmp byte[ecx], 10
        jne .noNewLine
        cmp byte[ecx+edx], 13
        jne .noNewLine
        call newLine
        add ecx, edx
        jmp .end_o
        .noNewLine:
                xor ebx, ebx
                mov bl, byte[ecx]
                stdcall DrawSymb, ebx, 0, 8
                add ecx, edx ; 
                jmp .drawSymbol
.end_o:
        ret
endp 
 
; RECOGNITION OF DOS COMMANDS FROM KEYBOARD BUFFER                       <<MAKE CASE INSENSITIVE>>      <<WITH ANOTHER END OF COMM AND LIST OF COMMS>>
proc findCommand uses esi edi edx
        mov esi, commNames 
        sub esi, 2
        mov eax, -1 ; No command
 
.start:
            add esi, 2
        mov edi, bufferInput
        xor dx, dx  
.goTrought:
        cmp word[esi], 0 ; is it the end of a command
        jz .isFind
        cmp dx, [ptrBufferInput] ; is it the end of the buffer
        jae .nextCommand
        inc dx
        cmpsw
        jz .goTrought
        sub edi, 2
 
.isFind:
            inc eax
        cmp word[edi], ' '
        jz .end_f
        cmp word[edi], 0
        jz .end_f
 
.nextCommand:
        cmp dword[esi], 0 ; If it's not an end of the list of commands
        jz .noCommand
        cmp word[esi], 0 ; If it's not an end of curr command
        jz .start
        add esi, 2
        jmp .nextCommand
 
.noCommand:
        stdcall loadProgram
        ret
.end_f:
        mov [ptrBufferInput], dx
        cmp eax, 14
        jz .noCommand
        mov edx, commTable
        shl eax, 2
        add edx, eax
        mov edx, [edx]
        call edx
        ret
endp
 
 
 
; ********************
; *     COMMANDS     *
; ********************
 
CD_comm:
        stdcall CD_
        ret
 
DIR_comm:
                stdcall DIR_
                ret
 
; CD
proc CD_ uses eax edx ebx ecx edi esi
        locals
                operand du 101 dup(0)
        endl
 
        lea edi, [operand]
        mov esi, bufferInput
        mov ebx, esi
        mov edx, edi
        add si, word[ptrBufferInput]
        add si, word[ptrBufferInput]
        add si, 2
.gettingOperand:
        movsw 
        cmp word[esi], 0
                je .setDir
        cmp edi, [edx+100]
        je .setDir
        cmp esi, [ebx+100]
        jnz .gettingOperand
 
.setDir:
                mov byte[edi], '\'
                mov word[edi+1], 0
        invoke SetCurrentDirectory, edx
        cmp eax, 0
        jne .moved
        call outputError
        jmp .end_c
 
.moved:
        mov ecx, 128
        mov eax, '$'
        mov edi, startString
        rep stosb
        invoke GetCurrentDirectory, 79, startString
        invoke GetCurrentDirectory, 0, 0
        sub eax, 2
        mov [startPos], ax
        mov byte[startString+eax*2+2], '>'
        stdcall outputString, startString, 1
 
.end_c:
        ret
        endp
 
CLS_comm:
        push edi
        push ecx
        push eax
 
        mov edi, btm_03h.videoModeMem
        mov ecx, bmSize 
        xor eax, eax
        rep stosb
 
        invoke InvalidateRect, [hWnd], 0, 0
        mov [repaintFull], 1
        mov [currPos], 0
        mov [currLine], 0
 
        pop eax
        pop ecx
        pop edi
        ret
 

proc DIR_ uses eax ebx edx edi
       locals
            operand du 101 dup(0)
            fileSizeOutput du 50 dup(0)
                        hFind dd 0
       endl
       
       lea edi, [operand]
       invoke GetCurrentDirectory, 99, edi
       invoke GetCurrentDirectory, 0, 0
       shl eax, 1
       add edi, eax
       sub edi, 2
       mov byte[edi], '\'
       mov byte[edi+1], 0
       add edi, 2
       mov byte[edi], '*'
       mov byte[edi+1], 0
       lea ebx, [hFind]
       lea ecx, [operand]
       invoke FindFirstFile, ecx, ffd
       mov [ebx], eax
       
       .gettingFiles:
                   mov edx, [ffd.dwFileAttributes]
                   and edx, FILE_ATTRIBUTE_DIRECTORY
                   cmp edx, 0
                   je .notDir
                                stdcall outputString, ffd.cFileName, 1
                        stdcall outputString, _dirOutput, 1
                        stdcall newLine
                        jmp .whilePart
                   .notDir:
                            stdcall outputString, ffd.cFileName, 1
                            stdcall outputString, _fileOutput, 1
                            lea edx, [fileSizeOutput]
                            cmp [ffd.nFileSizeHigh], 0
                            je .onlyLowPart
                            cinvoke wsprintfW, edx, _formatBytes, [ffd.nFileSizeHigh]
                            lea edx, [fileSizeOutput]
                            stdcall outputString, edx, 1
                            .onlyLowPart:
                                        cinvoke wsprintfW, edx, _formatBytes, [ffd.nFileSizeLow]
                                        lea edx, [fileSizeOutput]
                                        stdcall outputString, edx, 1
                                        stdcall outputString, _bytesOutput, 1
                                        stdcall newLine
                   .whilePart:
                           lea ebx, [hFind]
                           invoke FindNextFile, [ebx], ffd
                           cmp eax, 0
                           jne .gettingFiles
      mov [repaintFull], 1
      ret
endp
 
 
outputError:
        stdcall outputString, errorIndefComm, 1
        stdcall newLine
        stdcall newLine
        mov [repaintFull], 1
ret

; *************************************************
; *                 EGOR'S CODE                   * 
; *************************************************

   cpuTact:
   xor ebx, ebx
   mov esi, [IPpointer]
   mov bl, [esi]
   mov eax, [OPCODES+ebx*4]
   call eax
   ret

  BX_SI_MEM:
        catchAddr
        shr ebx,4
        push ebx
        xor ebx, ebx
        mov bx, [GeneralRegs+6]
        add bx, word[GeneralRegs+12]
 ;      push 0
        push ebx
        push 1
    retAddr
ret 

   BX_DI_MEM:
        catchAddr
        shr ebx,4
        push ebx
        xor ebx, ebx
        mov bx, [GeneralRegs+6]
        add bx, word[GeneralRegs+14]
    ;push 0
        push ebx
        push 1          
        retAddr 
ret

   BP_SI_MEM:
        catchAddr
        shr ebx,4
        push ebx
        xor ebx, ebx
        mov bx, [GeneralRegs+10]
        add bx, word[GeneralRegs+12]
        ;push 0
        push ebx
        push 1
        retAddr
ret

   BP_DI_MEM:
        catchAddr
        shr ebx,4
    push ebx
        xor eax,eax
        xor ebx, ebx
        mov bx, [GeneralRegs+10]
        mov ax, word[GeneralRegs+14]
        add ebx,eax
        ;push 0
        push ebx
        push 1  
        retAddr  
ret

  SI_MEM:
        catchAddr
        shr ebx,4
    push ebx
        xor ebx, ebx
        mov bx, word[GeneralRegs+12]
        ;push 0
        push ebx
        push 1
    retAddr
ret

  DI_MEM:
        catchAddr
    shr ebx,4
    push ebx    
        xor ebx, ebx
        mov bx, word[GeneralRegs+14]
        ;push 0
        push ebx
        push 1
        retAddr
ret

  BX_MEM:
        catchAddr
    shr ebx,4
    push ebx
        xor ebx, ebx
        mov bx, word[GeneralRegs+6]
        ;push 0
        push ebx
        push 1
        retAddr
ret

 IMM_16_MEM:
        catchAddr
        push 1
        push 0
         mov byte[addAddr],2
        ;xor edx,edx
        ;mov dx, [esi+2] better desition to push 0, because in that case it is possible to add word[esi+2] also with [bx+si] per example
      ;  push edx
        push 1 ; may be its the addr of LABEL therefore it's gonna be addresser so push 1 would be suitable
        retAddr
 ret

BP_MEM:
  catchAddr
  shr ebx,4
  push ebx
  xor ebx, ebx
  mov bx, word[GeneralRegs+10]
  ;push 0
  push ebx
  push 1
  retAddr               
ret     


AX_AL:
catchAddr
;mov edx, [ebp]
pop edx
push edx
xor ebx, ebx
mov ebx, GeneralRegs
;cmp edx,1
;jb BIT8_AX_AL
;jmp ender_AX_AL
;BIT8_AX_AL:
;inc ebx
;ender_AX_AL:
push 0
push ebx
push 0
retAddr
ret

CX_CL:
catchAddr
;mov edx, [ebp] ;getting the size of operand
pop edx
push edx
xor ebx, ebx
mov ebx, GeneralRegs
add ebx,2
cmp edx,1
;jb BIT8_CX_CL
;jmp ender_CX_CL
;BIT8_CX_CL:
;inc ebx
;ender_CX_CL:
push 0  
push ebx
push 0
retAddr
ret     

DX_DL:
catchAddr
pop edx
push edx
xor ebx, ebx
mov ebx, GeneralRegs
add ebx,4
;cmp edx,1
;jb BIT8_DX_DL
;jmp ender_DX_DL
;BIT8_DX_DL:
;inc ebx
;ender_DX_DL:
push 0
push ebx
push 0
retAddr
ret

BX_BL:
catchAddr
;mov edx, [ebp] ; size of operands is stored in this position of stack
pop edx
push edx
xor ebx, ebx
mov ebx, GeneralRegs
add ebx,6
;cmp edx,1
;jb BIT8_BX_BL
;jmp ender_BX_BL
;BIT8_BX_BL:
;inc ebx
;ender_BX_BL:
push 0
push ebx
push 0
retAddr
ret     


Reg_H:
catchAddr
pop edx
push edx
;xor ebx, ebx
sub ebx, 27*4
shr ebx,1
add ebx, GeneralRegs
cmp edx,1
jb BIT8_RegH
add ebx,7
BIT8_RegH:
dec ebx 
push 0
push ebx
push 0
retAddr
ret             

MODRM_Analiser: ;pushes the receiving data register,after that whether there is a 8 or 16 bit immediate data needs to be received,
                ;after that the address operand and the message, whether it is a register or an address
  catchAddr
  mov ebp, esp
  xor edx,edx
  xor ebx,ebx
  mov bl, [esi+1]
  and bl, 00111000b
  ror bl, 3
  pop edx ; size of operand is stored in this position of stack
  shl ebx,1 ;impossible to access H
  cmp edx,1
  ;push edx
  je MOD_MODRM_Reg
  ;push ebx ;may be more efficient just to push the index of Reg in the Array
  MOD_MODRM_B:
  cmp bl,8
  jb MOD_MODRM_L
  inc bl        
  MOD_MODRM_L:
  and bl,7
  ;jmp noShift_MOD_MODRM
  MOD_MODRM_Reg:
        ;shl ebx,1
 ; noShift_MOD_MODRM:   
 ; push edx; size of operand stores
  add ebx, GeneralRegs    
  push ebx ;addr of 1st register
  push edx; size of operand stores
  xor edx,edx
  xor ebx,ebx
  mov dl, [esi+1]
  mov bl,dl
  and dl, 11000000b
  shr dl, 3
  and bl, 00000111b
  add bl,dl 
  shl ebx,2
  shr dl,3
  cmp dl,3
  je addAddr0
  mov [addAddr],dl
  jmp moverMODRM
  addAddr0:
        mov byte[addAddr],0
  moverMODRM:
  mov edx,MODR_M_VALUES
  add edx,ebx
  mov edx,[edx]
  call edx
  retAddr
ret       

ZF:
 cmp dx,0
 jne NZ_ZF
 or word[FLAGSr],0000000000000000b
 jmp ender_ZF
 NZ_ZF:
 and word[FLAGSr],1111111111111111b
 ender_ZF:
ret 

SF: ; in the stack must be a value, which would show the size (8 or 16 bit operands) and the result in dx
 mov eax,[ebp+4]
 and al, 0Fh
 cmp al,  2
 jne Byte1SF
 cmp dh, 10000000b
 jb CLRSF
 SETSF:
 or word[FLAGSr],0000000000000000b ;set SF, CF
 jmp ender_SF   
 Byte1SF:
 cmp dl, 10000000b
 jb CLRSF
 jae SETSF
 CLRSF:
 and word[FLAGSr], 1111111111111111b ; clear SF, CF
 ender_SF:
ret

CF:
mov eax, [ebp+4]
cmp eax, 20h
jb CLR_CF
and eax, 0Fh
cmp eax,1
je Byte_CF
 cmp dh, 01111111b      
 jbe SET_CF
 ja CLR_CF
 Byte_CF:
 cmp dl, 01111111b      
 jbe SET_CF
 ja CLR_CF
  SET_CF:
 or word[FLAGSr], 0000000000000000b
 jmp ender_CF
 CLR_CF:
 and word[FLAGSr], 1111111111111111b
 jmp ender_CF
 ender_CF:
ret
 
OF: ;in the stack must be a value, which would show the size (8 or 16 bit operands) and signs of 2 operands and the result in dx
; sign of operands 0- both +, 1- +-, 2- --      
; current stack param: 0000:0000:0000:00XY; X - amount of negatives; Y- size (1-8bit, 2-16bit)
 mov eax, [ebp+4]
 cmp eax, 20h
 jae NegativesOF
 jb PositivesOF
 jmp ender_OF
 NegativesOF:
 and eax, 0Fh
 cmp eax, 1
 je ByteN_OF
 cmp dh, 01111111b      
 jbe SET_OF
 ja CLR_OF
 ByteN_OF:
 cmp dl, 01111111b      
 jbe SET_OF
 ja CLR_OF      
 PositivesOF:
 and eax, 0Fh ; getting the size
 cmp eax, 1
 je Byte_OF
 cmp dh, 01111111b      
 jbe CLR_OF
 ja SET_OF
 Byte_OF:
 cmp dl, 01111111b      
 jbe CLR_OF
 ja SET_OF      
 SET_OF:
 or word[FLAGSr], 0000000000000000b
 jmp ender_OF
 CLR_OF:
 and word[FLAGSr], 1111111111111111b
 jmp ender_OF
 ender_OF:
ret
 
 
        
 
; SECTION WITH INSTRUCTIONS THEMSELVES




EvGv:
        ;catchAddr
  mov byte[addAddr],0
  xor eax,eax
  push 1
  call MODRM_Analiser
  pop ecx ; whether it is [] or just taken from register values
  pop edi ;address of the memory 
  pop ebp ; the param with size of immediate values
  cmp ebp, 1
  jb _noOpc_EvGv
  push ecx
  xor ecx, ecx
  cmp byte[addAddr],1
  ja addWord_EvGv
   jb addByte_EvGv
  mov cl, byte[esi+2]
  movsx cx,cl
  jmp addByte_EvGv
  addWord_EvGv:
  mov cx, word[esi+2]
  addByte_EvGv:
  add di, cx
  pop ecx
 ; mov eax,1
  jmp _noOpc_EvGv
  _noOpc_EvGv:
  add esp,4
  pop ebx ; the general register
 ; xor eax,eax
 ; mov ax, [SegmentRegs+6]
 ; shl eax,4
 ; add ebx,eax
 ; add edi,eax
  mov ebp, esp
  xor edx, edx
  test ecx, ecx
  jz No_Add_RAM_EvGv
  add edi, RAM
  xor eax,eax
  mov ax, [SegmentRegs+6]
  shl eax,4
;  add ebx,eax
  add edi,eax
  No_Add_RAM_EvGv:
ret

 


;!!!!!!!!!!!!!!! ATTENTION THERE'S SMTH WRONG
EbGb:
  mov byte[addAddr],0
  xor eax,eax
  push 0
  call MODRM_Analiser
  pop ecx ; whether it is [] or just taken from register values
  pop edi ;address of the memory 
  pop ebp ; the param with size of immediate values
  cmp ebp, 1
  jb _noOpc_EbGb
 ; mov byte[addAddr],1
  push ecx
  xor ecx, ecx
  cmp byte[addAddr],1
  ja addWord
  jb addByte
  mov cl, byte[esi+2]
  movsx cx,cl
  jmp addByte
  addWord:
  mov cx, word[esi+2]
  addByte:
        movsx ecx,cx
  add edi, ecx
  pop ecx
 ; mov eax,1
  jmp _noOpc_EbGb
  _noOpc_EbGb:
  add esp,4
  pop ebx ; the general register
 ; xor eax,eax
 ; mov ax, [SegmentRegs+6] ;DS
 ; shl eax,4
 ; add ebx,eax
 ; add edi,eax
  mov ebp, esp
  xor edx, edx
  test ecx, ecx
  jz No_Add_RAM_EbGb
  add edi, RAM
  xor eax,eax
  mov ax, [SegmentRegs+6]
  shl eax,4
 ; add ebx,eax
  add edi,eax
  No_Add_RAM_EbGb:
ret


 
EbGb_Finish:    
  add esi,2 
  xor ebx,ebx
  mov bl, byte[addAddr]
  add esi, ebx
ret

EvGv_Finish:
  add esi,2 
  xor ebx,ebx
  mov bl, byte[addAddr]
  add esi, ebx
ret
        
ALIb:
  xor edx, edx
  xor eax,eax
  mov dl, [GeneralRegs]
ret 

AXIz:
  xor edx,edx
  xor eax, eax
  mov dx, [GeneralRegs] 
ret

TWOB:
  inc esi
  xor ebx, ebx
  mov bl, [esi]
  mov eax, [OPCODES_2_BYTE+ebx*4]
  jmp eax
ret    
        


POPer:

  xor edx,edx
  xor ebx, ebx
  mov bx, [SegmentRegs+4]
  shl ebx,4
  mov dx, word[GeneralRegs+8]
  add ebx,edx
  add ebx, RAM
  mov dx, word[ebx]
  add word[GeneralRegs+8],2
ret


PUSHer:
  sub word[GeneralRegs+8],2     
  xor edx,edx
  push ebx
  xor ebx,ebx
  mov bx, [SegmentRegs+4]
  shl ebx,4
  mov dx, word[GeneralRegs+8]
  add ebx,edx
  add ebx, RAM
  mov word[ebx],dx
 ; sub word[GeneralRegs+8],2
  pop ebx
ret



ADDEbGb:
  call EbGb
  mov dl,byte[edi]
  add dl, byte[ebx]
  
  ;flags changing
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],0
  ja GbEb_ADDEbGb
  mov [edi], dl
  jmp finish_ADDEbGb
  GbEb_ADDEbGb:
  mov [ebx], dl
  finish_ADDEbGb:
  call EbGb_Finish

ret

        
ADDEvGv:
  call EvGv
  mov dx,word[edi]
  add dx, word[ebx]
  
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],1
  ja GvEv_ADDEvGv
  mov [edi], dx
  jmp finish_ADDEvGv
  GvEv_ADDEvGv:
  mov [ebx], dx
  finish_ADDEvGv:
  call EvGv_Finish
ret
        
ADDALIb:
  call ALIb
  
  add dl, byte[esi+1]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [GeneralRegs], dl
 add esi,2
ret
        
ADDAXIz:
  call AXIz
  add dx, word[esi+1]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [GeneralRegs], dx
  add esi,3
 ; shr al, 4
 ; add al,2
  add esi,3
ret
        
PUSHFs: 
  PushSEG 8
ret
        
POPFs:  
  PopSEG 8
ret     


PUSHEs:
  PushSEG 0
ret

POPEs:
  PopSEG 0
ret
 
PUSHCs:
  PushSEG 2    
ret

POPCs:
  PopSEG 2
ret

PUSHSs:
  PushSEG 4
ret

POPDs:
  PopSEG 6
ret
 
PUSHDs:
  PushSEG 6
ret

POPSs:
  PopSEG 4
ret

PUSHGenR:
  sub word[GeneralRegs+8],2     
xor ebx, ebx
 mov bl, [esi]
 sub bl, 50h
 shl ebx,1      
 push esi
 xor esi,esi
 xor eax,eax
 mov si, word[SegmentRegs+4]
 mov ax, [GeneralRegs+8]
 shl esi, 4
 add esi,eax
 xor edx,edx
 mov dx, [GeneralRegs+ebx]                      
 mov word[ESI+RAM],dx
 ;sub word[GeneralRegs+8],2
 pop ESI
 inc ESI
ret

OREbGb: ; this section is fully stolen from ADDEbGb, therefore some thoughts about creating a function with it crope up
  call EbGb
  mov dl,byte[edi]
  or dl, byte[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],8
  ja GbEb_OREbGb
  mov [ebx], dl
  jmp finish_OREbGb
  GbEb_OREbGb:
  mov [edi], dl
  finish_OREbGb:
  call EbGb_Finish
ret

OREvGv: ; this section is fully stolen from ADDEbGb, therefore some thoughts about creating a function with it crope up
  call EvGv
  mov dx,word[edi]
  or dx, word[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],0Bh
  jb GvEv_OREvGv
  mov [edi], dx
  jmp finish_OREvGv
  GvEv_OREvGv:
  mov [ebx], dx
  finish_OREvGv:
  call EvGv_Finish
ret

 ORALIb:
  xor edx,edx
  mov dl, [GeneralRegs]
  or dl, byte[esi+1]
  
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [GeneralRegs], dl
  add esi,2
ret
                
ORAXIz:
  mov dx, [GeneralRegs]
  or dx, word[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  mov word[GeneralRegs], dx
  add esi,3 
ret

 ADCEvGv: ; i think i should rewrite this shit
  call EvGv
  xor edx, edx
  mov edx,1
  and dx, word[FLAGSr] ;getting the value of CF
  add dx, word[edi]

  or word[FLAGSr], bx
  cmp byte[esi],11
  jb GvEv_ADCEvGv
  add dx, word[ebx] 
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov word[edi],dx
  jmp finish_ADCEvGv
  GvEv_ADCEvGv:
  add word[ebx], dx
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  finish_ADCEvGv:
  call EvGv_Finish
ret

ADCEbGb:
  call EbGb
  xor edx, edx
  mov edx,1
  and dx, word[FLAGSr]
  add dl, byte[edi]
  add dl, byte[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],10
  ja GbEb_ADCEbGb
  mov [ebx], dl
  jmp finish_ADCEbGb
  GbEb_ADCEbGb:
  mov [edi], dl
  finish_ADCEbGb:
  call EbGb_Finish  
ret

; from here the changes are necessary 
ADCALIb:
  call ALIb
  xor edx, edx
  mov edx,1
  and dx, word[FLAGSr]
  add dl, [GeneralRegs]
  add dl, byte[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  mov [GeneralRegs], dl
  add esi,2
  
ret
        
 ADCAXIz:
  call AXIz     
  xor edx,edx
  mov edx,1
  and dx, word[FLAGSr]
  add dx, [GeneralRegs]
  add dx, word[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  mov word[GeneralRegs], dx
  add esi,3

ret

SBBEbGb:
  call EbGb
  xor edx,edx
  mov edx,1
  and dx, word[FLAGSr]
  add byte[ebx], dl
  mov dl, [edi]
  sub dl, byte[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],18h
  ja GbEb_SBBEbGb
  mov [edi], dl
  jmp finish_SBBEbGb
  GbEb_SBBEbGb:
  mov [ebx], dl
  finish_SBBEbGb:
  call EbGb_Finish

ret     

SBBEvGv:
  call EvGv
  xor edx,edx
  mov edx,1
  and dx, word[FLAGSr]
  add word[ebx],dx
  mov dx, [edi]
  sub dx, word[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],19h
  ja GvEv_SBBEvGv
  mov [ebx], dx
  jmp finish_SBBEvGv
  GvEv_SBBEvGv:
  mov [edx], dx
  finish_SBBEvGv:
 ; add esp, 4
  add esi,2
  push ecx
  xor ecx, ecx
  mov cl, al 
  add esi,ecx
  pop ecx
  call EvGv_Finish
ret

SBBALIb:
  call ALIb
  xor edx, edx
  mov edx,1
  and dx, word[FLAGSr]
  mov dh, [GeneralRegs]
  add dl, byte[esi+1]
  sub dh,dl
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
    
  mov [GeneralRegs], dh
  add esi,2
ret

 SBBAXIz:
  call AXIz     
  xor edx,edx
  mov edx,1
  and dx, word[FLAGSr]
  mov bp, [GeneralRegs]
  add dx, word[esi+1]
  sub bp, dx
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
 mov word[GeneralRegs], bp
 add esi,3
ret


ANDEbGb:
  call EbGb
  mov dl,byte[edi]
  and dl, byte[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi], 20h
  ja GbEb_ANDEbGb
  mov [edi], dl
  jmp finish_ANDEbGb
  GbEb_ANDEbGb:
  mov [ebx], dl
  finish_ANDEbGb:
  call EbGb_Finish

ret
        
 ANDEvGv:
  call EvGv
  mov dx,word[edi]
  and dx, word[ebx]
  
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],21h
  ja GvEv_ANDEvGv
  mov [edi], dx
  jmp finish_ANDEvGv
  GvEv_ANDEvGv:
  mov [ebx], dx
  finish_ANDEvGv:
  call EvGv_Finish     
ret 

ANDALIb:
  xor edx,edx
  mov dl, [GeneralRegs]
  and dl, byte[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  mov [GeneralRegs], dl
  add esi,2
ret

 ANDAXIz:
  mov dx, [GeneralRegs]
  and dx, word[esi+1]
 
  pushf
    pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov word[GeneralRegs], dx
  add esi,3 
ret     

SUBEbGb:
  call EbGb 
  cmp byte[esi],28h
  ja GbEb_SUBEbGb
  mov dl,byte[edi]
  sub dl, byte[ebx]
  pushf
  mov [edi], dl
  jmp finish_SUBEbGb
  GbEb_SUBEbGb:
  mov dl,byte[ebx]
  sub dl, byte[edi]
  pushf
  mov [ebx], dl
  finish_SUBEbGb:
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx   
  call EbGb_Finish
ret

SUBEvGv:
  call EvGv   
  cmp byte[esi],29h
  ja GvEv_SUBEvGv
  mov dx, [edi]
  sub dx, [ebx]
  pushf
  mov [edi], dx
  jmp finish_SUBEvGv
  GvEv_SUBEvGv:
  mov dx, [ebx]
  sub dx, [edi]
  pushf
  mov [ebx], dx
  finish_SUBEvGv:
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  call EvGv_Finish
ret


SUBALIb:
  call ALIb
  sub dl, byte[esi+1]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [GeneralRegs], dl
  add esi,2
ret
        
SUBAXIz:
  call AXIz
  sub dx, word[esi+1]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [GeneralRegs], dx
  add esi,3
ret


XOREbGb:
  call EbGb
  mov dl,byte[edi]
  xor dl, byte[ebx]
  
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi],30h
  ja GbEb_XOREbGb
  mov [edi], dl
  jmp finish_XOREbGb
  GbEb_XOREbGb:
  mov [ebx], dl
  finish_XOREbGb:
  call EbGb_Finish
ret

XOREvGv:
  call EvGv
  mov dx,word[edi]  
  xor dx, word[ebx]
  
  pushf
    pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  cmp byte[esi], 31h
  ja GvEv_XOREvGv
  mov [edi], dx
  jmp finish_XOREvGv
  GvEv_XOREvGv:
  mov [ebx], dx
  finish_XOREvGv:
  call EvGv_Finish
ret

XORALIb:
  xor edx,edx
  mov dl, [GeneralRegs]
  xor dl, byte[esi+1]
 
  pushf
    pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  mov [GeneralRegs], dl
  add esi,2
ret
                
XORAXIz:
  mov dx, [GeneralRegs]
  xor dx, word[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov word[GeneralRegs], dx
  add esi,3 
ret

CMPEbGb:
  call EbGb
  cmp byte[esi], 39h
  ja .GbEb
  mov dl,byte[edi]
  cmp dl, byte[ebx]
  jmp .flags
  .GbEb:
        mov dl, byte[ebx]
        cmp dl, byte[edi]
  .flags:
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
 
  call EbGb_Finish
ret

CMPEvGv:
  call EvGv
  cmp byte[esi], 38h
  ja .GvEv
  mov dx,word[edi]
  cmp dx, word[ebx]
  jmp .flags
  .GvEv:
        mov dx, word[ebx]
        cmp dx, word[edi]
  .flags:
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  

  call EbGb_Finish
ret

CMPALIb:
  xor edx,edx
  mov dl, [GeneralRegs]
  cmp dl, byte[esi+1]
  
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  add esi,2
  add esp,4
ret

CMPAXIz:
  mov dx, [GeneralRegs]
  cmp dx, word[esi+1]
 
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 

  add esi,3 
  
  ;add esp,4
ret


INCGenR:
   xor edx,edx
   xor eax,eax
   xor ebx,ebx
   mov bl,[esi]
   sub bl, 40h ; the bit mask shall be refactored including the position of the INC_AX instruction
   shl ebx,1
   inc eax
   mov dx, word[GeneralRegs+ebx]

   inc dx
   
   pushf
    pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
   
   mov [GeneralRegs+ebx], dx
   inc esi

ret


POPGenR:
  call POPer
  xor ebx,ebx
  mov bl, [esi]
  sub ebx, 58h
  shl ebx, 1
  mov word[GeneralRegs+ebx], dx
  inc esi
ret

PUSHA_ :; the reason for storing in stack the address of the return, not just jmp to the CPU_Cycle
  xor ebx,ebx
  push Pusher_PUSHA
  Pusher_PUSHA:
   call PUSHer
        mov word[GeneralRegs+ebx], dx
        add ebx,2
        cmp ebx,14
  jne Pusher_PUSHA
  add esp,4
ret



POPA_ :; the reason for storing in stack the address of the return, not just jmp to the CPU_Cycle
  mov ebx, 14
  push Pusher_POPA
  Pusher_POPA:
   call POPer
        mov word[GeneralRegs+ebx], dx
        sub ebx,2
        test ebx,ebx
  jnz Pusher_POPA
  add esp,4
ret

NOP_:
  inc esi
ret
  
DECGenR:
  xor edx,edx
  xor eax,eax
  xor ebx,ebx
  mov bl,[esi]
  sub bl, 48h ; the bit mask shall be refactored including the position of the DEC_AX instruction
  shl ebx,1
 ; inc eax
  mov dx, word[GeneralRegs+ebx]
  dec dx
 
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  mov [GeneralRegs+ebx], dx
   inc esi
ret
 
PUSHIz:
  sub word[GeneralRegs+8],2  
  xor ebx,ebx
  xor eax,eax
  mov bx, [SegmentRegs+4]
  mov ax,word[GeneralRegs+8]
  shl ebx, 4 
  add ebx, eax
  add ebx, RAM
  xor edx,edx
  mov dx, word[esi+1]
  mov word[ebx], dx
  ;sub word[GeneralRegs+8],2
  add esi,3
ret

PUSHIb:
  sub word[GeneralRegs+8],2     
  xor ebx,ebx
  xor eax,eax
  mov bx, [SegmentRegs+4]
  mov ax,word[GeneralRegs+8]
  shl ebx, 4 
  add ebx, eax
  add ebx, RAM
  xor edx,edx
  mov dl,byte[esi+1]
  mov word[ebx], dx
;  sub word[GeneralRegs+8],2
  add esi,2
ret

sJMPJb:
        xor eax,eax
        mov ebx, esi
        sub ebx,RAM
        mov al,[esi+1]
        movsx ax,al
       ; adc al,0
        add bx,ax
        add ebx,RAM
        mov esi, ebx
        add esi,2
ret     

nJMPJz:
        xor eax,eax

        mov ebx, esi
        sub ebx, RAM
        mov ax,[esi+1]
       ; adc al,0
        add bx,ax
        add ebx,RAM
        mov esi,ebx
        add esi,3
ret     

fJMPAp:
        xor eax,eax
        xor ebx,ebx
        mov cx, [esi+1]
        mov ax, [esi+3]
        mov esi,RAM
        shl eax,4
        add esi,ecx
        add esi,eax
ret     
  
; some thoughts are neccessary here
JccNEnd:
 ; mov ax,dx
  mov cl,[esi-1]
  and dx, word[FLAGSr]
  cmp dx,0
  je JMPer_JccNEnd
  No_JMP_JccNEnd:
  add esi,2
  jmp End_JccNEnd
  JMPer_JccNEnd:
 ; mov bl,[esi-1]
  cmp bl,0Fh
  je JccNEnd16
        xor eax,eax
        mov ebx, esi
        sub ebx, RAM
        mov al,[esi+1]
        movsx ax,al
       ; adc al,0
       add bx,ax
        add ebx,RAM
        mov esi,ebx
        add esi,2
   jmp noInc_JccNEnd
        JccNEnd16:
        xor eax,eax
        mov ax,[esi+1] ;this variant might be irrelevant
        add esi, eax
  End_JccNEnd:
 ;   cmp cl,0Fh
 ;   jne noInc_JccNEnd
 ;   inc esi
   noInc_JccNEnd: 
ret

JccEnd:
  mov cl,[esi-1]
  mov ax,dx
  and ax, word[FLAGSr]
  cmp dx,ax
  je JMPer_JccEnd
  No_JMP_JccEnd:
  add esi,2 ; useful for short jumps(8 bits), need to deal with near jumps(16bit)
  jmp End_JccEnd
  JMPer_JccEnd:
  ;mov bl,[esi-1]
  cmp cl,0Fh ; the previous byte of instruction equals 0F
  je JccEnd16
        xor eax,eax
        mov ebx, esi
        sub ebx, RAM
        mov al,[esi+1]
        movsx ax,al
       ; adc al,0
       add bx,ax
        add ebx,RAM
        mov esi,ebx
        add esi,2
   jmp noInc_JccEnd
        JccEnd16:
        xor eax,eax
        mov eax, esi
        sub eax, RAM
        sub eax, 100h
        add ax,[esi+1]
       ; adc al,0
        add eax,RAM
        add eax,100h
        mov esi,eax
        add esi,2
  End_JccEnd:
      ;  cmp cl, 0Fh
      ;  jne noInc_JccEnd
      ;  inc esi
        noInc_JccEnd:
ret
   
JccNO:
  xor edx,edx
  mov dx, 0000100000000000b ; here's gonna be the bit-mask for the OF
  call JccNEnd
ret

SetNO_:
        ;and word[FLAGSr], 1111011111111111b
ret

JccO:
  xor edx,edx
  mov dx, 0000100000000000b ; here's gonna be the bit-mask for the OF
  call JccEnd
ret

SetO_:
        ;or word[FLAGSr], 0000100000000000b
ret     

JccNB:
  xor edx,edx
  mov dx, 0000000000000001b ; here's gonna be the bit-mask for the OF
  call JccNEnd
ret

SetNB_:
        ;and word[FLAGSr], 1111111111111110b
ret

JccB:
  xor edx,edx
  mov dx, 0000000000000001b ; here's gonna be the bit-mask for the OF
  call JccEnd
ret

SetB_:
        ;or word[FLAGSr], 0000000000000001b
ret

JccNZ:
  xor edx,edx
  mov dx, 0000000001000000b ; here's gonna be the bit-mask for the OF
  call JccNEnd
ret

SetNZ_:
        ;and word[FLAGSr], 1111111110111111b
ret

JccZ:
  xor edx,edx
  mov dx, 0000000001000000b ; here's gonna be the bit-mask for the OF
  call JccEnd
ret

SetZ_:
        ;or word[FLAGSr],  0000000001000000b
ret

JccA:
  xor edx,edx
  mov dx, 0000000001000001b ; 
  and dx, [FLAGSr]
  cmp dx,1000001b
  je callJMP_A
  cmp dx,0
  je callJMP_A
  call No_JMP_JccNEnd
  jmp end_JccA
  callJMP_A:
  call JMPer_JccNEnd
  end_JccA: 
ret

SetNA_:
        ;and word[FLAGSr], 1111111110111110b
ret

JccNA:
  xor edx,edx
  mov dx, 0000000001000001b ; here's gonna be the bit-mask for the OF
  and dx, [FLAGSr]
  cmp dx,1000001b
  je callNOJMP_NA
  cmp dx,0
  je callNOJMP_NA
  call JMPer_JccEnd
    jmp end_JccNA
  callNOJMP_NA:
  call No_JMP_JccEnd
  end_JccNA:
ret

SetA_:
        ;or word[FLAGSr],  0000000001000001b
ret

JccNS:
  xor edx,edx
  mov dx, 0000000010000000b ; here's gonna be the bit-mask for the OF
  call JccNEnd
ret

SetNS_:
        ;and word[FLAGSr], 1111111101111111b
ret

JccS:
  xor edx,edx
  mov dx, 0000000010000000b ; here's gonna be the bit-mask for the OF
  call JccEnd
ret

SetS_:
        ;or word[FLAGSr],  00000000010000000b
ret

JccNP:
  xor edx,edx
  mov dx, 0000000000000100b ; here's gonna be the bit-mask for the OF
  call JccNEnd
ret

SetNP_:
        ;and word[FLAGSr], 1111111111111011b
ret

JccP:
  xor edx,edx
  mov dx, 0000000000000100b ; here's gonna be the bit-mask for the OF
  call JccEnd
ret

SetP_:
        ;or word[FLAGSr],  00000000000000100b
ret

; this section is ruining everything i'd done with handling flags:(
JccNL:
  xor edx,edx
  mov dx, 0000100010000000b 
  and dx, [FLAGSr]
  cmp dx,100010000000b
  je callJMP_NL
  cmp dx,0
  je callJMP_NL
  call No_JMP_JccNEnd
  jmp end_JccNL
  callJMP_NL:
  call JMPer_JccNEnd
  end_JccNL: 
ret

JccL:
  xor edx,edx
  mov dx, 0000100010000000b 
  and dx, [FLAGSr]
  cmp dx,100010000000b
  je callNOJMP_L
  cmp dx,0
  je callNOJMP_L
  call JMPer_JccEnd
    jmp end_JccL
  callNOJMP_L:
  call No_JMP_JccEnd
  end_JccL:
ret

JccNG:
  xor edx,edx
  mov dx, 1000000b
  and dx, [FLAGSr]
  cmp dx, 0
  jne jmper_jccNEnd
  mov dx, 0000100010000000b 
  and dx, [FLAGSr]
  cmp dx,100010000000b
  je callNOJMP_NG
  cmp dx,0
  je callNOJMP_NG
  jmper_jccNEnd:
  call JMPer_JccNEnd
  jmp end_JccNG
  callNOJMP_NG:
  call No_JMP_JccNEnd
  end_JccNG:
ret

; NEEDS TO BE RECHECKED
JccG:
  xor edx,edx
  mov dx, 1000000b
  and dx, [FLAGSr]
  cmp dx, 0
  jne callNOJMP_G
  mov dx, 0000100010000000b 
  and dx, [FLAGSr]
  cmp dx,100010000000b
  je callJMP_G
  cmp dx,0
  je callJMP_G
  jmp callNOJMP_G
  callJMP_G:
  call JMPer_JccEnd
  jmp end_JccG
  callNOJMP_G:
  call No_JMP_JccEnd 
   end_JccG: 
ret
; here all other Jcc gonna be made


XCHGEbGb:
  call EbGb
  mov dl,byte[edi]
  mov dh, byte[ebx]
  mov byte[edi], dh
  mov byte[ebx],dl
 

  call EbGb_Finish
ret     


XCHGEvGv:
 call EvGv
 xor eax,eax
  mov dx,word[edi]
  mov ax, word[ebx]
  mov word[edi], ax
  mov word[ebx],dx
 

  call EvGv_Finish
ret


XCHGAxGen:
        xor edx,edx
        xor ebx, ebx
        xor ecx,ecx
        mov bl, [esi]
        sub bl, 90h
        shl ebx,1
        mov dx, [GeneralRegs]
        mov cx, [GeneralRegs+ebx]
        mov word[GeneralRegs+ebx], dx
        mov word[GeneralRegs], cx
        inc esi
 ret
 
 PUSHFv:
         sub word[GeneralRegs+8],2
        xor edx,edx
        mov dx, word[FLAGSr]
        xor ebx,ebx
    mov bx, [SegmentRegs+4]
    shr ebx, 4
    add bx, word[GeneralRegs+8]
    add ebx, RAM
    mov word[ebx],dx
   ; sub word[GeneralRegs+8],2
    inc esi
 ret   
 
  POPFv:
    xor ebx,ebx
    xor eax,eax
    mov bx, [SegmentRegs+4]
    mov ax,word[GeneralRegs+8]
    shl ebx, 4 
    add ebx, eax
    add ebx, RAM
    mov dx,word[ebx]
    mov word[FLAGSr],dx
    add word[GeneralRegs+8],2
    inc esi
 ret   
        
 CBW_:
    xor edx,edx
    mov dx, word[GeneralRegs]
    cmp dl, 10000000b
    jae MinusExt_CBW
    mov dh, 0
    jmp Fin_CBW
    MinusExt_CBW:
    mov dh, 0FFh
    Fin_CBW:
    mov word[GeneralRegs], dx
    inc esi
 ret
 
 CWD_:
        xor edx,edx
        mov dx, word[GeneralRegs]
        cmp dh, 10000000b
        jae MinusExt_CWD
        mov word[GeneralRegs+4],0
        jmp Fin_CWD
        MinusExt_CWD:
        mov word[GeneralRegs+4], 0FFFFh
        inc esi
        Fin_CWD:
                
 ret
        
CLC_:
 and word[FLAGSr], 1111111111111110b
ret

STC_:
 or word[FLAGSr], 0000000000000001b
ret

CLI_:
 and word[FLAGSr], 1111110111111111b
ret

STI_:
 or word[FLAGSr], 0000001000000000b
ret

CLO_:
 and word[FLAGSr], 1111011111111111b
ret

STO_:
 or word[FLAGSr], 0000100000000000b
ret

CLD_:
 and word[FLAGSr], 1111101111111111b
ret

STD_:
  or word[FLAGSr], 0000010000000000b
ret

TESTEvGv:
        call EvGv
  cmp byte[esi], 38h
  jne .GvEv
  mov dx,word[edi]
  test dx, word[ebx]
  jmp .flags
  .GvEv:
        mov dx, word[ebx]
        test dx, word[ebx]
        .flags:
          pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
        call EbGb_Finish
ret

TESTEbGb:
        call EbGb
  cmp byte[esi], 38h
  jne .GbEb
  mov dl,byte[edi]
  cmp dl, byte[ebx]
  jmp .flags
  .GbEb:
        mov dl, byte[ebx]
        cmp dl, byte[ebx]
  .flags:
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
        call EbGb_Finish
ret

TESTALIb:
        xor edx,edx
        mov dl, [esi+1]
        mov dh, [GeneralRegs+1]
        cmp dh, dl
    pushf
     pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
        add esi,2
        xor ebx,ebx
    mov bl, byte[addAddr]
    add esi, ebx
    mov byte[addAddr],0 
ret

TESTAXIz:
        xor edx,edx
        xor eax,eax
        mov dx, [esi+1]
        mov ax, [GeneralRegs+1]
        cmp ax, dx
        pushf
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
        add esi, 3
        xor ebx,ebx
    mov bl, byte[addAddr]
    add esi, ebx
    mov byte[addAddr],0
ret


SAHF_:
        xor eax,eax
        mov al, [GeneralRegs]
    and al, 11010111b
    mov byte[FLAGSr+1],al
    inc esi
ret
        
LAHF_:
        xor eax,eax
        mov al, byte[FLAGSr]
        mov [GeneralRegs],al
        inc esi
ret

AAA_:
    xor eax,eax
    mov ax, [GeneralRegs]
    and ax, 0Fh
    cmp ax, 9
    jae ST_AF_CF_AAA
        mov ax, [FLAGSr]
        and ax,00010000b 
        test ax,ax
        jz CL_AF_CF_AAA
        ST_AF_CF_AAA:
        add word[GeneralRegs], 106h
        pushf
        pop ecx
        and word[FLAGSr], 1111111100111011b
        and cx, 0000000011000100b
        or word[FLAGSr], cx
        and byte[GeneralRegs+1], 0Fh
        or word[FLAGSr], 00010001b
        jmp end_AAA
        CL_AF_CF_AAA:
        and word[FLAGSr], 1111111111101110b
        end_AAA:
        inc esi
ret

AAS_:
        xor eax,eax
    mov ax, [GeneralRegs]
    and ax, 0Fh
    cmp ax, 9
    jae ST_AF_CF_AAS
        mov ax, [FLAGSr]
        and ax,00010000b 
        test ax,ax
        jz CL_AF_CF_AAS
        ST_AF_CF_AAS:
    sub word[GeneralRegs],1
    sub word[GeneralRegs+1],6
        or word[FLAGSr], 00010001b
        jmp end_AAS
        CL_AF_CF_AAS:
        and word[FLAGSr], 1111111111101110b
        end_AAS:
        and byte[GeneralRegs+1], 0Fh
        inc esi
ret


nCALLJz:
        xor ebx,ebx
        Push_Stack
        xor edx,edx
        xor ecx,ecx
        ;add esi,3
        mov ecx,esi
        add ecx,3
        sub ecx,RAM
        xor eax,eax
        mov ax, [SegmentRegs+2]
        shl eax,4
        sub ecx,eax
        mov word[ebx+RAM], cx
        mov dx,[esi+1]
        movsx edx,dx
        add esi, edx
        add esi,3
ret

fCALLAp:
        xor ebx,ebx
        xor ecx,ecx ; stores the address of segment where to jmp
        mov ecx,[esi+1]; first 16 bits- segment; second- offset
        rol ecx,16 ;getting higher bits in the cx
        sub esi, RAM
        mov bx, [SegmentRegs+2]
        shl ebx,4
        sub si, word[ebx]
        Push_Stack
        mov word[ebx], si
        xor esi,esi
        mov si, cx
        mov word[SegmentRegs+2],cx
        xor cx,cx ; clearing segment
        shr ecx,16
        shl esi,4
        add esi, RAM
        add esi, ecx
        add esi,5
ret

IRET_:
        xor edx,edx
        xor ebx,ebx
        xor ecx,ecx
        mov bx, [SegmentRegs+4]
        shl ebx,4
        mov cx, [GeneralRegs+8] ;SP
        add ebx,RAM
        add ebx,ecx
        mov cx, [ebx+2]
        mov word[SegmentRegs+2],cx
        shl ecx,4
        add ebx,2
        mov dx,[ebx-2]
    add ecx,edx
    add ecx,RAM
    mov esi,ecx
    
    mov cx, [ebx]
        mov word[FLAGSr],cx
        add ebx,2
    add word[GeneralRegs+8],6
  ;  add esi,2
ret


RETne:
        xor ebx,ebx
        xor ecx,ecx
        xor edx,edx
        cmp byte[esi], 0C3h
        je Read_Locals_RETne 
        mov cx, [esi+1]
        Read_Locals_RETne :
        push 0
        Pushed_RETne:
        Pop_Stack
        push ecx
        xor ecx,ecx
        add ebx,RAM
        mov cx, [ebx]
        xor ebx,ebx
        mov bx, [SegmentRegs+2]
        shl ebx,4
        add ebx,RAM
        add ebx, ecx
        pop ecx
 ;       add word[GeneralRegs+8],2
        add word[GeneralRegs+8],cx
        pop esi
;       cmp esi,0
        ;je .noAdd
;       add esi,2
;       .noAdd:
        ;add [GeneralRegs+8],si
        ;xor ebx,ebx
        ;mov bx, word[ebx]
;       movsx bx,bl
        mov esi,ebx
ret     
        
fRET_:
        xor ebx,ebx
        cmp byte[esi+1], 0CBh
        je Read_Locals_fRET 
        mov bx, [esi+1]
        add bx,2
        push ebx ;need to reconsider this part
        jmp Pushed_fRET
        Read_Locals_fRET:
        xor ebx,ebx ; this dumb xor supposed to solve the IRET calling this
        push 0
        Pushed_fRET:
        add word[GeneralRegs+8],bx
    Pop_Stack
        xor esi, esi
        mov si, word[ebx]
        mov word[SegmentRegs+2],si
        shl esi,4
    Pop_Stack
        xor eax,eax
        mov ax, word[ebx]
        add esi, eax
        add esi, RAM    
        pop ebx
        ;cmp  ebx, 0
        ;je noAdd_fRET
        ;add esi,2
        ;noAdd_fRET:
        add esi,ebx
        inc esi
ret     

LOOPJb:
        inc esi
        sub word[GeneralRegs+2],1
        cmp word[GeneralRegs+2],0
        je LoopFin_LOOPJb
        xor eax,eax
        mov ebx,esi
        sub ebx, RAM
        mov al, [esi]
        movsx eax,al
        add ebx,eax
        add ebx,RAM
    mov esi,ebx
    inc esi
    ;add esi, RAM
    jmp end_LOOPJb
    LoopFin_LOOPJb:
    inc esi
    end_LOOPJb:
ret
; may be should rather do all loops in just 1 procedure
LOOPEJb: ; need to work with flags changed by LOOP
        add esi,2
        sub word[GeneralRegs+2],1
        cmp word[GeneralRegs+2],0
        je LoopZF_Check_LOOPEJb
        LoopAgain_LOOPEJb:
xor eax,eax
        mov ebx,esi
        sub ebx, RAM
        mov al, [esi]
        movsx eax,al
        add ebx,eax
        add ebx,RAM
    mov esi,ebx
    ;inc esi
    jmp end_LOOPEJb
    LoopZF_Check_LOOPEJb:
    xor ebx,ebx
    mov bx, word[FLAGSr]
    test bx, 0000000001000000b  
    jz LoopAgain_LOOPEJb   
    
    end_LOOPEJb:
ret

LOOPNEJb:
        add esi,2
        sub word[GeneralRegs+2],1
        cmp word[GeneralRegs+2],0
        jne LoopZF_Check_LOOPNEJb
        LoopAgain_LOOPNEJb:
    xor eax,eax
        mov ebx,esi
        sub ebx, RAM
        mov al, [esi]
        movsx eax,al
        add ebx,eax
        add ebx,RAM
    mov esi,ebx
   ; inc esi
    jmp end_LOOPNEJb
    LoopZF_Check_LOOPNEJb:
    xor ebx,ebx
    mov bx, word[FLAGSr]
    test bx, 0000000001000000b  
    jz LoopAgain_LOOPNEJb
    ;inc esi
    end_LOOPNEJb:
ret

STOSYbAL:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov al, [GeneralRegs]
        mov byte[ebx],al
;    add word[GeneralRegs+14],1
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jz Dir_Plus_STOSYbAL
        add word[GeneralRegs+14],1
        jmp ender_STOSYbAL
        Dir_Plus_STOSYbAL:
        sub word[GeneralRegs+14],1      
        ender_STOSYbAL:
ret     

STOSYwAX:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov ax, [GeneralRegs]
        mov word[ebx],ax
        ;add [GeneralRegs+14],2
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jz Dir_Plus_STOSYwAX
        add word[GeneralRegs+14],2
        jmp ender_STOSYwAX
        Dir_Plus_STOSYwAX:
        sub word[GeneralRegs+14],2      
        ender_STOSYwAX:
ret


 LODSAlXb:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov al, byte[ebx]
        mov [GeneralRegs],al
;   add [GeneralRegs+14],1
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jz Dir_Plus_LODSAlXb
        add word[GeneralRegs+14],1
        jmp ender_LODSAlXb
        Dir_Plus_LODSAlXb:
        sub word[GeneralRegs+14],1      
        ender_LODSAlXb:
ret     
 
LODSAXXw:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov ax, word[ebx]
        mov [GeneralRegs],ax
;   add [GeneralRegs+14],2
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jz Dir_Plus_LODSAXXv
        add word[GeneralRegs+14],2
        jmp ender_LODSAXXv
        Dir_Plus_LODSAXXv:
        sub word[GeneralRegs+14],2      
        ender_LODSAXXv:
ret             

SCASALYb:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov al, [GeneralRegs]
        cmp al, byte[ebx]
        pushf
        pop ebx
        and word[FLAGSr],1111011100101010b
        and ebx, 0000100011010101b
        or word[FLAGSr], bx
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jz Dir_Plus_SCASALYb
        add word[GeneralRegs+14],1
        jmp ender_SCASALYb
        Dir_Plus_SCASALYb:
        sub word[GeneralRegs+14],1      
        ender_SCASALYb:
        
ret     

SCASAXYv:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov ax, [GeneralRegs]
        cmp ax, word[ebx]
        pushf
        pop ebx
        and word[FLAGSr],1111011100101010b
        and ebx, 0000100011010101b
        or word[FLAGSr], bx
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jnz Dir_Plus_SCASAXYv
        add word[GeneralRegs+14],2
        jmp ender_SCASAXYv
        Dir_Plus_SCASAXYv:
        sub word[GeneralRegs+14],2      
        ender_SCASAXYv:
ret     
        
 MOV_L_Ib:
        xor ebx,ebx
        xor eax,eax
        mov bl, [esi]
        sub bl, 0B0h
        shl bl, 1
    mov al, [esi+1]
    mov [GeneralRegs+ebx], al
    add esi,2
ret     

 MOV_H_Ib:
        xor ebx,ebx
        xor eax,eax
        mov bl, [esi]
        sub bl, 0B4h
        shl bl, 1
    mov al, [esi+1]
    mov [GeneralRegs+1+ebx], al
    add esi,2
ret     

MOV_W_Iv:
        xor ebx,ebx
        xor eax,eax
        mov bl, [esi]
        sub bl, 0B8h
        shl bl, 1
    mov ax, [esi+1]
    mov [GeneralRegs+ebx], ax
    add esi,3
ret

MOVEbIb: ;here needs to be added the address of Eb analyser
       mov [prevESP_],esp
       ; call EbGb
        xor ebx,ebx
        mov bl, [esi+1]
        and bl, 00111000b
        test bl,bl
        jnz err_MOVEbIb
        xor ecx,ecx
        mov bl,[esi+1]
    mov cl,bl
    and bl, 11000000b
    and cl, 111b
    shr bl,2
    add bl,cl
    shl ebx,2
    mov ebx,[ebx+MODR_M_VALUES]
    call ebx
    xor ebx,ebx
    mov bl, byte[addAddr]
    add esi, ebx
    mov byte[addAddr],0
  ;  add esp,12
    mov bl, [esi+1]
    mov byte[edi],bl
    add esi,2
    err_MOVEbIb: ; here is gonna be call of processor int
 ;   mov esp, [prevESP_]
ret
 
MOVEvIz:
            mov [prevESP_],esp
        call EvGv
        xor ebx,ebx
        mov bl, [esi+1]
        and bl, 00111000b
        test bl,bl
        jnz err_MOVEvIz
        xor ecx,ecx
        mov bl,[esi+1]
    mov cl,bl
    and bl, 11000000b
    and cl, 111b
    shr bl,2
    add bl,cl
    shl ebx,2
    mov ebx,[ebx+MODR_M_VALUES]
    call ebx
    xor ebx,ebx
    mov bl, byte[addAddr]
    add esi, ebx
    mov byte[addAddr],0
   ; add esp,12
    mov bx, [esi+2]
    mov word[edi],bx
    add esi,4
    err_MOVEvIz:
    mov esp, [prevESP_]
ret
   
MOVEbGb:
        call EbGb
        cmp byte[esi],88h
        ja GbEb_MOVEbGb
        mov dl, byte[ebx] 
        mov [edi], dl
        jmp finish_MOVEbGb
        GbEb_MOVEbGb:
        mov dl, byte[edi] 
        mov [ebx], dl
        finish_MOVEbGb:
        call EbGb_Finish
        ; call ZF
        ;call SF
        ;call OF
        ;call CF
        ;  add esp,4
ret

MOVEvGv:
        call EvGv
        cmp byte[esi],89h
        ja GvEv_MOVEvGv
        mov dx, word[ebx] 
        mov word[edi], dx
        jmp finish_MOVEvGv
        GvEv_MOVEvGv:
        mov dx, word[edi] 
        mov word[ebx], dx
        finish_MOVEvGv:
        call EvGv_Finish
        ; call ZF
        ;call SF
        ;call OF
        ;call CF
        ;  add esp,4
ret

 MOVALOb:
    xor eax,eax
    xor ebx,ebx
    xor ecx,ecx
    mov bx,word[SegmentRegs+6]
    shl ebx,4
    mov cx,[esi+1]
        add ebx,ecx
        add ebx,RAM
        cmp byte[esi],0A0h
        je ALOb_MOVALOb
        mov al,[GeneralRegs]
        mov [ebx],al
        jmp ender_MOVALOb
        ALOb_MOVALOb:
        mov al, [ebx]
        mov [GeneralRegs],al
        ender_MOVALOb:
        add esi,3
 ret
 
  MOVAXOv:
    xor eax,eax
        xor ebx,ebx
        xor ecx,ecx
        mov bx,word[SegmentRegs+6]
        shl ebx,4
        mov cx,[esi+1]
        add ebx,ecx
        add ebx,RAM
        cmp byte[esi],0A1h
        je AXOv_MOVAXOv
        mov ax,[GeneralRegs]
        mov [ebx],ax
        jmp ender_MOVAXOv
        AXOv_MOVAXOv:
        mov ax, [ebx]
        mov [GeneralRegs],ax
        ender_MOVAXOv:
        add esi,4
 ret
 
  MOVEvSw:
    call EvGv
    xor ebx, ebx
    mov bl, [esi+1]
    and bl, 00111000b
    shr ebx,2
    add ebx, SegmentRegs
    cmp byte[esi+1],8Ch
    ja MOVSwEv
    mov ax, [ebx]
    mov [edi],ax
    jmp MOVEvSw_end
    MOVSwEv:
    mov ax, [edi]
    mov [ebx],ax    
        MOVEvSw_end:
        add esi,2
        xor edx,edx
        mov dl, [addAddr]
        add esi, edx
        mov byte[addAddr],0
ret             
        

 CMPSYbXb:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov al, [ebx]
        xor ebx,ebx
        mov bx, [SegmentRegs+6]
    shl ebx,4
        add bx, [GeneralRegs+14]
        add ebx, RAM
        mov ah, [ebx]
        cmp ah,al
        pushf
        pop ebx
        and word[FLAGSr],1111011100101010b
        and ebx, 0000100011010101b
        or word[FLAGSr], bx
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jnz Dir_Plus_CMPSYbXb
        add word[GeneralRegs+12],1
        add word[GeneralRegs+14],1
        jmp ender_CMPSYbXb
        Dir_Plus_CMPSYbXb:
        sub word[GeneralRegs+12],1
        sub word[GeneralRegs+14],1      
        ender_CMPSYbXb:
ret     

 CMPSYvXv:
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov ax, [ebx]
        xor ebx,ebx
        mov bx, [SegmentRegs+6]
        shl ebx,4
        add bx, [GeneralRegs+14]
        add ebx, RAM
        mov dx, [ebx]
        cmp dx,ax
        pushf
        pop ebx
        and word[FLAGSr],1111011100101010b
        and ebx, 0000100011010101b
        or word[FLAGSr], bx
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jnz Dir_Plus_CMPSYvXv
        add word[GeneralRegs+12],2
        add word[GeneralRegs+14],2
        jmp ender_CMPSYvXv
        Dir_Plus_CMPSYvXv:
        sub word[GeneralRegs+12],2
        sub word[GeneralRegs+14],2      
        ender_CMPSYvXv:
ret     
        
 
MOVSYbXb: 
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov al, [ebx]
        xor ebx,ebx
        mov bx, [SegmentRegs+6]
        shl ebx,4
        add bx, [GeneralRegs+14]
        add ebx, RAM
        mov [ebx],al
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jnz Dir_Plus_MOVSYbXb
        add word[GeneralRegs+12],1
        add word[GeneralRegs+14],1
        jmp ender_MOVSYbXb
        Dir_Plus_MOVSYbXb:
        sub word[GeneralRegs+12],1
        sub word[GeneralRegs+14],1      
        ender_MOVSYbXb:
ret     

MOVSYvXv: 
        xor ebx,ebx
        xor eax,eax
        mov bx, word[SegmentRegs]
        shl ebx,4
        mov ax,word[GeneralRegs+14]
        add ebx,eax
        add ebx, RAM
        xor eax,eax
        mov ax, [ebx]
        xor ebx,ebx
        mov bx, [SegmentRegs+6]
        shl ebx,4
        add bx, [GeneralRegs+14]
        add ebx, RAM
        mov [ebx],ax
        inc esi
        mov ax, word[FLAGSr]
        and ax, 0000010000000000b
        test ax,ax
        jnz Dir_Plus_MOVSYvXv
        add word[GeneralRegs+12],2
        add word[GeneralRegs+14],2
        jmp ender_MOVSYvXv
        Dir_Plus_MOVSYvXv:
        sub word[GeneralRegs+12],2
        sub word[GeneralRegs+14],2      
        ender_MOVSYvXv:
ret     


 REPE_:
        xor ecx,ecx
        mov cx, [GeneralRegs+2]
        Loop_REPE:
        xor ebx,ebx
        mov bl, [esi+1]
    mov eax, [OPCODES+ebx*4]
    jmp eax
    mov ax,word[FLAGSr]
    and ax, 0000000001000000b
    test ax,ax
    jz endLoop_REPE
    loop Loop_REPE
    endLoop_REPE:
    mov [GeneralRegs+2], cx
    add esi,2
 ret
 
  REPNE_:
        xor ecx,ecx
        mov cx, [GeneralRegs+2]
        Loop_REPNE:
        xor ebx,ebx
        mov bl, [esi+1]
    mov eax, [OPCODES+ebx*4]
    jmp eax
    mov ax,word[FLAGSr]
    and ax, 0000000001000000b
    test ax,ax
    jnz endLoop_REPNE
    loop Loop_REPNE
    endLoop_REPNE:
    mov [GeneralRegs+2], cx
    add esi,2
 ret
 
 INTIb:
; xot eax,eax
; mov al, [esi+1]
; shl eax,4
 push ecx
 xor ecx,ecx
 mov cl, [esi+1]
 mov [NumInt],cl
 pop  ecx
 push esi       
 sub esi, RAM
 xor edx,edx
 mov dx, [SegmentRegs+2] ; CS
 shl edx,4
 sub esi,edx
 add esi,2 ; length of instruction
 xor ebx,ebx
 mov bx,[SegmentRegs+4] ; SS
 shl ebx,4
 add bx, [GeneralRegs+8] ;SP
 add ebx,RAM
 sub ebx,2
 push ecx
 mov cx, [FLAGSr]
 mov word[ebx],cx
 pop ecx
 shr edx,4 ;CS on stack
 sub ebx,2
 mov word[ebx],dx
 sub ebx,2
 mov word[ebx],si
 sub word[GeneralRegs+8],6
 ;and [FLAGSr],
 pop esi
 xor edx,edx
 mov dl,[esi+1]
 mov esi, RAM
 shl edx,2
 add esi,edx
 xor ebx,ebx
 xor ecx,ecx
 mov bx, [esi]
 mov cx, [esi+2]
 xor esi,esi
 shl ecx,4
 add esi, ebx
 add esi,ecx
 add esi, RAM
 ret

 INTO_:
 ret

INT3_:
ret

LESGzMp:
;       inc esi
 ret
        
FPU:
;       add esi,2
ret
                
SEGES: ; may also be a macros
        xor eax,eax
        mov ax,[SegmentRegs+6]
        push eax
        mov ax,[SegmentRegs]
        mov word[SegmentRegs+6],ax
        inc esi
        push Ret_ES
        jmp esi
        Ret_ES:
        pop eax
        mov [SegmentRegs+6], ax
                
   ;inc esi
ret

SEGSS:
   ;inc esi
ret

SEGSs:
  ; inc esi
ret

SEGFS:
  ; inc esi
ret

SEGCS:
  ; inc esi
ret

SEGDS:
  ; inc esi
ret

SEGGS:
  ; inc esi
ret

INALDX:
ret

INAXDX:
ret

INALIb:
        ;add esi,2
ret

INAXIv:
        ;add esi,3
ret

 OUTIbAL:
 ;      add esi,2
ret

 OUTIbAX:
 ;      add esi,3
ret
        
 ENTERIwIb:
 ;      inc esi
 ret
                
 LEAVE_:
 ;      inc esi
 ret
  
 SHIFTGR:
   ;inc esi
 ret
         
DIV_:
        ;inc esi
ret

JrCXZJb:
        ;inc esi
ret

OPSIZE:
  ;inc esi
ret

ADDRSIZE:
  ;inc esi
ret

imGroup1:
xor ebx,ebx
mov bl, [esi]
sub bl,80h
cmp bl,0
ja CMP1_imGroup1
No_REG 0
call imGroup1INSTREbIb
ret
CMP1_imGroup1:
cmp bl,1
ja CMP3_imGroup1
No_REGW 
 call imGroup1INSTREvIz
ret
CMP3_imGroup1:
No_REGW 
  call imGroup1INSTREvIb
ret

 imGroup1INSTREvIb:
 xor ecx,ecx
 mov cl, [esi+1]
 and cl, 00111000b
 shr cl,3
 xor eax,eax
 mov al,[esi+2]
 add esi,3
 cmp cl,0
 ja CMP1_imGroup1INSTREv
 add word[ebx],ax
 jmp end_imGroup1INSTR
 CMP1_imGroup1INSTREv:
 cmp cl,1
 ja CMP2_imGroup1INSTREv
 or word[ebx],ax
 jmp end_imGroup1INSTR
 CMP2_imGroup1INSTREv:
 cmp cl,2
 ja CMP3_imGroup1INSTREv
 mov cx, [FLAGSr]
 and cx,1
 add ax,cx
 or word[ebx],ax
 jmp end_imGroup1INSTR
 CMP3_imGroup1INSTREv:
 cmp cl,3
 ja CMP4_imGroup1INSTREv
 mov cx, [FLAGSr]
 and cx,1
 add ax,cx
 sbb word[ebx],ax
 ret
 CMP4_imGroup1INSTREv:  
 cmp cl,4
 ja CMP5_imGroup1INSTREv
 and word[ebx],ax
 jmp end_imGroup1INSTR
 CMP5_imGroup1INSTREv:
 cmp cl,5
 ja CMP6_imGroup1INSTREv
 sub word[ebx],ax
 jmp end_imGroup1INSTR
 CMP6_imGroup1INSTREv:
 cmp cl,6
 ja CMP7_imGroup1INSTREv
 xor word[ebx],ax
 jmp end_imGroup1INSTR
 CMP7_imGroup1INSTREv:
 cmp word[ebx],ax
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
ret                     


imGroup1INSTREbIb:
 xor ecx,ecx
 mov cl, [esi+1]
 and cl, 00111000b
 shr cl,3
 mov al, [esi+2]
 add esi,3
 cmp cl,0
 ja CMP1_imGroup1INSTREb
 add byte[ebx],al
 jmp end_imGroup1INSTR
 CMP1_imGroup1INSTREb:
 cmp cl,1
 ja CMP2_imGroup1INSTREb
 or byte[ebx],al
 jmp end_imGroup1INSTR
 CMP2_imGroup1INSTREb:
 cmp cl,2
 ja CMP3_imGroup1INSTREb
 mov cl, [FLAGSr]
 and cl,1
 add al,cl
 or byte[ebx],al
 jmp end_imGroup1INSTR
 CMP3_imGroup1INSTREb:
 cmp cl,3
 ja CMP4_imGroup1INSTREb
 mov cl, [FLAGSr]
 and cl,1
 add al,cl
 sbb byte[ebx],al
 ret
 CMP4_imGroup1INSTREb:  
 cmp cl,4
 ja CMP5_imGroup1INSTREb
 and byte[ebx],al
 jmp end_imGroup1INSTR
 CMP5_imGroup1INSTREb:
 cmp cl,5
 ja CMP6_imGroup1INSTREb
 sub byte[ebx],al
 jmp end_imGroup1INSTR
 CMP6_imGroup1INSTREb:
 cmp cl,6
 ja CMP7_imGroup1INSTREb
 xor byte[ebx],al
 jmp end_imGroup1INSTR
 CMP7_imGroup1INSTREb:
 cmp byte[ebx],al
 end_imGroup1INSTR:
   pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
ret             

imGroup1INSTREvIz:
 xor ecx,ecx
 mov cl, [esi+1]
 and cl, 00111000b
 shr cl,3
 mov ax, [esi+2]
 add esi,4
 cmp cl,0
 ja CMP1_imGroup1INSTR
 add word[ebx],ax
 jmp end_imGroup1INSTR
 CMP1_imGroup1INSTR:
 cmp cl,1
 ja CMP2_imGroup1INSTR
 or word[ebx],ax
 jmp end_imGroup1INSTR
 CMP2_imGroup1INSTR:
 cmp cl,2
 ja CMP3_imGroup1INSTR
 mov cx, [FLAGSr]
 and cx,1
 add ax,cx
 or word[ebx],ax
 jmp end_imGroup1INSTR
 CMP3_imGroup1INSTR:
 cmp cl,3
 ja CMP4_imGroup1INSTR
 mov cx, [FLAGSr]
 and cx,1
 add ax,cx
 sbb word[ebx],ax
 ret
 CMP4_imGroup1INSTR:    
 cmp cl,4
 ja CMP5_imGroup1INSTR
 and word[ebx],ax
 jmp end_imGroup1INSTR
 CMP5_imGroup1INSTR:
 cmp cl,5
 ja CMP6_imGroup1INSTR
 sub word[ebx],ax
 jmp end_imGroup1INSTR
 CMP6_imGroup1INSTR:
 cmp cl,6
 ja CMP7_imGroup1INSTR
 xor word[ebx],ax
 jmp end_imGroup1INSTR
 CMP7_imGroup1INSTR:
 cmp word[ebx],ax
  pushf
   pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
ret     
 
        
 
 

imGrop1A:
        
ret

imGroup3B:
;copy from FE_


ret

imGroup3W:

ret

MULvv_:
        

IMULGvEvIz:
  call EvGv
  mov ax, word[esi+2]  
  mov dx,word[edi]      
  imul dx
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [ebx], ax
  add esi,4
ret

IMULGbEbIb:
  call EvGv
  mov al, byte[esi+2]
  mov dx,word[edi]
        
  imul dx 
  pushf
  pop ecx
  and word[FLAGSr], 1111011100101010b ; clear neccessary
  and cx, 0000100011010101b ;neccessary flags 
  or word[FLAGSr], cx 
  
  mov [ebx], ax
  add esi,3
ret

MULb:
;       xor ebx,ebx
   ; xor ecx,ecx
        ;mov cl, [esi+1]
        ;add esi,2
        ;sub cl, 0F6h
        No_REG 0
    mov bl, byte[ebx]
    mov al,byte[GeneralRegs]
    mul bl
    mov [GeneralRegs],ax
    add esi,2
ret

MULw:
;       xor ebx,ebx
   ; xor ecx,ecx
        ;mov cl, [esi+1]
        ;add esi,2
        ;sub cl, 0F6h
        No_REG 1 
    mov bx, word[ebx]
    mov ax,word[GeneralRegs]
    mul bx
    mov [GeneralRegs],ax
    shr eax,16
    mov [GeneralRegs+4],ax
    add esi,2
ret

IMULb:
;       xor ebx,ebx
   ; xor ecx,ecx
        ;mov cl, [esi+1]
        ;add esi,2
        ;sub cl, 0F6h
        No_REG 0
    mov bl, byte[ebx]
    mov al,byte[GeneralRegs]
    imul bl
    mov [GeneralRegs],ax
    add esi,2
ret

IMULw:
;       xor ebx,ebx
   ; xor ecx,ecx
        ;mov cl, [esi+1]
        ;add esi,2
        ;sub cl, 0F6h
        No_REG 1 
    mov bx, word[ebx]
    mov ax,word[GeneralRegs]
    imul bx
    mov [GeneralRegs],ax
    shr eax,16
    mov [GeneralRegs+4],ax
    add esi,2
ret

NOTB:
        No_REG 0
        xor ecx,ecx
        mov cl, [ebx]
        not cl
        mov [ebx],cl
        add esi,2
ret     

NOTW:
        No_REG 0
        xor ecx,ecx
        mov cx, [ebx]
        not cx
        mov [ebx],cx
        inc esi
        
ret

NEGB:
        No_REG 0
        xor ecx,ecx
        mov cl, [ebx]
        neg cl
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx 
        mov [ebx],cl
        inc esi
ret     

NEGW:
        No_REG 1
        xor ecx,ecx
        mov cx, [ebx]
        neg cx
    pushf
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx 
        mov [ebx],cx
        inc esi
ret

MULAL:
        mov al,[GeneralRegs]
        mov dl, [ebx]
        mul dl
        mov word[GeneralRegs],ax
    pushf
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx 
    add esi,2
    xor ecx,ecx
        mov cl, [addAddr]
        add esi,ecx
        mov byte[addAddr],0
ret

MULAX:
        mov ax,[GeneralRegs]
        mov cx, [ebx]
        mul cx
        pushf
    pop ecx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and cx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], cx 
        
        
        mov word[GeneralRegs],ax
        mov word[GeneralRegs+4],dx
    add esi,2
        xor ecx,ecx
        mov cl, [addAddr]
        add esi,ecx
        mov byte[addAddr],0
ret

DIVAL:
        push ecx
        xor edx,edx
        mov ax,[GeneralRegs]
        mov cl,[ebx]    
        cmp cl,0
        je ZeroDiv
        div cl
        mov word[GeneralRegs],ax        
        pushf
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx 
    add esi,2
    xor ecx,ecx
        mov cl, [addAddr]
        add esi,ecx
        mov byte[addAddr],0
    pop ecx
ret

DIVAX:
        push ecx
        xor ecx,ecx
        xor edx,edx
        mov ax,[GeneralRegs+4]
        shl eax,16
        mov ax,[GeneralRegs]
        mov cx,[ebx]
        cmp cx,0
        je ZeroDiv
        div cx
        pushf
    pop ecx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and cx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], cx 
        
        mov word[GeneralRegs+4],dx
        mov word[GeneralRegs], ax
        add esi,2
        xor ecx,ecx
        mov cl, [addAddr]
        add esi,ecx
        mov byte[addAddr],0
        pop ecx    
ret


ZeroDiv:
        
ret
            
DAA_:
        
ret
        
DAS_:
        
        
        
        
        
        ;part with DAS flags effected
        pushf
        pop ecx
        and word[FLAGSr], 1111111100111011b
        and cx, 0000000011000100b
        or word[FLAGSr], cx
        inc esi
ret

 OUTDXAL:
        
 ret


  OUTDXAX:
        
 ret

LDSGzMp:

ret

BOUNDGvMa:

ret

ARPLEwGw:

ret

INSYb:

ret

INSYz:

ret

OUTSXb:

ret

OUTSXz:

ret

LEAGvM:
ret

fWAIT_:
        xor ebx,ebx
        mov bl, [NumInt]
        shl ebx,2
        add ebx, intTable
        mov ebx,[ebx]
        call ebx
ret


AAMIb:
ret

AADIb:
ret

XLAT_:
ret

HLT_:
ret

CMC_:
ret

F6_:
        xor ecx,ecx
        mov cl, [esi]
        ;add esi,2
        sub cl, 0F6h
        No_REG ecx
        cmp byte[addAddr],0
        je .noAddOffset
        push ecx
        xor ecx,ecx
        mov cx, [esi+2]
        add ebx,ecx
        add edi,ecx
        pop ecx
        .noAddOffset:
        xor eax,eax
    mov al,[esi+1]
   ; add esi,2
    and al, 111000b
    shr al, 3
    cmp al,0
    jne CMP2_FE
    cmp cl,0
    jne TESTIz_FE
    call TESTIb
    jmp ret_F6
    TESTIz_FE:
    call TESTIz
    jmp ret_F6


        CMP2_FE:
        cmp al,2
        jne  CMP3_FE
        cmp cl,0
        jne NOTW_FE
        call NOTB
        jmp ret_F6
    NOTW_FE:    
    call NOTW
    jmp ret_F6

        CMP3_FE:
        cmp al,3
        jne  CMP4_FE    
        cmp cl,0
        jne NEGW_FE
        call NEGB
        jmp ret_F6
        NEGW_FE:
    call NEGW
    jmp ret_F6

        CMP4_FE:
        cmp al,4
        jne  CMP5_FE    
        cmp cl,0
        jne MULAX_FE
        call MULAL
        jmp ret_F6 
        MULAX_FE: 
    call MULAX
    jmp ret_F6  
        
        CMP5_FE:
        cmp cl,5
        jne  CMP6_FE    
        cmp cl,0
        jne IMULAX_FE
        call MULAL
        jmp ret_F6
        IMULAX_FE: 
    call MULAX
    jmp ret_F6  
        
        CMP6_FE:
        cmp al,6
        jne  CMP7_FE
        cmp cl,0
        jne DIVAX_FE
        call DIVAL
        jmp ret_F6 
    DIVAX_FE:
    call DIVAX
    jmp ret_F6  
        
        CMP7_FE:
        cmp cl,0
        jne IDIVAX_FE
        call DIVAL
        IDIVAX_FE:
        call DIVAX
    jmp ret_F6
        ret_F6:
  ret

TESTIz:

    mov cx,[ebx]
    add esi,2
    test cx, word[esi-2]
    pushf
    pop edi
    and word[FLAGSr], 1111011100101011b ; clear neccessary
    and di, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], di 
ret 

TESTIb:

    mov cl,[ebx]
    add esi,2
    test cx, word[esi-2]
    pushf
    pop edi
    and word[FLAGSr], 1111011100101011b ; clear neccessary
    and di, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], di 
ret


FE_:
        No_REG 0
    xor eax,eax
    mov al,[esi+1]
    add esi,2
    and al, 111000b
    shr al, 3
    cmp al,0
    jne DEC_FE_
    inc byte[ebx]
    jmp FLAGS_FE
    DEC_FE_:
    dec byte[ebx] 
    FLAGS_FE:   
    pushf
    pop edi
    and word[FLAGSr], 1111011100101011b ; clear neccessary
    and di, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], di           
ret


        

FF_:
    No_REG 1
    xor eax,eax
    mov al,[esi+1]
    add esi,2
    and al, 111000b
    shr al, 3
    cmp al,0
    jne DEC_FF_
    inc word[ebx]
    jmp FLAGS_FF
    cmp al,1 
    DEC_FF_:
    dec word[ebx]
    FLAGS_FF:   
    pushf
    pop edi
    and word[FLAGSr], 1111011100101011b ; clear neccessary
    and di, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], di
ret


  
 SALB:
 cmp cl,0 
 je notHing_SALW
 No_REG 0
 xor eax,eax
 and cl, 111000b
 shr cl,3
 cmp cl,0
 jne  cmp1_SALB
 rol byte[ebx],cl
 jmp ender_SALB
 cmp1_SALB:
 cmp al,1
 jne  cmp2_SALB
 ror byte[ebx],cl
 jmp ender_SALB
 cmp2_SALB:
 cmp al,2      
 jne cmp3_SALB
 rcl byte[ebx],cl
 jmp ender_SALB
 cmp3_SALB:
 cmp al,3
 jne cmp4_SALB
 rcr byte[ebx],cl
 jmp ender_SALB
  cmp4_SALB:
 cmp al,4
 jne cmp5_SALB
 shl byte[ebx],cl
 jmp ender_SALB
 cmp5_SALB:
 cmp al,5
 jne SALB7
 shr byte[ebx],cl
 jmp ender_SALB
 SALB7:
 sar byte[ebx],cl
 ;cmp al,7
 ender_SALB:
  pushf
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx
 notHing_SALB:  
    add esi,2 
 ret
 
 
  SALW:
  cmp cl,0 
  je notHing_SALW
 No_REG 1
 xor eax,eax
 and cl, 111000b
 shr cl,3
 cmp cl,0
 jne  cmp1_SALW
 rol word[ebx],cl
 jmp ender_SALW
 cmp1_SALW:
 cmp al,1
 jne  cmp2_SALW
 ror word[ebx],cl
 jmp ender_SALW
 cmp2_SALW:
 cmp al,2      
 jne cmp3_SALW
 rcl word[ebx],cl
 jmp ender_SALW
 cmp3_SALW:
 cmp al,3
 jne cmp4_SALW
 rcr word[ebx],cl
 jmp ender_SALW
  cmp4_SALW:
 cmp al,4
 jne cmp5_SALW
 shl word[ebx],cl
 jmp ender_SALW
 cmp5_SALW:
 cmp al,5
 jne SALW7
 shr word[ebx],cl
 jmp ender_SALW
 SALW7:
 sar word[ebx],cl
 ;cmp al,7
 ender_SALW:
  pushf
    pop edx
    and word[FLAGSr], 1111011100101010b ; clear neccessary
    and dx, 0000100011010101b ;neccessary flags 
    or word[FLAGSr], dx 
 notHing_SALW:  
    add esi,2
 ret
 
        
 SAL16_8:
  xor ecx, ecx
  mov cl, [esi+1]
  inc esi 
  call SALW
  ret
  
SAL8_8:
  xor ecx, ecx
  mov cl, [esi+1]
  inc esi
  call SALB
  ret
  
SAL8_CL:
  xor ecx, ecx
  mov cl, [GeneralRegs+2]
  call SALB
  ret  
  
  
SAL16_CL:
  xor ecx, ecx
  mov cl, [GeneralRegs+2]
  call SALW
ret

SAL16_:
    mov ecx,1
    call SALW
ret

  SAL8_:
    mov ecx,1
    call SALB
ret

 Shifts:
        xor ebx,ebx
        mov bl, [esi]
        and bl, 11111110b
        cmp bl, 0C0h
        ja cmpD0_Shifts
        cmp bl, [esi]
        jne C1_Shifts
        call SAL8_8
        jmp ender_Shifts
        C1_Shifts:
        call SAL16_8    
        jmp ender_Shifts
        cmpD0_Shifts:
        cmp bl, 0D0h
        ja cmpD2_Shifts
        cmp bl, [esi]
        jne D1_Shifts
        call SAL8_
        jmp ender_Shifts
        D1_Shifts:
        call SAL16_
        jmp ender_Shifts
        cmpD2_Shifts:
        cmp bl, [esi]
        call SAL8_CL
        jmp ender_Shifts
        call SAL16_CL
        ender_Shifts:
 ret

ARPL_:
;  inc esi
  ret
        


; ---S E C T I O N  O F  V A R I A B L E S---
.data:
                                                                                  
;                  0        1        2            3         4         5         6          7           8          9          A          B            C        D          E         F
  OPCODES: dd     ADDEbGb,  ADDEvGv,  ADDEbGb,   ADDEvGv,   ADDALIb,  ADDAXIz,  PUSHEs,   POPEs,     OREbGb,    OREvGv,     OREbGb,    OREvGv,      ORALIb,   ORAXIz,    PUSHCs,  TWOB, \
                  ADCEbGb,  ADCEvGv,  ADCEbGb,   ADCEvGv,   ADCALIb,  ADCAXIz,  PUSHSs,   POPSs,     SBBEbGb,   SBBEvGv,    SBBEbGb,   SBBEvGv,   SBBALIb,  SBBAXIz,   PUSHDs,   POPDs, \
                  ANDEbGb,  ANDEvGv,  ANDEbGb,   ANDEvGv,   ANDALIb,  ANDAXIz,  SEGES,    DAA_,      SUBEbGb,   SUBEvGv,    SUBEbGb,   SUBEvGv,     SUBALIb,  SUBAXIz,    SEGCS,    DAS_, \
                  XOREbGb,  XOREvGv,  XOREbGb,   XOREvGv,   XORALIb,  XORAXIz,  SEGSS,    AAA_,      CMPEbGb,   CMPEvGv,    CMPEbGb,   CMPEvGv,    CMPALIb,  CMPAXIz,   SEGDS,    AAS_,   \
                  INCGenR,  INCGenR, INCGenR,    INCGenR,   INCGenR,  INCGenR,   INCGenR,  INCGenR,   DECGenR, DECGenR,    DECGenR,  DECGenR,     DECGenR,  DECGenR,   DECGenR,  DECGenR,   \
                  PUSHGenR, PUSHGenR, PUSHGenR,  PUSHGenR,  PUSHGenR, PUSHGenR,  PUSHGenR, PUSHGenR,  POPGenR,  POPGenR,     POPGenR,   POPGenR,    POPGenR, POPGenR,    POPGenR,  POPGenR,    \
                  PUSHA_,   POPA_,    BOUNDGvMa, ARPLEwGw,  SEGFS,    SEGGS,     OPSIZE,   ADDRSIZE,  PUSHIz,    IMULGvEvIz, PUSHIb,    IMULGbEbIb, INSYb,    INSYz,     OUTSXb,   OUTSXz,     \
                  JccO,     JccNO,    JccB,      JccNB,     JccZ,     JccNZ,     JccNA,    JccA,      JccS,      JccNS,      JccP,      JccNP,      JccL,     JccNL,     JccNG,    JccG,       \
                  imGroup1, imGroup1, imGroup1,  imGroup1,   TESTEbGb, TESTEvGv,  XCHGEbGb, XCHGEvGv,  MOVEbGb,   MOVEvGv,    MOVEbGb,   MOVEvGv,    MOVEvSw,  LEAGvM,    MOVEvSw,  imGrop1A,    \
                  NOP_,   XCHGAxGen, XCHGAxGen, XCHGAxGen, XCHGAxGen, XCHGAxGen, XCHGAxGen, XCHGAxGen, CBW_,      CWD_,      fCALLAp,   fWAIT_,     PUSHFv,   POPFv,    SAHF_,    LAHF_,      \
                  MOVALOb,  MOVAXOv,  MOVALOb,   MOVAXOv,   MOVSYbXb, MOVSYvXv,  CMPSYbXb, CMPSYvXv,  TESTALIb,  TESTAXIz,   STOSYbAL,  STOSYwAX,   LODSAlXb, LODSAXXw,  SCASALYb, SCASAXYv,   \
                  MOV_L_Ib, MOV_L_Ib, MOV_L_Ib,  MOV_L_Ib,  MOV_H_Ib, MOV_H_Ib,  MOV_H_Ib,  MOV_H_Ib, MOV_W_Iv,  MOV_W_Iv,  MOV_W_Iv,  MOV_W_Iv,    MOV_W_Iv, MOV_W_Iv,  MOV_W_Iv, MOV_W_Iv,    \
                  Shifts,   Shifts,  RETne,     RETne,     LESGzMp,  LDSGzMp,   MOVEbIb,   MOVEvIz,   ENTERIwIb, LEAVE_,     fRET_,    fRET_,      INT3_,    INTIb,     INTO_,    IRET_,      \
                  Shifts,    Shifts,   Shifts,   Shifts,   AAMIb,    AADIb,     SHIFTGR,  XLAT_,     FPU,       FPU,        FPU,     FPU,          FPU,     FPU,      FPU,       FPU,    \
                  LOOPNEJb, LOOPEJb,  LOOPJb,    JrCXZJb,   INALIb,   INAXIv,    OUTIbAL,  OUTIbAX,   nCALLJz,   nJMPJz,     fJMPAp,    sJMPJb,     INALDX,   INAXDX,    OUTDXAL,  OUTDXAX,   \
                  SHIFTGR,  SHIFTGR,  REPNE_,    REPE_,     HLT_,     CMC_,      F6_,  F6_,   CLC_,      STC_,       CLI_,      STI_,       CLD_,     STD_,      FE_,     FF_                
               MODR_M_VALUES:                  dd    BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM,  BP_DI_MEM,  SI_MEM,  DI_MEM,  IMM_16_MEM,   BX_MEM,\
                                                                           BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM,  BP_DI_MEM,  SI_MEM,  DI_MEM,  BP_MEM,  BX_MEM, \
                                                                           BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM, BP_DI_MEM, SI_MEM, DI_MEM, BP_MEM, BX_MEM, \
                                                                           AX_AL,           CX_CL,           DX_DL,           BX_BL,           Reg_H,        Reg_H,        Reg_H,        Reg_H      
  
       
       
        ; Keyboard buffer
        keyboardBufferCurr      db 0
        keyboardBufferRead      db 0
        entryBuffer             dd RAM+41h
        
        ; Commands Table                                                              
        commNames:               du       'cd', 0, 'cls', 0, 'dir', 0, 'help', 0, 'md', 0, 'mkdir', 0, 'move', 0, 'rd', 0, 'rmdir', 0, 'ren', 0, 'rename', 0, 'time', 0, 00
        ;                                     cd cls copy del dir help loadfix md mkdir move rd rmdir ren rename time
        commTable:               dd        CD_comm, CLS_comm, DIR_comm
         
        ; Interaprions Table     
        ;                                   0      1       2       3       4       5       6       7        8       9       A       B       C       D       E       F
        intTable:               dd          0,      0,      0,      0,      0,      0,      0,      0,       0,      0,      0,      0,      0,      0,      0,      0,\
                                       int10h,      0,      0,      0,      0,      0, int16h,      0,       0,      0,      0,      0,      0,      0,      0,      0,\
                                       int20h, int21h, int22h, int23h, int24h, int25h,      0,      0,       0,      0,      0,      0,      0,      0,      0,      0,\
                                            0,      0,      0,      0,      0,      0,      0,      0,       0,      0,      0,      0,      0,      0,      0,      0,\
                                            0,      0,      0, int43h,      0,      0,      0,      0,       0,      0,      0,      0,      0,      0,      0,      0

        ; Font's file
        hFontFile               dd ?
        
        ; Font's memory
        fontReaded              dd ?
        
        ; "Pointers" to C_Strings 
        _class                  du 'MAIN', 0
        _title                  du 'MSDOS', 0
        _fontName               du 'font.bin', 0
        _paletteName            du 'paletten.bin', 0
        _dirOutput                              du '   <DIR>', 0
        _fileOutput                             du '   <FILE>   ', 0
        _bytesOutput                    du ' bytes', 0
        _formatBytes                    du '%ld', 0
        ffd                                     WIN32_FIND_DATA
        wcex                    WNDCLASSEX 48, 0, WindowProc, 0, 0, 0, 0, NULL,\
                                           COLOR_HIGHLIGHT + 1, NULL, _class, 0
           
        ; Video modes' data
        ; For text mode - (25 * 16) * 80 = 32 000
        ; For graphic mode - = 64 000                      
        ;
        currVideoMode                   dd btm_03h
        
        btm_13h:
                    .number         db 13
                                .header                 BITMAPINFOHEADER                sizeof.BITMAPINFOHEADER, 320, -200, 1, 8, BI_RGB, 0, 0, 0, 256, 256
                                .palette                db 1024 dup(0)
        
        btm_03h:             
                    .number         db 3
                .header         BITMAPINFOHEADER        sizeof.BITMAPINFOHEADER, 640, -400, 1, 1, BI_RGB, 0, 0, 0, 2, 2
                .palette        db                      0, 0, 0, 0, \
                                                        175, 179, 176, 0
               ; .palette        db                      0, 0, 0, 0, \                   ; black
                           ;                                                                    66, 170, 255, 0,\        ; blue
                                ;                                                               0, 255, 127, 0,\                 ; green
                 ;                                       0, 255, 255, 0 \                ; cyan
                  ;                                      213, 48, 50, 0, \        ; red
                   ;                                     247, 84, 225, 0, \       ; magenta
                    ;                                    69, 22, 28, 0, \         ; brown
                     ;                                   255, 255, 255, 0, \      ; white
                      ;                                  175, 179, 176, 0,\              ; gray
                       ;                                 0, 124, 173, 0, \               ; bright blue
                        ;                                102, 255, 0, 0, \        ; bright green
                         ;                               224, 255, 255, 0, \      ; bright cyan
                          ;                              193, 0, 32, 0, \         ; bright red
                           ;                             255, 0, 255, 0, \               ; bright magenta
                            ;                            255, 253, 33, 0, \              ; yellow
                             ;                           255, 255, 255, 0         ; white
        
        mesg                    MSG
        hWnd                    dd ?
        hDC                     dd ?
        clientRect              RECT
                                           
        screenWidth             dd 0
        screenHeight            dd 0
        
        currPos                 dw 0
        startPos                dw 0
        currLine                db 0
        
        ; For work with programs          
        isProgram               db 0
        isWaitingInput          db 0
        
        bufferInput:            times 127 du 0  ; 
        ptrBufferInput          dw 0
        formatTime              db 'hh:mm:ss', 0
        oldTime:                times 9 db 0
        newTime:                times 9 db 0      ; going and replacing while param in Time isn't end
        
        ; Some flags
        insertState             db -1
        insertType                              db -1
        repaintFull             db 1
        
        startString:            times 79 db '$'         
        outString:              times 40 db '$'
                 
        ; Some errors
        errorIndefComm          du 'Bad command or wrong path$'
        errorWrongFileName      du 'Wrong path!$'        
        
        IPpointer                             dd 0
        swapper                               dd ?
        amountCPU db 0
        ; ***************
        ; * EGOR'S DATA *
        ; ***************
        retValPointer:                  dd 0
                retVal:                                 times 4 dd ?
                ; AX CX DX BX SP BP SI DI
                GeneralRegs:                    dw 8 dup(0h)  ; may accure to be very optimising desicion
                ; ES CS SS DS FS GS
                SegmentRegs:                    dw 6 dup(0h)
                FLAGSr:                                 dw 0000010000000010b
                addAddr:                                db 0
                NumInt:                             db 0
                prevESP_:                         dd ?   
  
  
;BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM,  BP_DI_MEM,  SI_MEM,  DI_MEM,  IMM_16_MEM,   BX_MEM,\
                 ;          BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM,  BP_DI_MEM,  SI_MEM,  DI_MEM,  BP_MEM,  BX_MEM, \
                  ;         BX_SI_MEM,  BX_DI_MEM,  BP_SI_MEM, BP_DI_MEM, SI_MEM, DI_MEM, BP_MEM, BX_MEM, \
                   ;        AX_AL,           CX_CL,           DX_DL,           BX_BL,           Reg_H,        Reg_H,        Reg_H,        Reg_H          
 ;                  0        1        2            3
   
   OPCODES_2_BYTE:   dd    JccO,      JccNO,     JccB,     JccNB,  JccZ,     JccNZ,   JccNA,   JccA,    JccS,   JccNS,  JccP,  JccNP,  JccL,  JccNL,  JccNG,  JccG, \
                           SetO_,     SetNO_,    SetB_,    SetNB_,   SetZ_,   SetNZ_,  SetNA_,  SetA_,   SetS_,  SetNS_, SetP_, SetNP_,\ ;SetL_, SetNL_, SetNG_, SetG_,\
                          PUSHFs,    POPFs    \ ;CPUID_,   BTEvGv,  SHLDIb,  SHLDCL,  NO_OPC2, NO_OPC2, PUSHGS, POPGS,\
                      ;   CMPXCHGEbGb, CMPXCHGEvGv,LSSGvMp,  BTREvGv, LFSGvMp
    ;MODR/M:      
        
            
                
; ---S E C T I O N  O F  D A T A---
data import
  library kernel32,"KERNEL32.DLL",\
              user32,"USER32.DLL",\
              gdi32,"GDI32.DLL",\
              shell32,"SHELL32.DLL"
              
  include "includes\api\kernel32.inc"
  include "includes\api\user32.inc"
  include "includes\api\gdi32.inc"
  include "includes\api\shell32.inc"
end data

btm_13h.videoModeMem:    dd RAM+0A0000h
RAM:                     db 1048576 dup(?)
btm_03h.videoModeMem:            db bmSize dup(?)
fontMem                  db fontMemSize dup(?)