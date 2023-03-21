DrawHorizontal Macro CorY
    
    mov ah,2
    mov dh,CorY
    mov dl,0    ;CorX
    int 10h
    
    mov al,'-'
    mov ah,9
    mov bh,0
    mov cx,80
    mov bl,0FH
    int 10h
    
ENDM

goto macro CorX,CorY
    
    mov ah,2
    mov dh,CorY
    mov dl,CorX
    int 10h 
    
ENDM
print macro string
    
    mov ah,9
    mov dx, offset string
    int 21h
    
ENDM


.model small
.data

namePlayer1 db 'Omar','$'
sizeNamePlayer1 db ?
namePlayer2 db 'Atef','$'
sizeNamePlayer2 db ?
F3_chat         db 'To end chatting press F3','$'
space           db ' ','$'
countChar db 0   
chatArr   DB   81 dup('$') 
corX_upper db 0 
corY_upper db 1          
corX_lower db 0        ;TODO : Change when using dosbosx
corY_lower db 11
dataSent db ?



;sente

sended          db ?
received        db ?


.code
send proc far
    
    mov dx , 3FDH        ; Line Status Register
CHKS:
    In al , dx             ;Read Line Status
    AND al , 00100000b
    JZ CHKS

    mov dx , 3F8H        ; Transmit data register
    mov  al,sended
    out dx , al 
    ret    
    send endp

receive proc far
    
    mov dx,3FDH        ; Line Status Register
CHKR:
    in al,dx 
    AND al,1
    JZ CHKR
    
    mov dx,03F8H
    in al,dx 
    mov si, offset received
    mov [si],al
    ret
    
    receive endp

initialize proc far
    
;Set Divisor Latch Access Bit
    mov dx,3fbh             
    mov al,10000000b        
    out dx,al               
    
    mov dx,3f8h            
    mov al,0ch            
    out dx,al
    
    mov dx,3f9h
    mov al,00h
    out dx,al
    
    mov dx,3fbh
    mov al,00011011b
    out dx,al
    
    initialize endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


scroll_down_lower proc
    
    cmp corY_upper,24    ;TODO -> 29
    mov ah,6
    mov al,1 ;no. of lines
    mov bh,0
    
    mov cx,1100h
    mov dx,2279
    
    int 10h
    ret
    scroll_down_lower endp

chat proc
    
    mov ax,0013h
    int 10h         ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    goto 0,0
    print namePlayer1
    
    DrawHorizontal 9
    goto 0,10
    print namePlayer2
    DrawHorizontal 22   ;TODO: CHANGE TO 28
    goto 0,23           ;TODO: CHANGE TO 29
    print F3_chat
    
    KeyPress:
    
        goto corX_upper,corY_upper 
        mov ah,0
        int 16h
        
        cmp ah,1ch       ;Enter
        jz set_line
        cmp ah,0EH      ;Backspace scan code
        jz backspace
        cmp ah,3dh      ;F3 sc                    ;;;;;;;;;;;;;;;;;;;
;        jz                                       ;;TODO AT THE END;;
                                                  ;;;;;;;;;;;;;;;;;;;
        push ax
        mov al,countChar  
        mov ah,0
    
        mov si,ax
        pop ax
        
        mov chatArr[si],al
        
        mov ah,2
        mov dl,chatArr[si] 
        int 21h
        
        inc countChar
        inc corX_upper
        JMP checkChat
		
	
		
	checkChat: 
		MOV AH,1
		INT 16H
		JNZ KeyPress        ;CHECK IF KEY PRESSED OR ANY DATA RECIEVED
		MOV DX,3FDH
		IN AL,DX
		TEST AL,1
		JZ checkChat
		
		MOV DX,03F8H
		IN AL,DX
		CMP AL,3dh
		JE endChat
		CMP AL,13 ;PRESSED ENTER
		JE set_line                        ;CHECK_RECIEVED DATA
		goto corX_lower,corY_lower
		MOV AH,2
		MOV DL,AL
		INT 21H
		INC corX_lower
		JMP checkChat
        
    set_line:
        inc corY_upper
        mov corX_upper,0
        
        cmp corY_upper,8
        jz scroll_down_upper
        JMP checkChat    
    
    backspace:
        cmp countChar,0
        je KeyPress
        
        dec corX_upper
        goto corX_upper,corY_upper
        print space
        dec countChar
        JMP checkChat
        
    scroll_down_upper:
    
        ;cmp corY_upper,8    ;TODO -> 14
        dec corY_upper
        mov ah,6
        mov al,1 ;no. of lines
        mov bh,0
    
        mov cx,0100h
        mov dx,0879
        int 10h
        
        goto corX_upper,corY_upper
        
        JMP KeyPress
		
		;chatting: 
		;MOV AH,1
        ;INT 16H
		
		
	SEND_LOOP:
		MOV AL,countChar
		MOV AH,0
		MOV SI,AX
		MOV chatArr[SI],13
		INC countChar
		MOV corX_upper,0
		LEA SI,chatArr
		CMP corY_upper,13
		JE scroll_down_upper
		INC corY_upper
		JMP START_SENDING
		
		
	START_SENDING:
		MOV AH,[SI]
		MOV dataSent,AH
		call far send
		INC SI
		DEC countChar
		JNZ START_SENDING
		JMP checkChat
endChat:
    ret
    chat endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main proc far
    mov ax,@data
    mov ds,ax
    xor ax,ax
    
    call chat
    
    main endp
end main


