.Model Small
.Stack 64
.Data

axLabel db 'AAX'

Palette LABEL WORD
	dw 0000h;0  -GRB ChatLine
	dw 00A1h;1  -GRB Strokes
	dw 0FFFh;2  -GRB 
	dw 0FFAh;3  -GRB Background
	dw 0FFFh;4  -GRB 
	dw 0FFFh;5  -GRB
	dw 0FFFh;6  -GRB
	dw 0FFFh;7  -GRB
	dw 0FFFh;8  -GRB
	dw 0FFFh;9  -GRB
	dw 0FFFh;10  -GRB
	dw 0FFFh;11  -GRB
	dw 0FFFh;12  -GRB
	dw 0FFFh;13  -GRB
	dw 0FFFh;14  -GRB
	dw 0FFFh;15  -GRB
	dw 0FFFh;16  -GRB
	dw 0FFFh;17  -GRB
	dw 0FFFh;18  -GRB
	dw 0FFFh;19  -GRB
ImageWidth EQU 320
ImageHeight EQU 200

Filename DB 'guifinal.bin', 0

Filehandle DW ?

ImageData DB ImageWidth*ImageHeight dup(0)

.Code
INCLUDE Pass.inc
MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX

    MOV AH, 0
    MOV AL, 13h
    INT 10h
	mov si,offset Palette
	xor ax,ax
	
	Paletteagain:
	mov dx,[si]
	call SetPalette ;Set Color AL to DX (-GRB)
	inc si			;Move down two bytes
	inc si
	inc ax
	cmp ax,16		;Are we done?
	jnz Paletteagain
	
    CALL OpenFile
    CALL ReadData
	
    LEA BX , ImageData ; BL contains index at the current drawn pixel
	
    MOV CX,0
    MOV DX,0
    MOV AH,0ch
	
; Drawing loop
drawLoop:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX,ImageWidth
JNE drawLoop 
	
    MOV CX , 0
    INC DX
    CMP DX , ImageHeight
JNE drawLoop

	

	
    ; Press any key to exit
    ;MOV AH , 0
    ;INT 16h
    
	xor cx,cx
	xor dx,dx
	Pass 1,3,axLabel,2
	Call DisplayContents
	
	call CloseFile
    
    ;Change to Text MODE
    ;MOV AH,0          
    ;MOV AL,03h
    ;INT 10h 

    ; return control to operating system
    MOV AH , 4ch
    INT 21H
    
MAIN ENDP

SetPalette proc		;AL=Color num ... DX=-GRB
	push dx
	push cx
	push bx
	push ax 
		mov bx,ax	;Color
		mov cl,2
		
		mov al,dh
		and al,00001111b ;----GGGG
		shl al,cl		 ;--GGGGgg
		mov ch,al	;G (6 bit 0-63)

		mov al,dl
		and al,11110000b ;RRRR----
		shr al,cl		 ;--RRRRrr
		mov dh,al	;r (6 bit 0-63)	
		
		mov al,dl
		and al,00001111b ;----BBBB
		shl al,cl		 ;--BBBBbb
		mov cl,al	;b (6 bit 0-63)

		mov bh,0 		
		mov ax,1010h 	;10.10 Set One DAC Color Register
		int 10h ;Set Color...Color= BL  R=DH  G=CH  B=CL   
	pop ax								;RGB = 0-63
	pop bx
	pop cx
	pop dx
	ret
SetPalette endp


OpenFile PROC 

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, Filename
    INT 21h
    
    ; you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful , file handle -> AX
    ; carry = 1 -> failed , AX -> error code
     
    MOV [Filehandle], AX
    
    RET

OpenFile ENDP

ReadData PROC
    MOV AH,3Fh
    MOV BX, [Filehandle]
    MOV CX,ImageWidth*ImageHeight ; number of bytes to read
    LEA DX, ImageData
    INT 21h
    RET
ReadData ENDP 


CloseFile PROC
	MOV AH, 3Eh
	MOV BX, [Filehandle]

	INT 21h
	RET
CloseFile ENDP
	
DisplayContents PROC
	INC SI
    again:
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	push dx
	MOV AL,[SI]   ;;the ascii of the letter for al bec. the letter to print is stored in AL
    MOV BL,0Eh  ;; BL FOR THE COLOR 
	MOV BH,0     ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
	pop dx
    
    INC DL   ;;increment the cordinates of x for the cursor
	INC SI   ;;go to the next letter 
    LOOP again      
    
DisplayContents ENDP

END MAIN