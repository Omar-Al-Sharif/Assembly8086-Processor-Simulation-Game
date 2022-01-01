;Some helping sources:https://www.chibialiens.com/8086/platform.php#LessonP1
;https://www.youtube.com/watch?v=wwxwYr88QsU
;http://www.techhelpmanual.com/144-int_10h_1010h__set_one_dac_color_register.html

displayRegister MACRO corX,corY,string,sizeOfString
    local again
   ;This macro is to display the names of registers 
   ;; corX--> cordinate of x of the starting point of typing
   ;; corY-->coordinate of y of the starting point of typing   
   ;; string--> the string I want to type it
   ;;color-->to select the color of the printed word or byte

    XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	
	MOV SI, OFFSET string
	MOV DH,corY
	MOV DL,corX
	MOV CX,sizeOfString


    
;-----------------------DisplayContents------------------
	again:

	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	push dx
	MOV AL,[SI]   	;;the ascii of the letter for al bec. the letter to print is stored in AL
    MOV BL,0EH  	;; BL FOR THE COLOR 
	MOV BH,0    	;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  	;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
	pop dx
    
    INC DL   ;;increment the cordinates of x for the cursor
	INC SI   ;;go to the next letter 
    LOOP again
	mov cx,00
	mov dx,0F015H
	mov ah,86h
	Int 15h
 

ENDM

DisplayWord MACRO corX,corY,string,numberByte,COLOR,ASCII_TABLE 
   local back 
   ;This is a macro to be used in displaying word in VGA mode
   ;; corX--> cordinate of x of the starting point of typing
   ;; corY-->coordinate of y of the starting point of typing   
   ;; string--> the string I want to type it
   ;;numberByte--> the size of the string
   ;;COLOR-->to select the color of the printed word or byte
   ;;ASCII_TABLE for XLAT instruction

    XOR AX,AX
    XOR BX,BX
    XOR CX,CX
    XOR DX,DX
   
    MOV SI,OFFSET STRING
    MOV CL,4 
    MOV DH,corY
    MOV DL,corX
    MOV CL,4
    MOV DI, numberByte
    
    INC SI  ;; this is to start by the first of 
    MOV AL,[SI]
  BACK:
    ;the next section for trying to isolate the nibble to print it only 
	sub ax,ax
	MOV AL,[SI]
	ROl Ax,CL
	ROR AL,CL
	PUSH AX
	MOV AL,AH
	MOV BX,OFFSET ASCII_TABLE 
	XLAT
	
	;NOW LET'S SET THE CURSOR 
	
	
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	;THIS FOR TYPING THE nibble
	
	MOV BL,COLOR  ;; BL FOR THE COLOR 
	MOV BH,0  ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
    
	;INCREMENT THE VALUES FOR THE NEXT ITERATIONS
	
	INC DL 
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	
	;NOW WE WILL PRINT THE SECOND NIBBLE
	POP AX
	MOV BX,OFFSET ASCII_TABLE
	XLAT
	
	MOV BL,COLOR  ;; BL FOR THE COLOR 
	MOV BH,0  ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H 
	
	;THE NEXT PART IS TO ORGANISE THE LOOP 
	INC DL
	DEC DI
	DEC SI
	
	CMP DI,0
	JNE BACK 
	
   
  ENDM
;-------------------------------------------------------------------------

.Model Small
.Stack 64
.Data
;corX db ?
;corY db ?
;sizeOfString db 2

memoryPlayer1 db 16 dup(0)
memoryPlayer2 db 17 dup(0)

scoreLabel	 db 'Score:'
scorePlayer1 db 50h
scorePlayer2 db 70h
namePlayer1 db 'Omar'
sizeNamePlayer1 db ?
namePlayer2 db 'Atef'
sizeNamePlayer2 db ?

cfPlayer1 db 0h
cfPlayer22 db 00h
cfPlayer2 db 0h

cfLabel db 'CF'
axLabel db 'AX'
bxLabel db 'BX'
cxLabel db 'CX'
dxLabel db 'DX'

siLabel db 'SI'
diLabel db 'DI'
spLabel db 'SP'
bpLabel db 'BP'

axPlayer1 DW 0000h
bxPlayer1 DW 0001h
cxPlayer1 DW 0002h
dxPlayer1 DW 0003h

siPlayer1 DW 0004h
diPlayer1 DW 0005h
spPlayer1 DW 0006h
bpPlayer1 DW 0007h

axPlayer2 DW 0008h
bxPlayer2 DW 0009h
cxPlayer2 DW 000Ah
dxPlayer2 DW 000Bh

siPlayer2 DW 000Ch
diPlayer2 DW 000Dh
spPlayer2 DW 000Eh
bpPlayer2 DW 000Fh
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

ASC_TABL DB '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
BOL_TABL DB '0','1'
Filename DB 'guifinal.bin', 0

Filehandle DW ?

ImageData DB ImageWidth*ImageHeight dup(0)


.Code
MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
	XOR AX,AX ;clear ax
    ;Change the current mode to graphics mode (VGA 320X200 PIXEL)
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
	
	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	DisplayRegister 0,3,axLabel,2
	DisplayRegister 0,5,bxLabel,2
	DisplayRegister 0,7,cxLabel,2
	DisplayRegister 0,9,dxLabel,2
	DisplayRegister 14,3,siLabel,2
	DisplayRegister 14,5,diLabel,2
	DisplayRegister 14,7,spLabel,2
	DisplayRegister 14,9,bpLabel,2
	DisplayRegister 21,3,axLabel,2
	DisplayRegister 21,5,bxLabel,2
	DisplayRegister 21,7,cxLabel,2
	DisplayRegister 21,9,dxLabel,2
	DisplayRegister 34,3,siLabel,2
	DisplayRegister 34,5,diLabel,2
	DisplayRegister 34,7,spLabel,2
	DisplayRegister 34,9,bpLabel,2

	DisplayRegister 20,21,scoreLabel,6
	DisplayRegister 0,21,scoreLabel,6

;;;;;;;;;;;;;;;;;;;;;;;Name;;;;;;;;;;;;;;;;;;;;;

	DisplayRegister 3,11,namePlayer1,7
	DisplayRegister 23,11,namePlayer2,7

;;;;;;;;;;Setting register values;;;;;;;;;;;;;

	DisplayWord 3,3,axPlayer1,2,09h,ASC_TABL
	DisplayWord 3,5,bxPlayer1,2,09h,ASC_TABL
	DisplayWord 3,7,cxPlayer1,2,09h,ASC_TABL
	DisplayWord 3,9,dxPlayer1,2,09h,ASC_TABL
	DisplayWord 9,3,siPlayer1,2,09h,ASC_TABL
	DisplayWord 9,5,diPlayer1,2,09h,ASC_TABL
	DisplayWord 9,7,spPlayer1,2,09h,ASC_TABL
	DisplayWord 9,9,bpPlayer1,2,09h,ASC_TABL


	DisplayWord 24,3,axPlayer2,2,09h,ASC_TABL
	DisplayWord 24,5,bxPlayer2,2,09h,ASC_TABL
	DisplayWord 24,7,cxPlayer2,2,09h,ASC_TABL
	DisplayWord 24,9,dxPlayer2,2,09h,ASC_TABL
	DisplayWord 29,3,siPlayer2,2,09h,ASC_TABL
	DisplayWord 29,5,diPlayer2,2,09h,ASC_TABL
	DisplayWord 29,7,spPlayer2,2,09h,ASC_TABL
	DisplayWord 29,9,bpPlayer2,2,09h,ASC_TABL

;;;;;;;;;;;;;;;;;;;;;;;Score;;;;;;;;;;;;;;;;;;;;;

	DisplayWord 6,21,scorePlayer1,1,09h,ASC_TABL	
	DisplayWord 26,21,scorePlayer2,1,09h,ASC_TABL

;;;;;;;;;;;;;;;;;;;;;;;Name;;;;;;;;;;;;;;;;;;;;;
	DisplayRegister 3,11,namePlayer1,7
	DisplayRegister 23,11,namePlayer2,7
;;;;;;;;;;;;;;;;;;;;;;;MemoryPlayer1;;;;;;;;;;;;;;;;;;;;;

	DisplayWord 16,0,memoryPlayer1,1,09h,ASC_TABL
	DisplayWord 16,1,memoryPlayer1+1,1,09h,ASC_TABL
	DisplayWord 16,2,memoryPlayer1+2,1,09h,ASC_TABL
	DisplayWord 16,3,memoryPlayer1+3,1,09h,ASC_TABL
	DisplayWord 16,4,memoryPlayer1+4,1,09h,ASC_TABL
	DisplayWord 16,5,memoryPlayer1+5,1,09h,ASC_TABL
	DisplayWord 16,6,memoryPlayer1+6,1,09h,ASC_TABL
	DisplayWord 16,7,memoryPlayer1+7,1,09h,ASC_TABL
	DisplayWord 16,8,memoryPlayer1+8,1,09h,ASC_TABL
	DisplayWord 16,9,memoryPlayer1+9,1,09h,ASC_TABL
	DisplayWord 16,10,memoryPlayer1+10,1,09h,ASC_TABL
	DisplayWord 16,11,memoryPlayer1+11,1,09h,ASC_TABL
	DisplayWord 16,12,memoryPlayer1+12,1,09h,ASC_TABL
	DisplayWord 16,13,memoryPlayer1+13,1,09h,ASC_TABL
	DisplayWord 16,14,memoryPlayer1+14,1,09h,ASC_TABL
	DisplayWord 16,15,memoryPlayer1+15,1,09h,ASC_TABL

	DisplayWord 16,17,cfPlayer1,1,0Fh,BOL_TABL

	DisplayRegister 19,0,ASC_TABL+0,1
	DisplayRegister 19,1,ASC_TABL+1,1
	DisplayRegister 19,2,ASC_TABL+2,1
	DisplayRegister 19,3,ASC_TABL+3,1
	DisplayRegister 19,4,ASC_TABL+4,1
	DisplayRegister 19,5,ASC_TABL+5,1
	DisplayRegister 19,6,ASC_TABL+6,1
	DisplayRegister 19,7,ASC_TABL+7,1
	DisplayRegister 19,8,ASC_TABL+8,1
	DisplayRegister 19,9,ASC_TABL+9,1
	DisplayRegister 19,10,ASC_TABL+10,1
	DisplayRegister 19,11,ASC_TABL+11,1
	DisplayRegister 19,12,ASC_TABL+12,1
	DisplayRegister 19,13,ASC_TABL+13,1
	DisplayRegister 19,14,ASC_TABL+14,1
	DisplayRegister 19,15,ASC_TABL+15,1

	DisplayRegister 18,17,cfLabel,2


;;;;;;;;;;;;;;;;;;;;;;;MemoryPlayer2;;;;;;;;;;;;;;;;;;;;;

	DisplayWord 36,0,memoryPlayer2,1,09h,ASC_TABL
	DisplayWord 36,1,memoryPlayer2+1,1,09h,ASC_TABL
	DisplayWord 36,2,memoryPlayer2+2,1,09h,ASC_TABL
	DisplayWord 36,3,memoryPlayer2+3,1,09h,ASC_TABL
	DisplayWord 36,4,memoryPlayer2+4,1,09h,ASC_TABL
	DisplayWord 36,5,memoryPlayer2+5,1,09h,ASC_TABL
	DisplayWord 36,6,memoryPlayer2+6,1,09h,ASC_TABL
	DisplayWord 36,7,memoryPlayer2+7,1,09h,ASC_TABL
	DisplayWord 36,8,memoryPlayer2+8,1,09h,ASC_TABL
	DisplayWord 36,9,memoryPlayer2+9,1,09h,ASC_TABL
	DisplayWord 36,10,memoryPlayer2+10,1,09h,ASC_TABL
	DisplayWord 36,11,memoryPlayer2+11,1,09h,ASC_TABL
	DisplayWord 36,12,memoryPlayer2+12,1,09h,ASC_TABL
	DisplayWord 36,13,memoryPlayer2+13,1,09h,ASC_TABL
	DisplayWord 36,14,memoryPlayer2+14,1,09h,ASC_TABL
	DisplayWord 36,15,memoryPlayer2+15,1,09h,ASC_TABL

	DisplayWord 36,17,cfPlayer22,1,0Fh,BOL_TABL

	DisplayRegister 39,0,ASC_TABL+0,1
	DisplayRegister 39,1,ASC_TABL+1,1
	DisplayRegister 39,2,ASC_TABL+2,1
	DisplayRegister 39,3,ASC_TABL+3,1
	DisplayRegister 39,4,ASC_TABL+4,1
	DisplayRegister 39,5,ASC_TABL+5,1
	DisplayRegister 39,6,ASC_TABL+6,1
	DisplayRegister 39,7,ASC_TABL+7,1
	DisplayRegister 39,8,ASC_TABL+8,1
	DisplayRegister 39,9,ASC_TABL+9,1
	DisplayRegister 39,10,ASC_TABL+10,1
	DisplayRegister 39,11,ASC_TABL+11,1
	DisplayRegister 39,12,ASC_TABL+12,1
	DisplayRegister 39,13,ASC_TABL+13,1
	DisplayRegister 39,14,ASC_TABL+14,1
	DisplayRegister 39,15,ASC_TABL+15,1

	DisplayRegister 38,17,cfLabel,2

	call CloseFile
    
    ; return control to operating system
  MOV AH, 0
  INT 21H
    
	hlt
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
	
; DisplayContents PROC

; DisplayContents ENDP

END MAIN 