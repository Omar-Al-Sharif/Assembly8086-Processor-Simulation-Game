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
	MOV AL,[SI]   ;;the ascii of the letter for al bec. the letter to print is stored in AL
    MOV BL,0EH  ;; BL FOR THE COLOR 
	MOV BH,0     ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
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
;-------------------------------------------------------------------------
Display MACRO corX,corY,string,numberByte,COLOR,ASCII_TABLE 
   local back 
   ;This is a macro to be used in displaying word in VGA mode
   ;; corX--> cordinate of x of the starting point of typing
   ;; corY-->coordinate of y of the starting point of typing   
   ;; string--> the string I want to type it
   ;;numberByte--> the size of the string
   ;;COLOR-->to select the color of the printed word or byte
   ;;ASCII_TABLE for XLAT instruction
   
    MOV SI,OFFSET STRING
    MOV CL,4 
    MOV DH,corY
    MOV DL,corX
    MOV CL,4
    MOV DI, numberByte    
    
    INC SI  ;; this is to start by the first of 
    MOV AL,[SI]
  BACK:
    ;the next section for trying to isolate the nipple to print it only 
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
	
	;THIS FOR TYPING THE NIPPLE
	
	MOV BL,COLOR  ;; BL FOR THE COLOR 
	MOV BH,0  ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
    
	;INCREMENT THE VALUES FOR THE NEXT ITERATIONS
	
	INC DL 
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	
	;NOW WE WILL PRINT THE SECOND NIPPLE
	POP AX
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
;---------------------------------------------------------
.Model Small
.Stack 64
.Data
;corX db ?
;corY db ?
;sizeOfString db 2

axLabel db 'AX'
bxLabel db 'BX'
cxLabel db 'CX'
dxLabel db 'DX'

siLabel db 'SI'
diLabel db 'DI'
spLabel db 'SP'
bpLabel db 'BP'

axnum db '0000'
bxnum db '0000'
cxnum db '0000'
dxnum db '0000'

sinum db '0000'
dinum db '0000'
spnum db '0000'
bpnum db '0000'
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
MAIN PROC FAR
    MOV AX , @DATA
    MOV DS , AX
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
	;Call DisplayContents
	
	DisplayRegister 0,5,bxLabel,2
	;Call DisplayContents

	DisplayRegister 0,7,cxLabel,2
	;Call DisplayContents

	DisplayRegister 0,9,dxLabel,2
	;Call DisplayContents
	


	DisplayRegister 14,3,siLabel,2
	;Call DisplayContents
	
	DisplayRegister 14,5,diLabel,2
	;Call DisplayContents

	DisplayRegister 14,7,spLabel,2
	;Call DisplayContents

	DisplayRegister 14,9,bpLabel,2
	;Call DisplayContents



	DisplayRegister 21,3,axLabel,2
	;Call DisplayContents
	
	DisplayRegister 21,5,bxLabel,2
	;Call DisplayContents

	DisplayRegister 21,7,cxLabel,2
	;Call DisplayContents

	DisplayRegister 21,9,dxLabel,2
	;Call DisplayContents


	DisplayRegister 34,3,siLabel,2
	;Call DisplayContents
	
	DisplayRegister 34,5,diLabel,2
	;Call DisplayContents

	DisplayRegister 34,7,spLabel,2
	;Call DisplayContents

	DisplayRegister 34,9,bpLabel,2
	;Call DisplayContents

	DisplayRegister 3,3,axnum,4

	DisplayRegister 3,5,bxnum,4

	DisplayRegister 3,7,cxnum,4

	DisplayRegister 3,9,dxnum,4


	DisplayRegister 9,3,sinum,4

	DisplayRegister 9,5,dinum,4

	DisplayRegister 9,7,spnum,4

	DisplayRegister 9,9,bpnum,4


	DisplayRegister 24,3,axnum,4

	DisplayRegister 24,5,bxnum,4

	DisplayRegister 24,7,cxnum,4

	DisplayRegister 24,9,dxnum,4


	DisplayRegister 29,3,sinum,4

	DisplayRegister 29,5,dinum,4

	DisplayRegister 29,7,spnum,4

	DisplayRegister 29,9,bpnum,4

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