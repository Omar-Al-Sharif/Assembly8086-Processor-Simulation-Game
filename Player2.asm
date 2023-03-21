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
	xor si,si
	xor di,di
	
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
	; mov cx,00
	; mov dx,0F015H
	; mov ah,86h
	; Int 15h

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


	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di
   
    MOV SI,OFFSET STRING
    MOV CL,4 
    MOV DH,corY
    MOV DL,corX
    MOV CL,4
    MOV DI, numberByte
    
    
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
    xor ax,ax
    INC SI  ;; this is to start by the first of 
    MOV AL,[SI]
  BACK:
    ;the next section for trying to isolate the nibble to print it only 
	;sub ax,ax
	MOV AL,[SI]
	ROl Ax,CL
	ROR AL,CL
	PUSH AX
	MOV AL,AH
	MOV BX,OFFSET ASCII_TABLE 
	XLAT
	
	;NOW LET'S SET THE CURSOR 
	
	
;	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
;	INT 10H      
	
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
	xor ax,ax
	CMP DI,0 
	JNE BACK
	   
ENDM

getString MACRO corX,corY,string
	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di

	mov dh,corY
	mov dl,corX
	mov ah,2
	int 10h

	mov dx, offset string
	mov ah,0Ah
	int 21h
ENDM

LoadString MACRO OriginalString,CopiedString,SizeOfOriginal,SizeOfCopied 

     
     
     MOV SI,OFFSET OriginalString 
     MOV DI,OFFSET CopiedString
     MOV CX,SizeOfOriginal
     
     REPE MOVSB 
     ;Push BX
     
     MOV BX,SizeOfOriginal
     MOV SizeOfCopied,BX
                                            
ENDM




COMPARE MACRO instruction,avaliable_instruction,sizeOfInstruction,opcode
     Local FOUND,UNFOUND,DONE1
     
     MOV SI,OFFSET avaliable_instruction
     MOV DI,OFFSET instruction
     MOV CX,sizeOfInstruction
     REPE CMPSB 
    
     CMP CX,0
     JE FOUND
     JNE UNFOUND

  FOUND: MOV AX,opcode   
         MOV instructionOpcode,AX
         MOV instructionFound,1
         JMP DONE1
         
  UNFOUND:MOV instructionFound,0 
    
  DONE1:
  
ENDM 

displayName MACRO corX,corY,string
	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di
	
;	MOV SI, OFFSET string
;	ADD SI,2
	MOV DH,corY
	MOV DL,corX
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	MOV DX, OFFSET string
	MOV AH,9
	INT 21H

	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di

ENDM

HexToAsc MACRO number,string  ;2 characters only
    
    Push ax
    Push di
    XOR AX, AX
    XOR DI,DI
    
    mov al,number
    MOV DL, 0AH
    DIV DL
    
    MOV DI, offset string
    ADD DI, 1
    MOV [DI], AH
    ADD [DI], 30H
    MOV AH, 0
    
    DIV DL
    DEC DI
    MOV [DI], AH
    ADD [DI], 30H
    
    Pop di
    Pop ax
    
ENDM
AscToHex MACRO string, result
    Push ax
    Push si
    XOR AX, AX
    XOR SI, SI
    
    mov si,offset string
    add si,2
    sub [si],30h
    inc si
    sub [si],30h
    
    mov SI,offset string
    ADD SI, 2
    mov AL, [SI]
    MOV DL, 10
    MUL DL
    INC SI
    ADD AL, [SI]
    mov result,AL
    
    Pop si
    Pop ax
    
ENDM

.model large 
.386
.stack 64
;ORG 1000
.data
;ORG 1000
please   db 'Please enter your name:',10,13,'$'
initial   db 'Initial Points:',10,13,'$'
key      db 'Press ENTER key to continue$' 

hasWon db 0h

StartChatting db 'To Start Chatting Press F1','$'
StartGame db 'To Start The Game Press F2','$'
EndGame db 'To End The Game Press ESC','$'
Assump  db 'All instructions should be written in UPPERCASE letters',10,13,' ',10,13,'No spaces between operands',10,13,' ',10,13,'Each mistake will cost you -1 points','$'

forbiddenCharLabel db 'Please Enter the forbidden Character(In Caps):',10,13,'$'
LevelSelect db 'Press 1 to select level 1',10,13,' ',10,13,'Press 2 to select level 2','$'

namePlayer1  db 15,?, 15 dup('$')

scoreLabelPlayer1 db 5,?,5 dup('$')

namePlayer2  db 15,?, 15 dup('$')

scoreLabelPlayer2 db 5,?,5 dup('$')

forbiddenTempChar1 db 2,?, 2 dup('$')
forbiddenTempChar2 db 2,?, 2 dup('$')

forbiddenChar1 db 0H
forbiddenChar2 db 0H

noOfForbiddenPlayer1 db 0h

scoreLabel	 db 'Score:','$'
;org 20
scorePlayer1 db 0h
scorePlayer2 db 0h

;;namePlayer1 db 'Omar','$'
;sizeNamePlayer1 db ?
;;namePlayer2 db 'Atef','$'
;sizeNamePlayer2 db ?


;cfPlayer2 db 00h


userString db 12,?,12 dup('$')

cfLabel db 'CF','$'
axLabel db 'AX'
bxLabel db 'BX'
cxLabel db 'CX'
dxLabel db 'DX'

siLabel db 'SI'
diLabel db 'DI'
spLabel db 'SP'
bpLabel db 'BP'


all_Registers_stringPlayer1   DB  "AXBXCXDXSIDISPBPALAHBLBHCLCHDLDH"  ;this is a string that contain all the registers, this string is helpful in detecting the exsistance of registers
player1Registers label byte
;Send elements starting from here
axPlayer1 dw 0
bxPlayer1 dw 07h
cxPlayer1 dw 09h
dxPlayer1 dw 30h
siPlayer1 dw 0
diPlayer1 dw 0
spPlayer1 dw 0Fh
bpPlayer1 dw 0
;player1Registers + 15
memoryPlayer1 db 16 dup(0)

originatevar db 0
cfPlayer1                      DB            0
axPlayer2 DW 0008h
bxPlayer2 DW 0009h
cxPlayer2 DW 000Ah
dxPlayer2 DW 000Bh
siPlayer2 DW 000Ch
diPlayer2 DW 000Dh
spPlayer2 DW 000Eh
bpPlayer2 DW 000Fh
;player2Registers + 15
memoryPlayer2 db 16 dup(0)
cfPlayer2                      DB            0
;End Send 33*2 bytes -> put 66 (42h) in cx and loop
axPlayerN DW 0000h
determineRegisters db 1h
CarryFlagN  dw 0000h

scorePlayerN dw 0h
playerTurn db 1h
forbiddenCharFound db 0h
;ORG 20
   operand2Immediate  dw   ?
   operand1Immediate  dw   ?
   
   isOperand2Immediate  dw   ?
   isOperand1Immediate  dw   ?
offsetOperand1 dw ?
offsetOperand2 dw ?
offsetOperandN dw ?
operation     DB         20,?,20 dup('$')
instruction   DB         5 DUP('$')
operands      DB         20 dup('$')
operand1     DB          5 DUP('$')
operand2     DB          5 DUP('$')  
operandN               DB     5 DUP('$')
ptrOperandN            DB     5 DUP('$')
emptyString            DB     5 DUP('$')
sizeOfOperand1     DW      ?
sizeOfOperand2     DW      ?
sizeOfOperandN         DW        ?
sizeOfInstruction  DW      ? 
Operand db '[CX]'
hexOperand1 dw 0
hexOperand2    dw 0 
hexOperandN    dw 0 
 
hasError db 0h
hasBrack db 0h
isNum db 0h
isChar db 0h
operandsCount db ?
validAddressing db 0h
validMem db 0h
isOperand1Register     DB        ?
   isOperand2Register     DB        ?
   isOperand1HasOffset    DB        ?
   isOperand2HasOffset    DB        0
   isOperandNHasOffset    DB        0
   isOperand18Bits        DB        ?
   isOperand28Bits        DB        ?
   isOperandN8Bits        DB        ?
   Counter                dw      0h
   isFirstCharLetterOperand1      DB            0
   isFirstCharLetterOperand2      DB            0
   isFirstCharNumberOperand1      DB            0
   isFirstCharNumberOperand2      DB            0
   isFirstCharLetterOperandN      DB            0
   isFirstCharNumberOperandN      DB            0
 
 
;Size db 4h
AsciiToHexTable db 30h,31h,32h,33h,34h,35h,36h,37h,38h,39h,41h,42h,43h,44h,45h,46h

instructionFound    db   0h     ;This is a boolean variable
instructionOpcode   dw   ?

movInst  db 'MOV'   
addInst  db 'ADD'
adcInst  db 'ADC'
subInst  db 'SUB'
sbbInst  db 'SBB'
mulInst  db 'MUL'
;imulInst  db 'IMUL'
xorInst  db 'XOR'
andInst  db 'AND'
orInst   db 'OR'
notInst  db 'NOT'
clcInst  db 'CLC'
pushInst db 'PUSH'
popInst  db 'POP'
incInst  db 'INC'
decInst  db 'DEC'
nopInst  db 'NOP'
divInst  db 'DIV'
shlInst  db 'SHL'
shrInst  db 'SHR'
negInst  db 'NEG'

;------------------------------------------------------------------
;------------------------------------------------------------------
;------------------------------------------------------------------

sWon  DB    0
gameLevel  DB  0
noOfClearedRegisters   DB 0



newForbidden   db 2,?, 2 dup('$')

forbiddenCharN db 2,?, 2 dup('$')

winningValue   dw   10Eh


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

.code 
;MAIN PROC FAR
 
; call the menus
     
; call the gui 
 
 
; MAIN ENDP 
 
 main proc far
    
    mov ax,@data
    mov ds,ax
    mov es,ax

    XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di
    
    mov ax,0013h
    int 10h  ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    mov ah,9
    mov dx,offset please
    int 21h
    
    mov ah,0ah
    mov dx, offset namePlayer1
    int 21h
    
    mov ah,2
    mov dx,0300h ;Moving the cursor
    int 10h  
    
	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di

    mov ah,9
    mov dx, offset initial
    int 21h

    XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di

    mov ah,0ah
    mov dx, offset scoreLabelPlayer1
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    mov ah,2
    mov dx,0600h ;Moving the cursor
    int 10h  
    
    mov ah,9
    mov dx, offset forbiddenCharLabel
    int 21h
    
    mov ah,0ah
    mov dx, offset forbiddenTempChar1
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    mov ah,2
    mov dx,0B10h ;Moving the cursor
    int 10h
    
    mov ah,9
    mov dx, offset key
    int 21h
    
is1Enter:
    mov ah,0
    int 16h
    cmp ah,1ch ;Enter key scan code
    Jnz is1Enter
    jz  itisEnter
    
itisEnter:
    call far ptr P2_main_screen


call far ptr GameFlow
  
main endp

Check_forbidden proc 
 
xor ax,ax
xor bx,bx
xor cx,cx
xor dx,dx

mov di,offset operation[2] 
mov al,forbiddenCharN  
mov cl,operation[1]
REPNE SCASB

CMP CX,0
JNE forbiddenFOUND 
MOV forbiddenCharFound,0
JMP RETURN 
 
forbiddenFOUND: MOV forbiddenCharFound,1



RETURN:
 RET
Check_forbidden endp 
 
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
IsWon PROC 
  

    mov ax,winningValue
    mov di,axPlayer1
    mov cx,08h
    repne SCASW
    CMP CX,0
    JNE Player1Won
	
    mov di,axPlayer2
    mov cx,08h
    repne SCASW
    CMP CX,0
    JNE Player2Won

    cmp scorePlayer1,0H
    je Player2Won
    cmp scorePlayer2,0h
    je Player1Won
    jmp NotWon

    Player1Won:
    mov al,1
     mov hasWon, al 
       ret
  
    Player2Won:
    mov al,1
    mov hasWon, al 
    ret

    NotWon: 
    mov al,0
    mov hasWon,al 
          ret
IsWon ENDP 


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------MAIN PROCEDURE----------------------------------------
;------------------------------------------------------------------------------------------
CountOperands proc
mov si, offset operand1
cmp byte ptr[si],'$'
jne IncCount
CheckSecond:
mov si, offset operand2
cmp byte ptr[si],'$'
je exit 
inc operandsCount
jmp exit
IncCount:
inc operandsCount
jmp CheckSecond
exit:
ret

CountOperands endp

AssignPlayerN proc
mov cl,determineRegisters
cmp cl,1h
je EnablePlayer1Reg
mov bx, offset axPlayer2
mov axPlayerN, bx
mov bx,offset cfPlayer2
mov CarryFlagN, bx
ret
EnablePlayer1Reg:
mov bx, offset axPlayer1
mov axPlayerN, bx
mov bx,offset cfPlayer1
mov CarryFlagN, bx
ret
AssignPlayerN endp

;Integrated Function
OperandsProc proc far
call far ptr Seperate
call far ptr AssignPlayerN
call far ptr AssignInstructionOpcode
call CountOperands
mov cl,operandsCount
cmp cl,0h
je NoOperands
call far ptr GetOperandType
NoOperands:
ret
OperandsProc endp

ExecuteOperation proc
call far ptr OperandsProc
call far ptr ExecuteInstruction
ret
ExecuteOperation endp
GameFlow proc far 

;Call the main screen 
; depending on the options of the main screen, we decide if we want to get into the game mode 
; the flowing code is just for the flow of the game mode 
;the flow of the game itself depends on the main screen
; we can call this procedure by game mode procedure
;Also note that this procedure will almost be the same in player2 with the exception of small change in input values for macros and others


;call far ptr ClearAllRegisters


CMP playerTurn,1
	jz  its_player_1  
	jnz ReceiveChanges
    

its_player_1:
 call far ptr ResetVars
 ;LoadString forbiddenTempChar1,forbiddenCharN,4,4
 MOV SI,OFFSET forbiddenTempChar1 
     MOV DI,OFFSET forbiddencharN
     MOV CX,4
     REPE MOVSB  	

 EnterAnotherKey:
	mov ah,01h 
    int 16h
	
	mov ah,0h
	int 16h 
	
	CMP AL,31H 
	je PowerUpMyself 

    CMP AL,32H 
	je PowerUpBoth	
	
	CMP AL,33H 
	je ChangeForbiddenChar
	
	CMP AL,34H 
	je ClearAll
	
	CMP AL,35H 
	je CheckCheckLevel2PowerUp
	jnz ContinueCheckingKeys
    	
	CheckCheckLevel2PowerUp:
     CMP gameLevel,2
     je ChangeWinValuePowerUp
	 CMP AL,37H
	 je PowerUpOnOtherRegisters
	 jne EnterAnotherKey
    
	
ContinueCheckingKeys:
    CMP AL,36H
	je No_power_up
	jne EnterAnotherKey
	 
	
	PowerUpMyself:
         getString 1,19,operation
		 
		 CALL Check_forbidden
		 CMP forbiddenCharFound,1
		 JE ErrorFound
		 
		 mov determineRegisters,2
		 CALL ExecuteOperation
		 
		 CMP hasError,1
		 JE ErrorFound
		 sub ScorePlayer1,5
		 jmp endTurn1 
	    
	PowerUpBoth:
	    getString 1,19,operation
		 
		 CALL Check_forbidden
		 CMP forbiddenCharFound,1
		 JE ErrorFound
		 
		 mov determineRegisters,1
		 CALL ExecuteOperation
		 
		 CMP hasError,1
		 JE ErrorFound
		 sub ScorePlayer1,3
		 jmp endTurn1 
		 
		 
		 CALL Check_forbidden
		 CMP forbiddenCharFound,1
		 JE ErrorFound
		 
		 mov determineRegisters,1
		 CALL ExecuteOperation
		 
		 CMP hasError,1
		 JE ErrorFound
		 jmp endTurn1 
	    
	ChangeForbiddenChar:
	    
		 INC noOfForbiddenPlayer1
		 CMP noOfForbiddenPlayer1,1
		 JLE ExecuteNoForbiddenPlayer1
		 JG DontExecuteForbidden
		
	ExecuteNoForbiddenPlayer1:
      		getString 1,19,newForbidden
			;LoadString newForbidden,forbiddenTempChar1,2,2
                MOV SI,OFFSET newForbidden 
     MOV DI,OFFSET forbiddenTempChar1
     MOV CX,2
     
     REPE MOVSB 
			Sub ScorePlayer1,8
			JMP No_power_up
	
	DontExecuteForbidden:
	         JMP No_power_up
	
	ClearAll:
	    INC noOfClearedRegisters
	    CMP noOfClearedRegisters,1
		JG DontExecuteClearRegisters 
		mov determineRegisters,1
		Call far ptr ClearAllRegisters            
		mov determineRegisters,2
		call far ptr ClearAllRegisters
		sub scorePlayer1,30
		;Update Gui
		jmp No_power_up
					DontExecuteClearRegisters:
                       JMP  EnterAnotherKey

						
	
	ChangeWinValuePowerUp: 
	    ;level 2
        ;change ascii to hex
        
		
    PowerUpOnOtherRegisters:  ; level 2
	    mov ah,01h 
        int 16h
	
	    mov ah,0h
	    int 16h 
		
		CMP AL,1
		je PowerUpMyself
		
		Cmp AL,2
		je No_power_up
		jne PowerUpOnOtherRegisters
	
	
	;This is the normal case for the player where his commands are executed on his rival's registers 
	No_power_up:
	    
		 getString 0,19,operation
		 
		 CALL Check_forbidden
		 CMP forbiddenCharFound,1
		 JE ErrorFound
		 
		 mov determineRegisters,2
		 CALL ExecuteOperation
		 
		 CMP hasError,1
		 JE ErrorFound
		 jmp endTurn1
		 
		
			
		
 ErrorFound: 
        SUB ScorePlayer1,10
        mov hasError,0
 
 endTurn1: 
        call IsWon
        mov al,hasWon
        CMP al,1
        JE Won
        mov playerTurn,2
		CALL far ptr ResetVars
       ;Update Gui
       ;CALL far ptr SendGuiElements		   

 ReceiveChanges: 
    ;  CALL far ptr RecieveGuiElements
	  ;Update Gui 
      call far ptr UpdateGui
 


Won: ;  Give winning screen  then Return to main screen 

ret
GameFlow endp  

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


SendGuiElements proc far
;Set Divisor Latch Access Bit
mov dx,3fbh             ; Line Control Register
mov al,10000000b        ;Set Divisor Latch Access Bit
out dx,al                ;Out it
;Set LSB byte of the Baud Rate Divisor Latch register.
mov dx,3f8h            
mov al,0ch            
out dx,al
;Set MSB byte of the Baud Rate Divisor Latch register.
mov dx,3f9h
mov al,00h
out dx,al
;Set port configuration
mov dx,3fbh
mov al,00011011b
;0:Access to Receiver buffer, Transmitter buffer
;0:Set Break disabled
;011:Even Parity
;0:One Stop Bit
;11:8bits
out dx,al

;Sending a value
mov cx,42h 
mov si, offset axPlayer1
;Check that Transmitter Holding Register is Empty
SendLoop:
        mov dx , 3FDH        ; Line Status Register
AGAIN:  In al , dx             ;Read Line Status
        and al , 00100000b
        JZ AGAIN                               ;Not empty

;If empty put the VALUE in Transmit data register
        mov dx , 3F8H        ; Transmit data register
        mov al,[si]
        inc si
        
        out dx , al
 Loop SendLoop       

; Put the byte you want to send in location 3f8h
ret
SendGuiElements endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


RecieveGuiElements proc far
;Set Divisor Latch Access Bit
mov dx,3fbh             ; Line Control Register
mov al,10000000b        ;Set Divisor Latch Access Bit
out dx,al                ;Out it
;Set LSB byte of the Baud Rate Divisor Latch register.
mov dx,3f8h            
mov al,0ch            
out dx,al
;Set MSB byte of the Baud Rate Divisor Latch register.
mov dx,3f9h
mov al,00h
out dx,al
;Set port configuration
mov dx,3fbh
mov al,00011011b
;0:Access to Receiver buffer, Transmitter buffer
;0:Set Break disabled
;011:Even Parity
;0:One Stop Bit
;11:8bits
out dx,al

mov cx,42h
mov si,offset axPlayer1
;Check that Data is Ready
RecieveLoop:
        mov dx , 3FDH        ; Line Status Register
    CHK:    in al , dx 
          and al , 1
          JZ CHK                                    ;Not Ready
 ;If Ready read the VALUE in Receive data register
          mov dx , 03F8H
          in al , dx 
          mov [si] , al
          inc si
          
Loop RecieveLoop

ret
RecieveGuiElements endp


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------




;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


AssignInstructionOpcode proc far
  XOR AX,AX
  XOR BX,BX
  XOR CX,CX
  XOR DX,DX 
  
  
  COMPARE instruction,movInst,sizeOfInstruction,00h
  CMP instructionFound,1
  JE  DONE2 
         
  COMPARE instruction,addInst,sizeOfInstruction,01h
  CMP instructionFound,1
  JE  DONE2  
  
  COMPARE instruction,adcInst,sizeOfInstruction,02h
  CMP instructionFound,1
  JE  DONE2  
  
  COMPARE instruction,subInst,sizeOfInstruction,03h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,sbbInst,sizeOfInstruction,04h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,mulInst,sizeOfInstruction,05h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,andInst,sizeOfInstruction,06h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,orInst,sizeOfInstruction,07h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,xorInst,sizeOfInstruction,08h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,divInst,sizeOfInstruction,09h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,pushInst,sizeOfInstruction,0Ah
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,popInst,sizeOfInstruction,0Bh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,incInst,sizeOfInstruction,0Ch
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,decInst,sizeOfInstruction,0Dh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,nopInst,sizeOfInstruction,0Eh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,clcInst,sizeOfInstruction,0Fh
  CMP instructionFound,1
  JE  DONE2
  
  MOV hasError,1
    
  DONE2:
  ret
AssignInstructionOpcode endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

         

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
         
      
AsciiToHex proc far
  mov cx,sizeOfOperandN
  mov si, bx 
SearchDigits:
  mov di,offset AsciiToHexTable
  mov al,[si]
  push cx
  mov cx,10h
  repne scasb
  jne InvalidNum
  mov dh,30h 
  mov dl,10h
  sub dl,cl
  cmp dl,0Bh
  jb FirstHalf
  mov dh,37h
FirstHalf:
  sub [si],dh
  pop cx
  inc si
  loop SearchDigits
;Combine the digits
  mov cx,sizeOfOperandN
  mov si, bx
  mov dx,cx
  dec dx
  add si,dx
;mov dx,000Fh 
  mov al,4h
  mov dl,0h
PushDigits:
  mov al,4h
  mul dl 
  push cx
  mov cl,al
  mov bl,[si]
  mov bh,0h
  shl bx,cl
  inc dl
  dec si
  pop cx
  push bx
  Loop PushDigits
  mov cx,sizeOfOperandN
  mov di,0h
Combine:
  pop ax
  add di,ax
  Loop Combine
  mov si,hexOperandN
  mov [si],di
ret
  InvalidNum:
  inc hasError
ret
AsciiToHex endp 

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

Seperate proc far
             ;------------------------------------------------------------------------------------------------------    
      ;-----here I seperated the operation into instruction+ operand1+operand2-------------
              XOR SI,SI
              XOR DI,DI
              XOR AX,AX
              XOR BX,BX
              XOR CX,CX
              XOR DX,DX

              MOV SI, OFFSET operation 
              Add SI,2h
              MOV BP,SI
              MOV DI, OFFSET instruction
              MOV sizeOfInstruction,SI 
Prepare_instruction_Seperate:           ; here you are trying to check if you found space or not, if u don't, it will jump to instruction_seperate Loop
              CMP [SI],byte ptr 20h              ; 20h= ASCII code of SPACE
              JNE instruction_seperate
              
              SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
              MOV sizeOfInstruction,SI
              MOV SI,BP  
              ADD SI,sizeOfInstruction
              JMP Prepare_operand_seperation
   
instruction_seperate:                   ;This loop is responsible for the copy the chars of instruction into instruction variable
              MOV Al,[SI]   
              MOV [DI],Al
              INC DI
              INC SI     
              JMP Prepare_instruction_Seperate

Prepare_operand_seperation: 
              MOV DI,OFFSET operands
              INC SI
               
   label1:  
              CMP [SI],byte ptr 24H              ;24h= ASCII code of $
              JNE operand_seperation
              JE prepare_seperation_first_operand

Operand_seperation:
              MOV Al,[SI]
              MOV [DI],Al
              INC DI
              INC SI
              JMP label1  
      
prepare_seperation_first_operand:   ;this loop is to seperate the operands into operand1 and operand2
               MOV SI,OFFSET operands
               MOV DI,OFFSET OPERAND1
               MOV BP,SI
               MOV sizeOfOperand1,SI
   
   label2:    CMP [SI],byte ptr ','
              JNZ Check_Dollar_Sign 
              jz CalculateSize
              
              Check_Dollar_Sign:
              CMP [SI],byte ptr 0dh 
              jz CalculateSize
              JNZ seperate_first_operand
              
              CalculateSize:
              SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
              MOV sizeOfOperand1,SI
              MOV SI,BP  
              ADD SI,sizeOfOperand1
              
              CMP [SI],byte ptr 0dh
              JZ FINISHED_SEPERATION
              JMP prepare_seperation_second_operand
              
              

seperate_first_operand:
               
              MOV AL,[SI]   
              MOV [DI],AL
              INC DI
              INC SI     
              JMP label2  
                                 
                                 
prepare_seperation_second_operand: 
               INC SI
               MOV DI,OFFSET OPERAND2 
               MOV BP,SI
               MOV sizeOfOperand2,SI
               
   label3:     CMP [SI],byte ptr 0dh
               JNE seperate_second_operand
               
               SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
               MOV sizeOfOperand2,SI
               MOV SI,BP  
               ADD SI,sizeOfOperand2 
               JMP FINISHED_SEPERATION

seperate_second_operand:
              
              MOV AL,[SI]   
              MOV [DI],AL
              INC DI
              INC SI     
              JMP label3           
              
              
FINISHED_SEPERATION:  ;now you have the instruction in instruction variable, also you have operand1 and operand2 
ret           
              
Seperate endp


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


LoadOffsetNStatus proc
cmp cx,1h
je MoveHasOffset2
mov al,IsOperandNHasOffset
mov IsOperand2HasOffset,al
ret
MoveHasOffset2:
mov al,IsOperandNHasOffset
mov IsOperand1HasOffset,al    
ret    
LoadOffsetNStatus endp    
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


;*********************
CheckBrackets proc

mov cx,sizeOfOperandN
CheckBracket:
cmp byte ptr[bx], 5Bh
jz HasBrackets 
inc bx ;Where to store results?
Loop CheckBracket
ret 
HasBrackets:
inc hasBrack
ret
CheckBrackets endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

;*********************
IsNumber proc
mov al,0h
mov isNum,al
cmp byte ptr[bx], 29h
ja CheckLessThan
ret
CheckLessThan:
cmp byte ptr[bx], 40h
jb Number
ret
Number:
inc isNum
ret
IsNumber endp
;*********************           

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

IsCharacter proc

cmp byte ptr[bx], 40h
ja CheckBelow
ret
CheckBelow:
cmp byte ptr [bx], 5Bh
jb Character
ret
Character:
inc isChar
ret
IsCharacter endp

ValidateAddressing proc
cmp [bx], 'SI'
je SiPointedContents ;Get contents of memory that si points to
cmp [bx], 'DI'
je DiPointedContents
cmp [bx], 'BX'
je BxPointedContents
;if not one of the previous it may be num
Call IsNumber
mov al,isNum
cmp al,1 
je ValidMemoryRange
ret
SiPointedContents:
mov di,[axPlayerN+8]
mov si,axPlayerN 
add si,0Fh
Add di,si
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret
DiPointedContents:
mov di,[axPlayerN+10]
mov si,axPlayerN 
add si,0Fh
Add di,si
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret
BxPointedContents:
mov di,[axPlayerN+2]
mov si,axPlayerN 
add si,0Fh      
Add di,si
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret 
ValidMemoryRange:
;Modify size before calling
mov ax,sizeOfOperandN
sub ax,2
mov sizeOfOperandN,ax
push bx
call far ptr RemoveBrackets
pop bx
push bx
call far ptr AsciiToHex
pop bx
call far ptr ValidateMemRange
mov al, validMem
cmp al,1h
je MemoryContents
inc hasError
ret

MemoryContents:

mov bx,hexOperandN
mov cx,[bx]
;mov si, offset memoryPlayer1
mov si,axPlayerN 
add si,0Fh
Add cx,si  
mov si, offsetOperandN
mov [si],cx
inc isOperandNHasOffset
inc validAddressing
ret

ValidateAddressing endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


RemoveBrackets proc far
mov cx,sizeOfOperandN
mov ah,0
mov si,bx
RmBrackets:
mov al,[bx]
cmp al,']'
je PopStackToOperand
push ax
inc bx
Loop RmBrackets
mov cx, sizeOfOperandN
Add cx,2h
mov bx,si
Call far ptr ClearString 
mov si,bx
dec si
mov cx,sizeOfOperandN
PopStackToOperand:
pop bx
xchg bh,bl
mov [si],bx
dec si
Loop PopStackToOperand
ret
RemoveBrackets endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


ClearString proc far
mov cx,Counter
cmp cx,1h
je ClearOperand1
;LoadString emptyString,operand2,5,5
 MOV SI,OFFSET emptyString 
     MOV DI,OFFSET operand2
     MOV CX,5
     
     REPE MOVSB 
    ; Push BX
     
     MOV BX,5
     ;MOV SizeOfCopied,BX
ret
ClearOperand1:
;LoadString emptyString,operand1,5,5
MOV SI,OFFSET emptyString 
     MOV DI,OFFSET operand1
     MOV CX,5
     
     REPE MOVSB 
     ;Push BX
     
     MOV BX,5
     ;MOV SizeOfCopied,BX
ret
ClearString endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

AssignOperandN proc
cmp cx,1h
je AssignOperandOne
AssignOperandTwo:
mov bx, offset operand2
;mov operandN, byte ptr [si]
mov si,sizeOfOperand2
mov sizeOfOperandN,si
mov si,offset hexOperand2
mov hexOperandN,si
mov si, offset offsetOperand2
mov offsetOperandN,si
ret
AssignOperandOne:
mov bx, offset operand1
;mov operandN,[si]
mov si,sizeOfOperand1
mov sizeOfOperandN,si
mov si,offset hexOperand1
mov hexOperandN,si
mov si, offset offsetOperand1
mov offsetOperandN,si
ret
AssignOperandN endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

ValidateMemRange proc far
mov si,hexOperandN
mov ax,[si]
cmp ax,0h
ja UpperRange
inc hasError
ret
UpperRange: 
cmp ax,0fh
jb ValidMemory
inc hasError
ret
ValidMemory:
inc validMem
ret
ValidateMemRange endp



;---------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

DirectOrIndirect proc

mov ch,0h
mov cl, operandsCount

LoopAllOperands:
mov Counter,cx
push cx
call AssignOperandN
push bx
Call CheckBrackets 
mov al,hasBrack
cmp al,1
je NumOrChar
pop bx
jmp Handled
NumOrChar:
pop bx
inc bx
Call IsCharacter 
mov al,isChar
cmp al,1
je CheckAdMode ;Check Addressing mode
;if not char, it may be num
Call IsNumber
mov al,isNum
cmp al,1 
je CheckAdMode
inc hasError
ret
CheckAdMode: 
Call ValidateAddressing
mov al,validAddressing
cmp al,1
je Handled
inc hasError
pop cx
ret
Handled:
pop cx 

call LoadOffsetNStatus
Loop LoopAllOperands
ret
DirectOrIndirect endp

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

;*****************************

GetOperandType proc

call DirectOrIndirect
;**************
;Immediate branching
;Register branching

Call far ptr RegisterOrImmediate

;***************
ret
GetOperandType endp
;****************************

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

ClearGarbage proc 

            XOR SI,SI
            XOR DI,DI
            XOR AX,AX
            XOR BX,BX
            XOR CX,CX
            XOR DX,DX

ClearGarbage ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

SaveOperands proc


ret
SaveOperands endp





;------------------------------------------------------------------------------------------
;--------------------------------------PROCUDURES------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

RegisterOrImmediate proc
     
               
     xor ax,ax
     xor bx,bx
     xor cx,cx
     xor dx,dx
     xor si,si
     xor di,di        
     ;----------------------------    
     CMP operandsCount,1
     JE Check_Modes_Of_Operand1
     JG Check_Modes_Of_Operand2    


 Check_Modes_Of_Operand2:
          
           CMP hasError,1
           je GiveErrorToPlayer 
           
           CMP isOperand2HasOffset,1
           JE  Check_Modes_Of_Operand1
          
           MOV isOperandNHasOffset,0
           LoadString emptyString,operandN,5,sizeOfOperandN
           LoadString operand2,operandN,sizeOfOperand2,sizeOfOperandN
           
           CALL Check_First_char
           CMP hasError,1 
           je GiveErrorToPlayer 
           
        
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand2,AL
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand2,AL 
           
           
           CMP isFirstCharLetterOperand2,1
           JE  Call_register_addressing_function2
           JMP Call_Immediate_addressing_function2
           
           
     Call_register_addressing_function2:
           CALL Check_Register_Addressing
           CMP hasError,1
           je GiveErrorToPlayer  
           MOV isOperand2Register,1
           MOV AX,offsetOperandN
           MOV offsetOperand2,AX
           MOV Bl,isOperandN8Bits
           MOV isOperand28Bits,Bl
           jmp Check_Modes_Of_Operand1
            
           
     Call_Immediate_addressing_function2:
           CALL AsciiToHex
           mov si,hexOperandN 
           MOV AX,[si]           
           CMP ax,0FFFFh
           JLE  The_operand_is_immediate
           MOV hasError,1       
           JMP GiveErrorToPlayer
                 
           
     The_operand_is_immediate:
                mov si,hexOperandN 
                MOV AX,[si]
                MOV Operand2Immediate,AX
                MOV AX, OFFSET Operand2Immediate                   
                MOV offsetOperand2,AX
                MOV isOperand2Immediate,1
                           
          
           ;CHECK FIRST CHAR
           ;if it is letter --> call the Register_Addressing_Function
           ;if it can't find offset-->give error
           ;if it is number ---> call the Imidate_Addressing_function              

 Check_Modes_Of_Operand1:
           CMP hasError,1
           je GiveErrorToPlayer 
           
           CMP isOperand1HasOffset,1
           JE  FinishAddressing
           
           MOV isOperandNHasOffset,1
           LoadString emptyString,operandN,5,sizeOfOperandN
           LoadString operand1,operandN,sizeOfOperand1,sizeOfOperandN
           
           CALL Check_First_char
           CMP hasError,1
           je GiveErrorToPlayer 
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand1,AL
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand1,AL
           
           
           
           CMP isFirstCharLetterOperand1,1
           JE  RegAddFun1
           JMP ImmAddFun1
           
           
           RegAddFun1: 
                 CALL Check_Register_Addressing
                 CMP hasError,1
                 je GiveErrorToPlayer 
                 MOV isOperand1Register,1
                 MOV AX,offsetOperandN
                 MOV offsetOperand1,AX
                 MOV Bl,isOperandN8Bits
                 MOV isOperand18Bits,Bl
                 JMP FinishAddressing
                 
        ImmAddFun1:
           CALL AsciiToHex 
           mov si,hexOperandN 
           MOV AX,[si]
           CMP AX,0FFFFh
           JLE The_operand_is_immediate
           MOV hasError,1       
           JMP GiveErrorToPlayer
           
           
           
           
           
     The_operand1_is_immediate: 
                mov si,hexOperandN 
                MOV AX,[si]
                MOV Operand1Immediate,AX
                MOV AX, OFFSET Operand1Immediate                   
                MOV offsetOperand1,AX
                MOV isOperand1Immediate,1
      
                
                
               
            ;CHECK FIRST CHAR
           ;if it is letter --> call the Register_Addressing_Function
              ;if it can't find offset-->give error
           ;if it is number ---> call the Imidate_Addressing_function
              ;give error 
          
 GiveErrorToPlayer: 
                             ;CALL ERROR FUNCTION
              ret             ;JMP END TURN

 FinishAddressing: 
                  CMP operandsCount,2
                  JE Check_Register_mismatch
                  ret
                                         
        Check_Register_mismatch: CMP isOperand1Register,1
                                 JE Check_operand2_for_mismatch                                         
                                 ret
                   
        Check_operand2_for_mismatch: CMP isOperand2Register,1
                                     JE  CHECK_TYPE_OF_REGISTERS
                                     ret
                                
        CHECK_TYPE_OF_REGISTERS:     MOV AL,isOperand28Bits
                                     CMP isOperand18Bits,AL
                                     CMP AL,0H
                                     JNE  ERROR_MISMATCH
                                     JE FINISHEDCHECK
                                     
                                            
        ERROR_MISMATCH: MOV hasError,1
                        je GiveErrorToPlayer                                                        
        
        FINISHEDCHECK: 
        ret 
 RegisterOrImmediate endp




Check_First_char PROC NEAR 
     
     PUSH SI
     PUSH AX
     MOV SI,OFFSET OperandN 
     
     MOV AL,[SI]
  check_if_letter:  CMP AL,'A'  ;IF GREATER THAN OR A GO TO CHECK IF LESS THAN Z
                   JE First_char_letter
                   JG Check_less_than_or_equal_Z
                   JL  Check_if_number
    
    
  Check_less_than_or_equal_Z: CMP AL,'Z'
                             JLE First_char_letter
                             JMP Make_hasError_equal_1
 
  Check_if_number:  CMP AL,'0'
                   JE First_char_number
                   JG Check_less_than_or_equal_9
                   JL  Make_hasError_equal_1
                   
  Check_less_than_or_equal_9: CMP AL,'9'
                             JLE First_char_number
                             JMP Make_hasError_equal_1
                                
  First_char_number: MOV isFirstCharNumberOperandN,1  
                    JMP FirstCharIsKnown
 
  First_char_letter: MOV isFirstCharLetterOperandN,1
                    JMP FirstCharIsKnown
                   
  Make_hasError_equal_1: MOV hasError,1
 
  FirstCharIsKnown:    
     POP AX
     POP SI
     
RET    
Check_First_char ENDP
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


Check_Register_Addressing  PROC NEAR
       
       XOR AX,AX
       XOR BX,BX
       XOR CX,CX
       XOR DX,DX
       
       
       
       CMP sizeOfOperandN,2
       JNE This_isnt_register  
       MOV DI,OFFSET all_Registers_stringPlayer1
       MOV AL,operandN
       MOV AH, operandN[1]
       MOV CX,16
       REPNE SCASW 
       MOV AX, axPlayerN
       CMP CX,8
       JL  It_is_8bits
       
       MOV isOperandN8bits,0
       
       CMP CX,15
       JNE NEXT14
       
       
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
     
    
NEXT14:
       CMP CX,14
       JNE NEXT13

       Add Ax,2h 
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT13:       
       CMP CX,13
       JNE NEXT12
        
       Add AX,4h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT12: 
       CMP CX,12
       JNE NEXT11
        
       Add AX,6h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT11:
       CMP CX,11
       JNE NEXT10
        
       Add AX,8h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
NEXT10:
       CMP CX,10
       JNE NEXT9
        
       Add AX,0Ah
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
         
         
NEXT9:
       CMP CX,9
       JNE NEXT8
        
       Add AX,0Ch
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       

NEXT8:
       CMP CX,8
       JNE It_is_8bits
        
       Add AX,0Eh
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
It_is_8bits:       
       
       MOV isOperandN8bits,1
       MOV AX,axPlayerN
       CMP CX,7
       JNE NEXT6
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
     
    
NEXT6:
        CMP CX,6
        JNE NEXT5
        
       ADD AX,1h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT5:       
       CMP CX,5
       JNE NEXT4
        
       ADD AX,2h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT4: 
       CMP CX,4
       JNE NEXT3
        
       ADD AX,3h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT3:
       CMP CX,3
       JNE NEXT2
        
       ADD AX,4h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
NEXT2:
       CMP CX,2
       JNE NEXT1
        
       ADD AX,5h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
         
         
NEXT1:
       CMP CX,1
       JNE NEXT0
        
       ADD AX,6h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
        
NEXT0: 
       CMP CX,1
       JNE This_isnt_register
        
       ADD AX,7h
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       
       

OffsetIsFound: JMP Register_Addressing_mode_is_finished     
       
       
This_isnt_register: MOV hasError,1    
       
       

Register_Addressing_mode_is_finished:    
    
RET
Check_Register_Addressing ENDP   




;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;---------------------------------MOV FUNCTION---------------------------------------------

ExecuteMov PROC
    
    CMP isOperand18Bits,1
    JE OneByte1
    
    MOV SI, offsetOperand2
    MOV DI, offsetOperand1
    MOV BX,[SI]
    MOV [DI],BX
    ret
    
  OneByte1:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    MOV [DI],BL
        
ret    
ExecuteMov ENDP    
    
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;-------------------------------- ADD FUNCTION --------------------------------------------
    
ExecuteAdd PROC
    mov bp,carryFlagN
    CMP isOperand18Bits,1
    JE OneByte2
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD [DI],BX
    JC set_carry1
    MOV [bp],byte ptr 0h
    ret
    
  OneByte2:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD [DI],BL
    JC set_carry1
    MOV [bp], byte ptr 0h  
    ret
    
  set_carry1: MOV [bp], byte ptr 01h  
        
ret
ExecuteAdd ENDP    

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;---------------------------------- ADC FUNCTION ------------------------------------------

ExecuteAdc PROC
    mov bp,carryFlagN
    CMP isOperand18Bits,01h
    JE OneByte3
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD BL,[bp]
    ADD [DI],BX
    JC set_carry2
    MOV [bp],byte ptr 0h
    ret
    
  OneByte3:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD BL,[bp]
    ADD [DI],BL
    JC set_carry2
    MOV [bp],byte ptr 0h
    ret
    
  set_carry2: MOV [bp],byte ptr 1h  
        
ret
ExecuteAdc ENDP


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- SUB FUNCTION --------------------------------------------

ExecuteSub PROC
    mov bp,CarryFlagN
    CMP isOperand18Bits,1
    JE OneByte4
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    SUB [DI],BX
    JC set_carry3
    MOV [bp],byte ptr 0h
    ret
    
  OneByte4:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    SUB [DI],BL
    JC set_carry3
    MOV [bp],byte ptr 0h 
    ret
    
  set_carry3: MOV [bp],byte ptr 1h  
        
ret
ExecuteSub ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;-------------------------------- SBB FUNCTION -----------------------------------------

ExecuteSbb PROC
    mov bp,CarryFlagN
    CMP isOperand18Bits,1
    JE OneByte5
    
    


    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD BL,[bp]
    SUB [DI],BX
    JC set_carry4
    MOV [bp],byte ptr 0h
    ret
    
  OneByte5:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD BL,[bp]
    SUB [DI],BL
    JC set_carry4
    MOV [bp],byte ptr 0h 
    ret
    
  set_carry4: MOV [bp],byte ptr 01h  
        
ret    
ExecuteSbb ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- AND FUNCTION -------------------------------------------

ExecuteAnd PROC
    CMP isOperand18Bits,1
    JE OneByte6
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    AND [DI],BX
    ret
    
  OneByte6:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    AND [DI],BL
        
ret
ExecuteAnd ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- OR FUNCTION --------------------------------------------



ExecuteOr PROC
    CMP isOperand18Bits,1
    JE OneByte7
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    OR [DI],BX
    ret
    
  OneByte7:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    OR [DI],BL
    
    
ret    
ExecuteOr ENDP


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------- XOR FUNCTION ---------------------------------------



ExecuteXor PROC
    CMP isOperand18Bits,1
    JE OneByte8
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    OR [DI],BX
    ret
    
  OneByte8:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    OR [DI],BL
    
    
ret    
ExecuteXor ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ PUSH FUNCTION ---------------------------------------

                        ;si =offset memeory+sp 
ExecutePush PROC

     MOV BP,axPlayerN
     Add BP,0fh
    
     ;SUB spPlayer1,1
     ;MOV DI,spPlayer1
     mov si,axPlayerN
     add si,0ch
     mov cx,1
     sub [si],cx

     
     MOV BX,offsetOperandN
     add di,bp
     MOV SI ,di
     MOV AX,[BX]
     MOV [SI],AX
     
     
ret
ExecutePush ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ POP FUNCTION ----------------------------------------

ExecutePop PROC
   
     MOV SI,axPlayerN
     Add SI,0fh 
     mov di,axPlayerN
     add di,0ch
     ADD [DI],SI
     MOV AX,[DI]
     
     MOV [DI],word ptr 0000H    
     MOV BX,offsetOperandN
     MOV [BX],AX
     mov di,axPlayerN
     add di,0ch
     ADD [di],byte ptr 02h
        
ret
ExecutePop ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ INC FUNCTION ----------------------------------------

ExecuteInc PROC
    MOV BX,offsetOperand1
    INC word ptr [BX]
        
ret
ExecuteInc ENDP



;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ DEC FUNCTION ----------------------------------------
      
ExecuteDec PROC
    
    MOV BX,offsetOperand1
    DEC word ptr [BX]
    
ret
ExecuteDec ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ MUL FUNCTION ----------------------------------------

ExecuteMul PROC
    
    CMP isOperand18Bits,1
    JE BYTExBYTE
    
    MOV AX,axPlayer1
    MOV BX,offsetOperand1
    MOV CX,[BX]
    MUL CX
    MOV axPlayer1,AX
    MOV dxPlayer1,DX
    ret
      
 BYTExBYTE:
    MOV AX,axPlayer1
    MOV SI,offsetOperand1
    MOV DL,[SI]
    MUL DL
    MOV axPlayer1,AX
              
    
ret
ExecuteMul ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ DIV FUNCTION ----------------------------------------

ExecuteDiv PROC
    
    CMP isOperand18Bits,1
    JE BYTEdivBYTE
    
    MOV AX,axPlayer1
    MOV DX,dxPlayer1
    MOV BX,offsetOperand1
    MOV CX,[BX]
    DIV CX
    MOV axPlayer1,AX
    MOV dxPlayer1,DX
    ret
      
 BYTEdivBYTE:
    MOV AX,axPlayer1
    MOV BX,offsetOperand1
    MOV DL,[BX]
    DIV DL
    MOV axPlayer1,AX
              

ret
ExecuteDiv ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------



ResetVars proc far
mov operand2Immediate, 0h
mov operand1Immediate, 0h
mov isOperand2Immediate, 0h
mov isOperand1Immediate, 0h
;Load empty
mov hasError,0H
mov hasBrack,0H
mov isNum,0H
mov isChar,0H
mov validAddressing,0H
mov validMem,0H
mov isOperand1Register,0H
mov isOperand2Register,0H
mov isOperand1HasOffset,0H
mov isOperand2HasOffset,0H
mov isOperandNHasOffset,0H
mov isOperand18Bits,0H
mov isOperand28Bits,0H
mov isOperandN8bits,0H
mov Counter,0H
mov isFirstCharLetterOperand1,0H
mov isFirstCharLetterOperand2,0H
mov isFirstCharLetterOperandN,0H
mov isFirstCharNumberOperand1,0H
mov isFirstCharNumberOperand2,0H
mov isFirstCharNumberOperandN,0H
mov instructionFound,0H
mov instructionOpcode,0H

ret
ResetVars endp


ClearAllRegisters proc far
mov si,axPlayerN
mov ax,0h
mov [si],ax
mov [si+2],ax
mov [si+4],ax
mov [si+6],ax
mov [si+8],ax
mov [si+0Ah],ax
mov [si+0Ch],ax
mov [si+0Eh],ax

ret
ClearAllRegisters endp

  
;*

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

ExecuteInstruction PROC far
    
 CMP hasError,1
 jne continueexec
mov hasError,1h
 ret   
    
 continueexec:   
 CMP instructionOpcode,00h
  JE CALL_ExecuteMov    
    
 CMP instructionOpcode,01h
  JE CALL_ExecuteAdd 
 
 CMP instructionOpcode,02h
  JE CALL_ExecuteAdc
 
 CMP instructionOpcode,03h
  JE CALL_ExecuteSub
 
 CMP instructionOpcode,04h
  JE CALL_ExecuteSbb
 
 CMP instructionOpcode,05h
  JE CALL_ExecuteMul
 
 CMP instructionOpcode,06h
  JE CALL_ExecuteAnd
 
 CMP instructionOpcode,07h
  JE CALL_ExecuteOr
 
 CMP instructionOpcode,08h
  JE CALL_ExecuteXor
 
 CMP instructionOpcode,09h
  JE CALL_ExecuteDiv
 
 CMP instructionOpcode,0Ah
  JE CALL_ExecutePush
 
 CMP instructionOpcode,0Bh
  JE CALL_ExecutePop
 
 CMP instructionOpcode,0Ch
  JE CALL_ExecuteInc
 
 CMP instructionOpcode,0Dh
  JE CALL_ExecuteDec
 
 CMP instructionOpcode,0Eh
  JE CALL_ExecuteNop
 
 CMP instructionOpcode,0Fh
  JE CALL_ExecuteClc  
    
 
 CALL_ExecuteMov:
          CALL ExecuteMov 
          RET
 CALL_ExecuteAdd:
          CALL ExecuteAdd
          RET
 CALL_ExecuteAdc:
          CALL ExecuteAdc
          RET
 CALL_ExecuteSub:
          CALL ExecuteSub
          RET
 CALL_ExecuteSbb:
          CALL ExecuteSbb
          RET
 CALL_ExecuteMul:
          CALL ExecuteMul
          RET
 CALL_ExecuteAnd:
          CALL ExecuteAnd
          RET
 CALL_ExecuteOr:
          CALL ExecuteOr
          RET
 CALL_ExecuteXor:
          CALL ExecuteXor
          RET
 CALL_ExecuteDiv:
          CALL ExecuteDiv
          RET
 CALL_ExecutePush:
          CMP isOperand18Bits,1
          JE GiveErrorToThePlayer
          CALL ExecutePush
          RET
 CALL_ExecutePop:
          CALL ExecutePop
          RET
 CALL_ExecuteInc:
          CALL ExecuteInc
          RET
 CALL_ExecuteDec:
          CALL ExecuteDec
          RET
 CALL_ExecuteNop:
          NOP 
          RET
 CALL_ExecuteClc:
          mov si, CarryFlagN
          MOV [si],byte ptr 0h
          RET

GiveErrorToThePlayer: 
         MOV hasError,1
                    
ret    
ExecuteInstruction ENDP 


SetColorPalette proc far		;AL=Color num ... DX=-GRB
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
	pop ax								
	pop bx
	pop cx
	pop dx
	ret
SetColorPalette endp


OpenFile proc far 

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

ReadData proc far
    MOV AH,3Fh
    MOV BX, [Filehandle]
    MOV CX,ImageWidth*ImageHeight ; number of bytes to read
    LEA DX, ImageData
    INT 21h
    RET
ReadData ENDP 


CloseFile proc far
	MOV AH, 3Eh
	MOV BX, [Filehandle]

	INT 21h
	RET
CloseFile ENDP

Halt proc far 
    HLT  
Halt endp	


P2_main_screen proc far
    mov ax,0013h
    int 10h  ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di
    
    mov ah,9
    mov dx,offset please
    int 21h
    
    mov ah,0ah
    mov dx, offset namePlayer2
    int 21h
    
    mov ah,2
    mov dx,0300h ;Moving the cursor
    int 10h  
    
    mov ah,9
    mov dx, offset initial
    int 21h
    
    mov ah,0ah
    mov dx, offset scoreLabelPlayer2
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    mov ah,2
    mov dx,0600h ;Moving the cursor
    int 10h  
    
    mov ah,9
    mov dx, offset forbiddenCharLabel
    int 21h
    
    mov ah,0ah
    mov dx, offset forbiddenTempChar2
    int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    mov ah,2
    mov dx,0B10h ;Moving the cursor
    int 10h
    
    mov ah,9
    mov dx, offset key
    int 21h
    
is2Enter:
    mov ah,0
    int 16h
    cmp ah,1ch ;Enter key scan code
    Jnz is2Enter
    jz  itisEnter2
itisEnter2:
    call far ptr main_main_screen
ret   
P2_main_screen endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main_main_screen proc far
    
    mov ax,0013h
    int 10h  ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    mov ah,2
    mov dx,051Ch ;Moving the cursor
    mov bh,0
    int 10h
    
    mov ah,9
    mov dx,offset StartChatting
    int 21h
    
    mov ah,2
    mov dx,081Ch ;Moving the cursor
    mov bh,0
    int 10h
    
    mov ah,9
    mov dx,offset StartGame
    int 21h
    
    mov ah,2
    mov dx,0B1Ch ;Moving the cursor
    mov bh,0
    int 10h
    
    mov ah,9
    mov dx,offset EndGame
    int 21h
    
isKey:
    mov ah,0
    int 16h
    cmp ah,01h ;ESC scan code
    JZ  Hat
    Jnz isF2
    
isF2:
    cmp ah,3Ch ;F2 scan code
    Jnz isKey
    call far ptr Assumptions
    RET
HAT:
    call far ptr Halt
ret   
main_main_screen endp

Assumptions proc far
    mov ax,0013h
    int 10h  ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    mov ah,2
    mov dx,0500h ;Moving the cursor
    int 10h
    
    mov ah,9
    mov dx,offset Assump
    int 21h
    
    mov ah,2
    mov dx,0B10h ;Moving the cursor
    int 10h
    
    mov ah,9
    mov dx, offset key
    int 21h
    
is3Enter:
    mov ah,0
    int 16h
    cmp ah,1ch ;Enter key scan code
    Jnz is3Enter
    jz  itisEnter3
    
itisEnter3:
    call far ptr Level
ret    
Assumptions endp

Level proc far
    mov ax,0013h
    int 10h  ;clear screen 10h/ah=0
    MOV AX,0003H
    int 10h
    
    mov ah,2
    mov dx,0500h ;Moving the cursor
    int 10h
    
    mov ah,9
    mov dx,offset LevelSelect
    int 21h
    
isNumberr:
    mov ah,0
    int 16h
    cmp al,31h ;1 ASCII code
    JZ Level1
    cmp aL,32h ;2 ASCII code
    jz Level2
    Jnz isNumberr
    
Level1:
	call far ptr UpdateGui
    
    ret
Level2:    
    
ret    
Level endp
    

 
 UpdateGui proc far 
   	
	; mov ax,0013h
    ; int 10h
    ; MOV AX,0003H
    ; int 10h
	; mov ax,0013h
    ; int 10h  ;clear screen 10h/ah=0

    mov ah,06
    MOV BH,07
    mov al,00
    xor cx,cx
    ;mov dh,0c8h
    ;mov dl,140h
    MOV DX,184FH
    int 10h

	call far ptr GUI



RET 
UpdateGui ENDP 

GUI PROC FAR
    ;MOV AX , @DATA
    ;MOV DS , AX
	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di

    ;Change the current mode to graphics mode (VGA 320X200 PIXEL)
    MOV AH, 0   
    MOV AL, 13h 
    INT 10h
	mov si,offset Palette

	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	
	Paletteagain:
	mov dx,[si]
	call far ptr SetColorPalette ;Set Color AL to DX (-GRB)
	inc si			;Move down two bytes
	inc si
	inc ax
	cmp ax,16		;Are we done?
	jnz Paletteagain
	
    call far ptr OpenFile
    call far ptr ReadData
	
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


; 	 xor ax,ax 
; 	 xor cx,cx 
; 	 xor dx,dx 

call far ptr CloseFile
	
    
	 ;HexToDec scorePlayer1

	XOR AX,AX ;clear ax
	XOR BX,BX ;clear bx
	XOR CX,CX ;clear cx
	XOR DX,DX ;clear dx
	xor si,si
	xor di,di
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
    
; 	 mov ah,86h
; 	 mov     cx, 4Ch    ;    004C4B40h = 5,000,000
;  mov     dx, 4B40h
; 	 int 15h 
; 	 xor ax,ax 
; 	 xor cx,cx 
; 	 xor dx,dx 

	DisplayName 20,21,scoreLabel
	
; 	 mov ah,86h
; 	 mov     cx, 4Ch    ;    004C4B40h = 5,000,000
; mov     dx, 4B40h
; 	 int 15h 
; 	 xor ax,ax 
; 	 xor cx,cx 
; 	 xor dx,dx 
	DisplayRegister 0,21,scoreLabel,6

	
; 	 mov ah,86h
; 	 mov     cx, 4Ch    ;    004C4B40h = 5,000,000
; mov     dx, 4B40h
; 	 int 15h 
; 	 xor ax,ax 
; 	 xor cx,cx 
; 	 xor dx,dx 

;;;;;;;;;;;;;;;;;;;;;;;Name;;;;;;;;;;;;;;;;;;;;;

	;DisplayRegister 3,11,namePlayer1,7
	;DisplayRegister 23,11,namePlayer2,7

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

	
 
   
   
;;;;;;;;;;;;;;;;;;;;;;;Name;;;;;;;;;;;;;;;;;;;;;
	displayName 3,11,namePlayer1+2
	displayName 23,11,namePlayer2+2

;;;;;;;;;;;;;;;;;;;;;;;Score;;;;;;;;;;;;;;;;;;;;;
	displayName 6,21,scoreLabelPlayer1+2
	displayName 26,21,scoreLabelPlayer2+2
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

	displayName 16,16,cfLabel

     	 xor ax,ax
           	 xor cx,cx
                xor si,si
                xor di,di
                xor bx,bx
                xor dx,dx  


	;DisplayWord 16,17,cfPlayer1,1,0Fh,BOL_TABL
    ;MOV SI,OFFSET cfPlayer1
    MOV CL,4 
    MOV DH,17
    MOV DL,17

    
    
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H 	
	
    add cfPlayer1,30H
    mov al,cfPlayer1
    mov ah,0eh
    MOV bx,0fh
    int 10h

    sub cfPlayer1,30h



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

	displayName 36,16,cfLabel

	;DisplayWord 36,17,cfPlayer2,1,0Fh,BOL_TABL
     xor ax,ax
           	 xor cx,cx
                xor si,si
                xor di,di
                xor bx,bx
                xor dx,dx  


    MOV CL,4 
    MOV DH,17
    MOV DL,37

    
    
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H 	
	
    add cfPlayer2,30H
    mov al,cfPlayer2
    mov ah,0eh
    MOV bx,0fh
    int 10h

    sub cfPlayer2,30h

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

   
	;getString 0,19,operation
	
    
    ; return control to operating system
  ;MOV AH, 0
  ;INT 21H
    
	RET
	
GUI ENDP


; DisplayContents PROC

; DisplayContents ENDP



 END MAIN 
 
