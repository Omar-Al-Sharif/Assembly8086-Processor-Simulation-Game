.model small
.stack 64
.data
VALUE db ?
axPlayer1 dw 0000h
bxPlayer1 dw 0000h
cxPlayer1 dw 0000h
dxPlayer1 dw 0000h
.code
Main proc far
mov ax,@data
mov ds,ax

call RecieveGuiElements
mov si,offset axPlayer1
mov cx,08h
Print:
mov ah,02      
mov dl , [si]
int 21h
inc si
Loop Print 

Main endp 

RecieveGuiElements proc 
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

mov cx,08h
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

end Main
