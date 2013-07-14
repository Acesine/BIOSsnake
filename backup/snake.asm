;;; Here starts code sectors
[bits 16]
[org 0x8000]
function_start:
	mov ax, 0		;
	mov ds, ax		;
	mov si, GAME_MSG;
	call printString	;

	mov ah, 0x86		;
	mov cx, 0x000f		;
	mov dx, 0x4240		;
	int 0x15		; wait for 1s
	call field		;
	jmp $			;
field:
	call reset		;
	mov ax, 0xb800		;
	mov es, ax		;

	;; set timer routine
	cli			;
	push ds			;
	mov ax, 0x0000		;
	mov ds, ax		;
	mov bx, 0x1c*4		;
	mov word[bx], clockRoutine;
	mov ax, 0		   ;
	mov word[bx+2], ax	   ;
	pop ds			   ;
	sti			   ;
	;;
	;; show a random apple
	call showRandApple	;
	;;
	mov ax, word[snakebox]	;
	mov bx, 0		;
	mov word[es:bx], ax 	;
	.loop:
	cmp byte[status], 0	       ;
	je .reset		;
	mov ah, 0x00		; read a character
	int 0x16		;
	cmp al, 'd'		;
	je .store		;
	cmp al, 'w'		;
	je .store		;
	cmp al, 'a'		;
	je .store		;
	cmp al, 's'		;
	je .store		;
	jmp .loop		;
	.store:
	mov bx, [snakeLen]      ;
	dec bx;
	mov byte[bx+snakeDir], al   ;
	jmp .loop		;
	.reset:
	mov ah, 0x00		;
	int 0x16		;
	cmp al, 'r'		;
	jne .reset		;
	call reset		;
	call showRandApple
	jmp .loop		;
	ret			;
clockRoutine:
	mov ax, cs		;
	mov ds, ax		;
	mov al, byte[delay]	;
	cmp byte[timer], al	;
	je .next		;
	inc byte[timer]		;
	iret			;
	.next:
	cmp byte[status], 1    ;
	jne .return			      ;
	mov ax, word[snakeLen] ;
	dec ax                                ;
	mov si, snakePos	      ;
	mov cx, 4			      ;
	mul cx				      ;
	add si, ax			      ;
	mov ax, word[si]          ;
	mov byte[newBoxFlag], 0;
	cmp word[applePos], ax; reach an apple
	jne .move;
	cmp word[snakeLen], 90;
	jne .o;
	call gamecomplete;
	iret
	.o:
	inc word[appleNum]	;
	call showRandApple	;
    mov byte[newBoxFlag], 1;
	inc word[snakeLen]     ;
	;; add a snake box
	;; set position
	mov ax, word[snakeLen] ;
	dec ax                                ;
	mov si, snakePos	      ;
	mov cx, 4			      ;
	mul cx				      ;
	add si, ax			      ;
	mov bx, word[si-4]		      ;
	mov word[si], bx		      ;
	;; set direction
	mov ax, word[snakeLen] ;
	dec ax;
	mov si, snakeDir	      ;
	mov cx, 1			      ;
	mul cx				      ;
	add si, ax			      ;
	mov bx, word[si-1]		      ;
	mov word[si], bx		      ;
	;; change speed
	mov ax, word[appleNum]	;
	mov dx, 0		;
	mov cx, 5		;
	div cx			;
	cmp dx, 0		;
	jne .move		;
	inc word[level];
	dec byte[delay]		;
	cmp byte[delay], 0	;
	jne .move		;
	mov byte[delay], 1	;

	.move:
    call printInfo;
	mov byte[timer], 0	;

	call moveSnake		;

    mov al, byte[newBoxFlag];
    cmp al, 0;
    jne .return;
    ;; update snake boxes directions
    mov bx, 0;
	.loop: ;copy snakeDir to snakeDirTmp
	cmp bx, [snakeLen];
	je .update;
	mov dl, byte[bx+snakeDir];
	mov byte[bx+snakeDirTmp], dl;
	inc bx;
	jmp .loop;

    .update:
    mov bx, 1;
	.updateloop: ; update directions of snake
	cmp bx, [snakeLen];
	je .return;
	mov dl, byte[bx+snakeDirTmp];
	mov byte[bx-1+snakeDir], dl;
	inc bx;
	jmp .updateloop;

	.return:
	iret			;


moveSnake:
    mov al, byte[newBoxFlag];
    cmp al, 0;
    je .nonewbox;

    mov cx, word[snakeLen] ;
	dec cx;
	mov si, snakeDir		 ;
	mov ax, 1				 ;
	mul cx					 ;
	add si, ax				 ;
	mov al, byte[si]	;
	cmp al, 'd'		;
	je .r;
	cmp al, 'w'		;
	je .u;
	cmp al, 'a'		;
	je .l;
	cmp al, 's'		;
	je .d;

    .r:
    call moveright		;
    ret;
    .u:
    call moveup			;
    ret;
    .l:
    call moveleft		;
    ret;
    .d:
    call movedown		;
    ret;

    .nonewbox:
	mov cx, word[snakeLen] ;
	dec cx;
	.loop:
	cmp cx, 0
	jl .return			      ;
	mov si, snakeDir		 ;
	mov ax, 1				 ;
	mul cx					 ;
	add si, ax				 ;
	mov al, byte[si]	;

	cmp al, 'd'		;
	je .right		;
	cmp al, 'w'		;
	je .up			;
	cmp al, 'a'		;
	je .left		;
	cmp al, 's'		;
	je .down		;

	.right:
	call moveright	;
	dec cx;
    jmp .loop;


	.up:
	call moveup		;
	dec cx			;
    jmp .loop;

	.left:
	call moveleft		;
	dec cx			;
    jmp .loop;

	.down:
	call movedown		;
	dec cx			;
    jmp .loop;

	.return:
	ret			;
cls:
	pusha			;
	mov ax, 0xb800		;
	mov es, ax		;
	mov bx, 0		;
	.loop:
	cmp bx, 4000		;
	jz .return		;
	mov ax, word[backgroundbox];
	mov word[es:bx], ax	;
	add bx, 2		;
	jmp .loop		;
	.return:
	popa			;
	ret			;

showRandApple:
	pusha			;
	mov ax, 0xb800		;
	mov es, ax		;

	.loop:
	call rand		;
	mov ax, cx		;
	mov dx, 0		;
	mov cx, 80		;
	div cx			;
	mov bx, dx		;
	call rand		;
	mov ax, cx		;
	mov dx, 0		;
	mov cx, 25		;
	div cx			;
	mov ax, dx		;

	mov cx, 80		;
	mul cx			;
	add ax, bx		;
	mov cx, 2		;
	mul cx			;
	mov bx, ax		;
	mov ax, word[es:bx]		      ;
	cmp ax, word[snakebox] ;
	je .loop		;

	mov ax, word[applebox];
	mov word[es:bx], ax;
	mov word[applePos], bx	; store current apple position

	popa			;
	ret			;

rand:
	;; r_k = (multiplier*r_k-1 + increment) mod modulus
	;; r_k-1 is stored in [seed], using time as seed
	;; modulus=65535, multiplier=31821, increment=13849
	;; result range (0-cx)
	;; return value in cx
	push ax			;
	push bx			;
	push dx			;
	mov ah, 0x00		; read time
	int 0x1a		;
	mov word[seed], dx	;
	mov ax, 31821		;
	mul word[seed]	;
	add ax, 13849		;

	mov word[seed], ax	;
	mov cx, ax				;
	pop dx			;
	pop bx			;
	pop ax			;
	ret			;

gameover:
	cmp byte[status], 1  ;
	jne .return			    ;
	mov si, GAMEOVER_MSG;
	call printString		   ;
	mov byte[status], 0 ;
	.return:
	ret			;
gamecomplete:
	cmp byte[status], 1  ;
	jne .return			    ;
	mov si, SNAKEFULL_MSG;
	call printString		   ;
	mov byte[status], 0 ;
	.return:
	ret			;
reset:
	call cls				;
	;; set cursor position: (10,20)
	mov ah, 0x02				;
	mov bh, 0				;
	mov dh, 10				;
	mov dl, 20				;
	int 0x10				;
	;; reset variables
	mov byte[level], 0;
	mov word[seed], 0 ;
	mov word[applePos], 0	     ;
	mov word[appleNum], 0	     ;
	mov byte[timer], 0	     ;
	mov byte[delay], 2	     ;
	mov byte[snakeDir], 'd'     ;
	mov word[snakePos], 0	     ;
	mov byte[status], 1   ;
	mov word[snakeLen], 1 ;

	ret			     ;
moveup:
	;; move box located in [es:bx] upward
	;; set bx as current pos
	;; cx is counter of snake blocks
	pusha			;
	mov si, snakePos		 ;
	mov ax, 4				 ;
	mul cx					 ;
	add si, ax				 ;
	mov bx, word[si];

	mov ax, 0xb800		;
	mov es, ax		;
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

    mov al, byte[newBoxFlag];
    cmp al, 1;
    je .norecover;
	mov ax, word[backgroundbox]	;
	mov word[es:bx], ax	;
	.norecover:
	cmp bx, 160		;
	jge .next		;
	add bx, 160		;
	call gameover		;
	jmp .over		;
	.next:
	sub bx, 160		;
	;; test if collide
	mov ax, word[es:bx];
	cmp ax, word[snakebox];
	jne .set;
	call gameover;
	jmp .over;
	.set:
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov word[si], bx	;
	.over:
	popa			;
	ret			;

movedown:
	;; move box located in [es:bx] downward
	;; cx is counter of snake blocks
	pusha			;
	mov si, snakePos		 ;
	mov ax, 4				 ;
	mul cx					 ;
	add si, ax				 ;
	mov bx, word[si];

	mov ax, 0xb800		;
	mov es, ax		;
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov al, byte[newBoxFlag];
    cmp al, 1;
    je .norecover;
	mov ax, word[backgroundbox]	;
	mov word[es:bx], ax	;
	.norecover:
	add bx, 160		;
	cmp bx, 4000		;
	jl .next		;
	sub bx, 160		;
	call gameover		;
	jmp .over		;
	.next:
	;; test if collide
	mov ax, word[es:bx];
	cmp ax, word[snakebox];
	jne .set;
	call gameover;
	jmp .over;
	.set:
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov word[si], bx	;
	.over:
	popa			;
	ret			;

moveleft:
	;; move box located in [es:bx] leftward
	;; cx is counter of snake blocks
	pusha			;
	mov si, snakePos		 ;
	mov ax, 4				 ;
	mul cx					 ;
	add si, ax				 ;
	mov bx, word[si];

	mov ax, 0xb800		;
	mov es, ax		;
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov al, byte[newBoxFlag];
    cmp al, 1;
    je .norecover;
	mov ax, word[backgroundbox]	;
	mov word[es:bx], ax	;
	.norecover:
	mov dx, 0		;
	mov ax, bx		;
	mov cx, 160		;
	div cx			;
	cmp dx, 0		;
	jne .next		;
	add bx, 2		;
	call gameover		;
	jmp .over		;
	.next:
	sub bx, 2		;
	;; test if collide
	mov ax, word[es:bx];
	cmp ax, word[snakebox];
	jne .set;
	call gameover;
	jmp .over;
	.set:
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov word[si], bx	;
	.over:
	popa			;
	ret			;

moveright:
	;; move box located in [es:bx] rightward
	;; cx is counter of snake blocks
	pusha			;
	mov si, snakePos		 ;
	mov ax, 4				 ;
	mul cx					 ;
	add si, ax				 ;
	mov bx, word[si];

	mov ax, 0xb800		;
	mov es, ax		;
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;

	mov al, byte[newBoxFlag];
    cmp al, 1;
    je .norecover;
	mov ax, word[backgroundbox]	;
	mov word[es:bx], ax	;
    .norecover:
	mov dx, 0		;
	mov ax, bx		;
	mov cx, 160		;
	div cx			;
	cmp dx, 158		;
	je .over		;
	add bx, 2		;
	;; test if collide
	mov ax, word[es:bx];
	cmp ax, word[snakebox];
	jne .set;
	call gameover;
	jmp .over;
	.set:
	mov ax, word[snakebox]	;
	mov word[es:bx],ax	;
	mov ax, bx		;

	mov word[si], bx	;
	popa			;
	ret			;
	.over:
	call gameover		;
	popa			;
	ret			;

printInfo:
    pusha;
    ;; set cursor position: (1,70)
	mov ah, 0x02				;
	mov bh, 0				;
	mov dh, 1				;
	mov dl, 70				;
	int 0x10				;
	mov si, SCORE_STR;
	call printString;
    mov cx, word[appleNum];
	call printInt;
	;; set cursor position: (3,70)
	mov ah, 0x02				;
	mov bh, 0				;
	mov dh, 3				;
	mov dl, 70				;
	int 0x10				;

	mov si, LEVEL_STR;
	call printString;
	mov cx, word[level];
	call printInt;
    ;; set cursor position: (10,20)
	mov ah, 0x02				;
	mov bh, 0				;
	mov dh, 10				;
	mov dl, 20				;
	int 0x10				;

	popa;
	ret;

%include "io.asm"

seed dw 0			;
applePos dw 0			;
appleNum dw 1			;
level dw 1;

applebox dw 0xcc20	;
snakebox dw 0x1120		;
backgroundbox dw 0x7020		;
timer db 0			;
delay db 2			;

status db 0			;

snakePos dw 0			;
	times 100 dw -1		;
snakeDir db 'd'			;d:right, w:up, a:left, s:down e: end
	times 100 db 'e'
snakeDirTmp db 'd'			;d:right, w:up, a:left, s:down e: end
	times 100 db 'e'
newBoxFlag db 0
snakeLen dw 1			;

SCORE_STR db "Score: ",0;
LEVEL_STR db "Level: ",0;
GAME_MSG db 'Game starts...',0	;
GAMEOVER_MSG db "Game Over!(Double press 'r' to try again)",0	;
SNAKEFULL_MSG db "Snake is full!(Double press 'r')", 0;
times 2048-($-function_start) db 0		;
