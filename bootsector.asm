[bits 16]
[org 0x7c00]

FUNCOFFSET equ 0x8000	;
FUNCSEG    equ 0x0800   ;

call 0x0000:main;
main:
	cli			        ;
	mov ax, 0x9000		;
	mov ss, ax		    ;
	mov bp, 0x0000		;
	mov sp, bp		    ;
	sti			        ;
	mov ax, 0	        ;
	mov ds, ax		    ;
	mov es, ax		    ;
	;; load 4 sectors to 0x0000:FUNCOFFSET

	mov byte[BOOT_DRIVE],dl ; store disk drive id
	mov bx, FUNCOFFSET;
	mov dh, 4		    ;
	call loadDisk		;
	mov si, LOAD_MSG	;
	call printString	;


jump:
	jmp FUNCSEG:0x0000;
	mov ah,0x0e		;
	mov al,'X'		;
	int 0x10		;
	jmp jump		;

loadDisk:
	push dx			;
	mov ah, 0x02		;
	mov al, dh		;
	mov ch, 0x00		;
	mov dh, 0x00		;
	mov cl, 0x02		;
	int 0x13		;
	jc .disk_error		;
	pop dx			;
	cmp dh, al		;
	jne .disk_error		;
	ret			;
	.disk_error:
	mov si, DISK_ERROR_MSG	;
	call printString	;
	jmp $			;

%include "io.asm"

BOOT_DRIVE db 0			;
LOAD_MSG db 'Game loaded...',0	;
DISK_ERROR_MSG db 'Disk error.',0 ;
times 510-($-$$) db 0		;
dw 0xaa55			; end of bootsector.
;;
