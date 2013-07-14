FILE=snake

all: snake.img

snake.img: snake.bin bootsector.bin
	cat bootsector.bin snake.bin > snake.img

bootsector.bin: bootsector.asm
	nasm -f bin bootsector.asm -o bootsector.bin

snake.bin: snake.asm
	nasm -f bin snake.asm -o snake.bin

run: $(FILE).img
	make clean
	make
	qemu $(FILE).img

clean:
	rm $(FILE).img *.bin

usb: $(FILE).img
	sudo dd if=./$(FILE).img of=/dev/sdb bs=512
