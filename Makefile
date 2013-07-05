FILE=snake
COUNT=1

all: $(FILE).img

$(FILE).img: $(FILE).asm
	nasm -f bin $(FILE).asm -o $(FILE).img

run: $(FILE).img
	qemu $(FILE).img

clean:
	rm $(FILE).img

usb: $(FILE).img
	sudo dd if=./$(FILE).img of=/dev/sdb bs=512