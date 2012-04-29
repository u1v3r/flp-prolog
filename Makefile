OUTPUT = xdvors08
SOURCE = main
CC = swipl

build:
	$(CC) -q -o $(OUTPUT) -c $(SOURCE) 

clean:
	rm -f $(OUTPUT)
