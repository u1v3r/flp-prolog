OUTPUT = xdvors08
SOURCE = main

build:
	swipl -q -o $(OUTPUT) -c $(SOURCE) 

clean:
	rm -f $(OUTPUT)
