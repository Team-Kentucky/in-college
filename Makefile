COMPILER = cobc
FLAGS = -Isrc/ -x -free

all:
	mkdir -p bin
	$(COMPILER) $(FLAGS) -o bin/inCollege src/inCollege.cob

clean:
	rm -rf bin