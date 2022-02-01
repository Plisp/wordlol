all: clean
	clang wordlol.c -o wordlol -g -fsanitize=address -fsanitize=undefined

clean:
	rm -f *.o wordlol
