.PHONY: clean install

default: clean install

clean:
	rm -rf lisp-systems

install:
	./get-deps.sh monty-hall
