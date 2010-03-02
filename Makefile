all:
	cd docs; make
	cd compiler; make
	cd assembler; make

clean:
	cd docs; make clean
	cd compiler; make clean
	cd assembler; make clean
