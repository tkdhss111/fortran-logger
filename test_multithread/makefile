MKFILE:=-C .. -f makefile

test: caf cafrun

caf:
	caf -cpp ../src/logger_mo.f90 ./unit_test.f90 -I../include -o a

cafrun:
	cafrun -n 4 ./a

clean:
	make clean $(MKFILE)
