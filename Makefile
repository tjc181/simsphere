FC=gfortran
SRC= src
TESTS= tests
PROG=simsphere

$(PROG): $(SRC)

$(SRC):
	$(MAKE) -C $@ $(MAKECMDGOALS)

all: $(PROG)

clean:
	$(RM) *.o *.gch *.mod $(PROG) 

.PHONY: $(SRC)
