FC=gfortran
SRC= src
PROG=simsphere
BIN= bin

$(PROG): $(SRC)

$(SRC): $(BIN)
	$(MAKE) -C $@ $(MAKECMDGOALS)

$(BIN):
	mkdir $(BIN)

all: $(PROG) 

test: $(BIN)
	$(MAKE) -C tests $(MAKECMDGOALS)
	$(BIN)/test

clean:
	$(RM) *.o *.mod $(BIN)/$(PROG) $(BIN)/test

.PHONY: $(SRC)
