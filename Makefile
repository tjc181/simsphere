FC=gfortran
SRC= src
TESTS= tests
PROG=simsphere
BIN= bin

$(PROG): $(SRC)

$(SRC): $(BIN)
	$(MAKE) -C $@ $(MAKECMDGOALS)

$(BIN):
	mkdir $(BIN)

all: $(PROG) 

test: $(BIN)
	$(MAKE) -C $(TESTS) $(MAKECMDGOALS)
	$(BIN)/test

clean:
	$(MAKE) -C $(SRC) $(MAKECMDGOALS)
	$(MAKE) -C $(TESTS) $(MAKECMDGOALS)
	$(RM) *.o *.mod $(BIN)/$(PROG) $(BIN)/test

.PHONY: $(SRC)
