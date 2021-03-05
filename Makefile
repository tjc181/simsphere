ifndef SIMHOME
override SIMHOME=.
endif

ifndef BUILDROOT
override BUILDROOT=$(SIMHOME)/build
endif

all: 
	$(SIMHOME)/scripts/build.sh

clean:
	@echo "Cleaning build directory: $(BUILDROOT)"
	rm -rf $(BUILDROOT)

.PHONY: all
