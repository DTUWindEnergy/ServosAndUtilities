
include ../config.mk

.PHONY: all

all: mech_brake

.PHONY: clean

clean:
	-rm mech_brake.so *.mod *.o

.PHONY: mech_brake

mech_brake: mech_brake.so

mech_brake.o: mech_brake_fcns.o

mech_brake.so: mech_brake.o mech_brake_fcns.o
	$(FC) $(LFLAGS) $^ -o $@
