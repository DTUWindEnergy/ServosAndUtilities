
include ../config.mk

.PHONY: all

all: flap_servo

.PHONY: clean

clean:
	-rm flap_servo.so *.mod *.o

.PHONY: flap_servo

flap_servo: flap_servo.so

flap_servo.so: flap_servo.o $(MISC_MOD)
	$(FC) $(LFLAGS) $^ -o $@

