
include ../config.mk

.PHONY: all

all: generator_servo

.PHONY: clean

clean:
	-rm generator_servo.so *.mod *.o

.PHONY: generator_servo

generator_servo: generator_servo.so

generator_servo.o: generator_servo_fcns.o

generator_servo.so: generator_servo.o generator_servo_fcns.o $(MISC_MOD)
	$(FC) $(LFLAGS) $^ -o $@

