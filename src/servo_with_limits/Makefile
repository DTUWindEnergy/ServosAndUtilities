
include ../config.mk

.PHONY: all

all: servo_with_limits

.PHONY: clean

clean:
	-rm servo_with_limits.so *.mod *.o

.PHONY: servo_with_limits

servo_with_limits: servo_with_limits.so

servo_with_limits.o: servo_with_limits_data.o rk45.o

servo_with_limits.so: servo_with_limits.o servo_with_limits_data.o rk45.o $(MISC_MOD)
	$(FC) $(LFLAGS) $^ -o $@
