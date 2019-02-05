
.PHONY: all

all: flap_servo generator_servo mech_brake servo_with_limits

.PHONY: flap_servo

flap_servo:
	make -C flap_servo

.PHONY: generator_servo

generator_servo:
	make -C generator_servo

.PHONY: mech_brake

mech_brake:
	make -C mech_brake

.PHONY: servo_with_limits

servo_with_limits:
	make -C servo_with_limits

.PHONY: clean

clean:
	make -C flap_servo clean
	make -C generator_servo clean
	make -C mech_brake clean
	make -C servo_with_limits clean

#filters/
# DONE flap_servo/
# DONE generator_servo/
# DONE mech_brake/
# DONE servo_with_limits/
