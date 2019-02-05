
CNTRL_DIR=../../../BasicDTUController/src
MISC_MOD_DIR=$(CNTRL_DIR)/dtu_we_controller
MISC_MOD=$(MISC_MOD_DIR)/misc_mod.o

FC=mpif90
FFLAGS=-fPIC -c -I$(MISC_MOD_DIR) -O3
LFLAGS=-L$(MISC_MOD_DIR) -shared -fPIC

%.o: %.f90
	$(FC) $(FFLAGS) $< -o $@

%.o: %.for
	$(FC) $(FFLAGS) $< -o $@

%.so: %.o
	$(FC) $(LFLAGS) $^ -o $@

