# Compiler and flags
COMPILER       = gfortran
COMPILER_FLAGS = -Jmod -fdefault-real-8 -fbounds-check -fbacktrace -finit-real=nan

# Executable name and general rule
.SUFFIXES:
.SUFFIXES: .f90 .o 

# General rule
obj/%.o     : %.f90 
	${COMPILER} ${COMPILER_FLAGS} -c -o $@ $<

# Listing obj units
units = driver ode io mkracken component medium physics eos lusolver
umods = ${patsubst %, %.o, ${units}}
VPATH = ${patsubst %, src/%, ${units}}
VPATH += src
 
# Source files from driver unit
driver =\
driver.f90 \
driver_abort.f90 \
driver_ballistic.f90 \
driver_data.f90 \
driver_func.f90 \
driver_init.f90 \
driver_init_ballistic.f90 \
driver_init_mdot.f90 \
driver_interface.f90 \
driver_interrupt.f90 \
driver_interrupt_ballistic.f90 \
driver_interrupt_mdot.f90 \
driver_main.f90 \
driver_mdot.f90 \
driver_post.f90 \
driver_reset.f90 \
driver_restore_defaults.f90 \
driver_stability.f90 \
driver_stability_1.f90 \
driver_stability_2.f90 \
driver_stability_3.f90 \
driver_stability_slaves.f90 \
driver_stop.f90 \
driver_update.f90 \
driver_update_ballistic.f90 \
driver_update_mdot.f90 \
driver_update_tstep.f90 \
driver_vars.f90 
uobjs  += ${patsubst %.f90, %.o ,${driver}}
 
# Source files from ode unit
ode =\
ode.f90 
uobjs  += ${patsubst %.f90, %.o ,${ode}}
 
# Source files from io unit
io =\
io.f90 \
io_data.f90 \
io_file_handling.f90 \
io_header.f90 \
io_interface.f90 \
io_log.f90 \
io_main.f90 \
io_plot.f90 \
io_vars.f90 \
io_write_header_sweep.f90 
uobjs  += ${patsubst %.f90, %.o ,${io}}
 
# Source files from mkracken unit
mkracken =\
mkracken.f90 
uobjs  += ${patsubst %.f90, %.o ,${mkracken}}
 
# Source files from component unit
component =\
component.f90 \
component_L1.f90 \
component_eddington.f90 \
component_envelope.f90 \
component_evol.f90 \
component_getvirtemp.f90 \
component_interface.f90 \
component_main.f90 \
component_mdot.f90 \
component_stars.f90 \
component_stream.f90 \
component_system.f90 \
component_system_slaves.f90 \
component_timescales.f90 \
component_vars.f90 
uobjs  += ${patsubst %.f90, %.o ,${component}}
 
# Source files from medium unit
medium =\
medium.f90 \
medium_Lfunc.f90 \
medium_calc.f90 \
medium_func.f90 \
medium_init.f90 \
medium_main.f90 
uobjs  += ${patsubst %.f90, %.o ,${medium}}
 
# Source files from physics unit
physics =\
physics.f90 \
physics_eggleton.f90 \
physics_grav.f90 \
physics_inertial.f90 \
physics_interface.f90 \
physics_main.f90 \
physics_ns.f90 \
physics_nuclear.f90 \
physics_roche.f90 \
physics_vars.f90 \
physics_wd.f90 
uobjs  += ${patsubst %.f90, %.o ,${physics}}
 
# Source files from eos unit
eos =\
eos.f90 \
eos_cons.f90 \
eos_func.f90 \
eos_helmeos.f90 \
eos_helmholtz.f90 \
eos_interface.f90 \
eos_pretty_eos_out.f90 \
eos_read_helm_table.f90 \
eos_root.f90 \
eos_table_storage.f90 \
eos_vars.f90 \
eos_vector.f90 
uobjs  += ${patsubst %.f90, %.o ,${eos}}
 
# Source files from lusolver unit
lusolver =\
lusolver.f90 
uobjs  += ${patsubst %.f90, %.o ,${lusolver}}
 
# Compile main program 
EXE    = 2STAR_
part   = main.o ${uobjs}
full   = ${patsubst %, obj/%, ${part}}
${EXE}: ${full}
	${COMPILER} -o $@ ${full}
PST    = PAPER_
${PST}: ${full}
	${COMPILER} -o $@ ${full}

# Cleanup
clean: 
	rm -f obj/*.o mod/*.mod ${EXE}
# Listing dependencies
 
# main	
obj/main.o  : obj/driver.o obj/io.o obj/eos.o obj/component.o obj/eos.o
obj/paper.o : obj/driver.o obj/io.o obj/medium.o
 
# driver
obj/driver.o                     : obj/physics.o obj/driver_vars.o obj/driver_interface.o
obj/driver_abort.o               : obj/physics.o obj/component.o obj/io.o
obj/driver_ballistic.o           : obj/driver.o obj/component.o obj/ode.o 
obj/driver_data.o                : obj/driver.o obj/component.o obj/physics.o obj/io.o
obj/driver_func.o                : obj/driver.o obj/physics.o 
obj/driver_init.o                : obj/driver.o obj/io.o 
obj/driver_init_ballistic.o      : obj/driver.o obj/io.o obj/component.o obj/physics.o 
obj/driver_init_mdot.o           : obj/driver.o obj/io.o obj/component.o obj/physics.o obj/ode.o 
obj/driver_interrupt.o           : obj/driver.o obj/component.o 
obj/driver_interrupt_ballistic.o : obj/driver.o obj/component.o obj/io.o 
obj/driver_interrupt_mdot.o      : obj/driver.o obj/component.o obj/physics.o obj/io.o obj/ode.o
obj/driver_main.o                : obj/driver.o obj/io.o 
obj/driver_mdot.o                : obj/driver.o obj/component.o obj/physics.o obj/io.o obj/ode.o
obj/driver_post.o                : obj/driver.o obj/component.o obj/physics.o obj/io.o
obj/driver_reset.o               : obj/driver.o 
obj/driver_restore_defaults.o    : obj/driver.o obj/component.o 
obj/driver_stability.o           : obj/driver.o obj/io.o 
obj/driver_stability_1.o         : obj/driver.o obj/io.o obj/component.o obj/physics.o
obj/driver_stability_2.o         : obj/driver.o obj/io.o 
obj/driver_stability_3.o         : obj/driver.o obj/io.o obj/component.o obj/physics.o
obj/driver_stability_slaves.o    : obj/driver.o obj/io.o obj/component.o obj/physics.o
obj/driver_stop.o                : obj/driver.o obj/io.o
obj/driver_update.o              : obj/driver.o 
obj/driver_update_ballistic.o    : obj/driver.o obj/component.o obj/physics.o 
obj/driver_update_mdot.o         : obj/driver.o obj/component.o obj/physics.o obj/ode.o
obj/driver_update_tstep.o        : obj/driver.o obj/component.o obj/physics.o obj/ode.o obj/io.o
 
# ode
 
# io
obj/io.o                    : obj/io_vars.o obj/io_interface.o obj/mkracken.o 
obj/io_interface.o          : obj/io_vars.o
obj/io_data.o               : obj/io.o 
obj/io_file_handling.o      : obj/io.o 
obj/io_header.o             : obj/io.o obj/driver.o obj/physics.o obj/component.o
obj/io_log.o                : obj/io.o
obj/io_main.o               : obj/io.o 
obj/io_plot.o               : obj/io.o obj/driver.o obj/physics.o obj/component.o 
obj/io_write_header_sweep.o : obj/io.o 
 
# Mkracken
 
# component
obj/component.o               : obj/physics.o   obj/component_vars.o obj/component_interface.o
obj/component_vars.o          : obj/physics_vars.o
obj/component_L1.o            : obj/component.o obj/driver.o obj/physics.o 
obj/component_eddington.o     : obj/component.o obj/driver.o obj/physics.o obj/io.o
obj/component_envelope.o      : obj/component.o obj/driver.o obj/physics.o 
obj/component_evol.o          : obj/component.o obj/driver.o obj/physics.o 
obj/component_getvirtemp.o    : obj/component.o obj/physics.o 
obj/component_main.o          : obj/component.o obj/driver.o obj/io.o
obj/component_mdot.o          : obj/component.o obj/driver.o obj/physics.o obj/component_eddington.o
obj/component_reset.o         : obj/component.o 
obj/component_stars.o         : obj/component.o obj/driver.o obj/physics.o obj/io.o
obj/component_stream.o        : obj/component.o obj/physics.o
obj/component_system.o        : obj/component.o obj/driver.o obj/physics.o obj/io.o obj/component_stars.o obj/component_eddington.o obj/component_system_slaves.o
obj/component_system_slaves.o : obj/component.o obj/driver.o obj/physics.o obj/io.o
obj/component_timescales.o    : obj/component.o obj/driver.o obj/physics.o
 
# medium 
obj/medium_Lfunc.o : obj/medium.o 
obj/medium_calc.o  : obj/medium.o obj/physics.o obj/ode.o obj/io.o
obj/medium_func.o  : obj/medium.o obj/physics.o
obj/medium_init.o  : obj/medium.o obj/physics.o obj/ode.o
obj/medium_main.o  : obj/medium.o obj/io.o
 
# physics
# Declaring variables and interface blocks
obj/physics.o           : obj/physics_vars.o obj/physics_interface.o 
# General dependencies
obj/physics_grav.o      : obj/physics.o
obj/physics_intertial.o : obj/physics.o 
obj/physics_main.o      : obj/physics.o obj/lusolver.o obj/io.o obj/driver.o
obj/physics_ns.o        : obj/physics.o 
onj/physics_wd.o        : obj/physics.o 
obj/physics_roche.o     : obj/physics.o 
 
# EOS
obj/eos.o                : obj/eos_vars.o obj/eos_interface.o
obj/eos_helmeos.o        : obj/eos_vector.o obj/eos_table_storage.o obj/eos_cons.o
obj/eos_helmholtz.o      : obj/eos_vector.o 
obj/eos_pretty_eos_out.o : obj/eos_vector.o 
obj/eos_read_helm_table.o: obj/eos_table_storage.o 
obj/eos_func.o           : obj/eos.o obj/driver.o 
obj/eos_root.o           : obj/eos.o obj/eos_vector.o obj/lusolver.o
 
# LU
 
