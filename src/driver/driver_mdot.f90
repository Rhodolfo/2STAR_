  subroutine dr_perform_mdot_evolution(setmode,dm,am,invar,donfreq,accfreq)
! Integrates the evolution equations for the period, masses and
! frequencies (if tides are included).
  use rho_ode_solver, only: rho_bs_step,rho_error_control,rho_dt_suggested
  use driver,         only: dr_init,&
                            dr_time,dr_time_tolerance,dr_time_step,&
                            dr_exit_trigger,dr_interrupt,dr_update,&
                            dr_mdot_vector_old,dr_mdot_vector_new,&
                            mdot_source_function,dr_setup_mode
  use component,      only: cp_donor_mass,cp_accretor_mass,cp_donor_freq,&
                            cp_accretor_freq,cp_setup_var,cp_binary_separation
  use        IO,      only: IO_log,IO_verb
  implicit none
  integer, optional :: setmode
  real,    optional :: dm,am,invar,donfreq,accfreq
  logical :: DUMVERB,optcheck
! Optional argument handling
  optcheck = present(setmode).and.present(dm).and.present(am).and.present(invar)&
        .and.present(donfreq).and.present(accfreq)
  if (optcheck) then
    call IO_log("[driver] dr_perform_mdot_evolution has been called, using optional argument as passed to the routine")
    dr_setup_mode    = setmode
    cp_donor_mass    = dm
    cp_accretor_mass = am
    cp_setup_var     = invar
    cp_donor_freq    = donfreq
    cp_accretor_freq = accfreq
  else
    call IO_log("[driver] dr_perform_mdot_evolution has been called, using the default module variables")
  end if
! Now let's call the initializer 
  call IO_log("[driver] Initializing variables through dr_init")
  call dr_init(dr_setup_mode,&
               cp_donor_mass,cp_accretor_mass,&
               cp_setup_var,cp_donor_freq,cp_accretor_freq)
  call IO_log("[driver] Variables initialized through dr_init")
! Evolution loop begins here
  call IO_log("[driver] Beginning time evolution in dr_perform_mdot_evolution")
  DUMVERB = IO_verb
  IO_verb = .false.
  do while ( (dr_time.lt.dr_time_tolerance).and.(.not.dr_exit_trigger) )
    call dr_interrupt
    if (dr_exit_trigger) exit
    call rho_bs_step(dr_time,dr_time_step,&
                    dr_mdot_vector_old,dr_mdot_vector_new,&
                    mdot_source_function)
    call dr_update
  end do
  call IO_log("[driver] Loop is done")
  IO_verb = DUMVERB
  end subroutine dr_perform_mdot_evolution
