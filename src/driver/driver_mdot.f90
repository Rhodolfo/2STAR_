  subroutine dr_perform_mdot_evolution(setmode,dm,am,invar,donfreq,accfreq)
! Integrates the evolution equations for the peri, masses and
! frequencies (if tides are included).
  use     ode, only: ode_bs_step,ode_error_control,ode_dt_suggested,ode_rk4_step
  use dr_vars, only: dr_time,dr_time_tolerance,dr_time_step,&
                     dr_exit_trigger,dr_setup_mode,&
                     dr_mdot_vector_old,dr_mdot_vector_new,dr_include_tides
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_freq,&
                     cp_acc_freq,cp_setup_var,cp_bin_sepa,&
                     cp_env_mass,cp_don_sync_0
  use io_vars, only: io_verb
  use dr_interface, only: dr_init,mdot_source_function,dr_interrupt,dr_update
  use io_interface, only: io_log
  implicit none
  integer, optional :: setmode
  real,    optional :: dm,am,invar,donfreq,accfreq
  logical :: DUMVERB,optcheck
! Optional argument handling and setting envelope mass to zero
  cp_env_mass = 0.
  optcheck = present(setmode).and.present(dm).and.present(am).and.present(invar)&
        .and.present(donfreq).and.present(accfreq)
  if (optcheck) then
    call IO_log("[driver] dr_perform_mdot_evolution has been called, using optional argument as passed to the routine")
    dr_setup_mode = setmode
    cp_don_mass   = dm
    cp_acc_mass   = am
    cp_setup_var  = invar
    cp_don_freq   = donfreq
    cp_acc_freq   = accfreq
  else
    call IO_log("[driver] dr_perform_mdot_evolution has been called, using the default module variables")
  end if
! Now let's call the initializer 
  call IO_log("[driver] Initializing variables through dr_init")
  call dr_init(dr_setup_mode,&
               cp_don_mass,cp_acc_mass,cp_env_mass,&
               cp_setup_var,cp_don_freq,cp_acc_freq)
  call IO_log("[driver] Variables initialized through dr_init")
! Evolution loop begins here
  call IO_log("[driver] Beginning time evolution in dr_perform_mdot_evolution")
  DUMVERB = IO_verb
  IO_verb = .false.
  do while (.not.dr_exit_trigger)
    call dr_interrupt
    if (dr_exit_trigger) exit
    if (.not.dr_include_tides) then 
      call ode_bs_step(dr_time,dr_time_step,&
                       dr_mdot_vector_old,dr_mdot_vector_new,&
                       mdot_source_function)  
    else  
      call ode_bs_step(dr_time,dr_time_step,&
                       dr_mdot_vector_old,dr_mdot_vector_new,&
                       mdot_source_function)   
    end if
    call dr_update
  end do
  call IO_log("[driver] Loop is done")
  IO_verb = DUMVERB
  end subroutine dr_perform_mdot_evolution
