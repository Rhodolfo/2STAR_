  subroutine dr_perform_ballistic_evolution(setmode,dm,am,invar,donfreq,accfreq)
! Performs an RG4 integration to obtain the path of a test particle along the
! full Roche+Coriolis field, thrown from the L1 point as described by Lubow and
! Shu.
  use        IO, only: IO_log
  use    driver, only: dr_time,dr_time_step,dr_time_tolerance,dr_time_step_tolerance,&
                       dr_res_factor,dr_screen_counter,dr_force_counter,&
                       dr_step_counter,dr_file_counter,stream_source_function,&
                       dr_mode_ballistic,dr_mode_mdot,dr_hybrid,dr_exit_trigger,&
                       dr_integration_mode,dr_phase_point_old,dr_phase_point_new,&
                       dr_interrupting,dr_stop_on_hit,dr_init,dr_setup_mode
  use component, only: cp_donor_mass,cp_accretor_mass,cp_donor_freq,cp_accretor_freq,cp_setup_var
  use       ode, only: ode_rg4_step
  use        IO, only: IO_save,IO_verb
  implicit none
  integer, optional :: setmode
  real,    optional :: dm,am,invar,donfreq,accfreq 
  real    :: dum_time               
  real    :: dum_time_step
  real    :: dum_time_tolerance
  real    :: dum_time_step_tolerance
  real    :: dum_res_factor
  integer :: dum_screen_counter 
  integer :: dum_force_counter  
  integer :: dum_step_counter  
  integer :: dum_file_counter   
  logical :: dum_save
  logical :: dum_verb
  logical :: dum_trigger
  logical :: optcheck
! Optional argument handling
  optcheck = present(setmode).and.present(dm).and.present(am).and.present(invar)&
        .and.present(donfreq).and.present(accfreq)
  if (optcheck) then
    call IO_log("dr_perform_ballistic_evolution has been called, optional arguments")
    dr_setup_mode    = setmode
    cp_donor_mass    = dm
    cp_accretor_mass = am
    cp_setup_var     = invar
    cp_donor_freq    = donfreq
    cp_accretor_freq = accfreq
  else
    call IO_log("dr_perform_ballistic_evolution has been called, using native module variables")
  end if
! Hybrid mode performs a ballistic evolution alongside an mdot evolution
! These "if" control procedures make sure the mdot evolution parameters are saved,
! and allow for a clean ballistic evolution from within an mdot evolution
! Saving time and time steps, don't want to discard them in hybrid mode
  if (dr_hybrid) then 
    dum_time                = dr_time
    dum_time_step           = dr_time_step
    dum_time_tolerance      = dr_time_tolerance
    dum_time_step_tolerance = dr_time_step_tolerance
    dum_res_factor          = dr_res_factor
    dum_screen_counter      = dr_screen_counter
    dum_force_counter       = dr_force_counter
    dum_step_counter        = dr_step_counter
    dum_file_counter        = dr_file_counter 
    dum_save                = IO_save
    dum_verb                = IO_verb
    dum_trigger             = dr_exit_trigger
! Setting up some flags for the code, hybrid mode runs with these settings
    dr_integration_mode = dr_mode_ballistic
    IO_save             = .false.
    IO_verb             = .false.
    dr_stop_on_hit      = .true.
    dr_interrupting     = .false.
    dr_exit_trigger     = .false.
  else 
  end if
    call dr_init(dr_setup_mode,cp_donor_mass,cp_accretor_mass,&
                  cp_setup_var,cp_donor_freq,cp_accretor_freq)
! Evolution loop begins here
    do while ( ((dr_time.lt.dr_time_tolerance).and.(.not.dr_exit_trigger)) )
      call dr_interrupt
      if (dr_exit_trigger) exit
      call ode_rg4_step(dr_time,dr_time_step,&
                        dr_phase_point_old,dr_phase_point_new,&
                        stream_source_function)
      call dr_update
    end do
! If called within Hybrid mode, return the evolution parameters
  if (dr_hybrid) then 
    dr_time                = dum_time
    dr_time_step           = dum_time_step
    dr_time_tolerance      = dum_time_tolerance
    dr_time_step_tolerance = dum_time_step_tolerance
    dr_res_factor          = dum_res_factor
    dr_screen_counter      = dum_screen_counter
    dr_force_counter       = dum_force_counter
    dr_step_counter        = dum_step_counter
    dr_file_counter        = dum_file_counter 
    dr_integration_mode    = dr_mode_mdot
    dr_exit_trigger        = dum_trigger
    IO_save                = dum_save
    IO_verb                = dum_verb
  else
  end if
  return
  end subroutine dr_perform_ballistic_evolution
