  subroutine dr_perform_ballistic_evolution(setmode,dm,am,invar,donfreq,accfreq)
! Performs an RG4 integration to obtain the path of a test particle along the
! full Roche+Coriolis field, thrown from the L1 point as described by Lubow and
! Shu.
  use dr_vars, only: dr_time,dr_time_step,dr_time_tolerance,dr_time_step_tolerance,&
                     dr_res_factor,dr_screen_counter,dr_force_counter,&
                     dr_step_counter,dr_file_counter,&
                     dr_mode_ballistic,dr_mode_mdot,dr_exit_trigger,&
                     dr_integration_mode,dr_phase_point_old,dr_phase_point_new,&
                     dr_interrupting,dr_stop_on_hit,dr_setup_mode
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_don_freq,cp_acc_freq,cp_setup_var,&
                     cp_env_mass
  use IO_vars, only: IO_save,IO_verb
  use dr_interface, only: stream_source_function,dr_init
  use IO_interface, only: IO_log
  use ode         , only: ode_rk4_step
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
    dr_setup_mode  = setmode
    cp_don_mass    = dm
    cp_acc_mass    = am
    cp_setup_var   = invar
    cp_don_freq    = donfreq
    cp_acc_freq    = accfreq
  else
    call IO_log("dr_perform_ballistic_evolution has been called, using native module variables")
  end if
    call dr_init(dr_setup_mode,cp_don_mass,cp_acc_mass,cp_env_mass,&
                  cp_setup_var,cp_don_freq,cp_acc_freq)
! Evolution loop begins here
    do while ( ((dr_time.lt.dr_time_tolerance).and.(.not.dr_exit_trigger)) )
      call dr_interrupt
      if (dr_exit_trigger) exit
      call ode_rk4_step(dr_time,dr_time_step,&
                        dr_phase_point_old,dr_phase_point_new,&
                        stream_source_function)
      call dr_update
    end do
  return
  end subroutine dr_perform_ballistic_evolution
