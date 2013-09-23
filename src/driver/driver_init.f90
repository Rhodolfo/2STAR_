  subroutine dr_init(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  use      dr_vars, only: dr_exit_trigger,dr_integration_mode,&
                          dr_mode_ballistic,dr_mode_mdot,&
                          dr_threshhold_reached
  use dr_interface, only: dr_init_ballistic,dr_init_mdot
  use io_interface, only: io_write_header,io_open
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,envmass,invar,donfreq,accfreq
  dr_threshhold_reached = .false.
  dr_exit_trigger       = .false.
  if (dr_integration_mode.eq.dr_mode_ballistic) then
    call dr_init_ballistic(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  else if (dr_integration_mode.eq.dr_mode_mdot) then
    call dr_init_mdot(setup_mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  else
    call dr_abort("dr_init","Invalid integration for main driver")
  end if
  call IO_open
  call IO_write_header 
  call IO_write_header(6)
  return 
  end subroutine dr_init
