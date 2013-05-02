  subroutine dr_init(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  use    driver, only: dr_exit_trigger,dr_integration_mode,&
                       dr_mode_ballistic,dr_mode_mdot,&
                       dr_init_ballistic,dr_init_mdot
  use      IO,   only: IO_write_header,IO_open
  implicit none
  integer, intent(in) :: setup_mode
  real   , intent(in) :: donmass,accmass,invar,donfreq,accfreq
  dr_exit_trigger = .false.
  if (dr_integration_mode.eq.dr_mode_ballistic) then
    call dr_init_ballistic(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  else if (dr_integration_mode.eq.dr_mode_mdot) then
    call dr_init_mdot(setup_mode,donmass,accmass,invar,donfreq,accfreq)
  else
    call dr_abort("dr_init","Invalid integration for main driver")
  end if
  call IO_open
  call IO_write_header 
  call IO_write_header(6)
  return 
  end subroutine dr_init
