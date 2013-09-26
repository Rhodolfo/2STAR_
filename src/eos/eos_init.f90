  subroutine eos_init
  use eos_vars, only: eos_dens,eos_temp 
  use eos_interface, only: eos_helmholtz
  implicit none
  eos_dens = 1e3
  eos_temp = 1e6  
  call eos_helmholtz
  end subroutine eos_init
