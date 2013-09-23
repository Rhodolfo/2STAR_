  function eos_pres_entr(input)
  use eos_vars, only: eos_dens,eos_temp,eos_pres,eos_entr,&
                      eos_dens_unit,eos_temp_unit,eos_pres_unit,eos_entr_unit                    
  use eos_interface, only: eos_helmholtz
  use  dr_interface, only: dr_abort
  implicit none
  real, dimension(:)           :: input
  real, dimension(size(input)) :: eos_pres_entr
  if (size(input).ne.2) call dr_abort("eos_func","Wrong dimension for eos_func")
  eos_dens = input(1)*eos_dens_unit
  eos_temp = input(2)*eos_temp_unit
  call eos_helmholtz
  eos_pres_entr(1) = eos_pres/eos_pres_unit
  eos_pres_entr(2) = eos_entr/eos_entr_unit
  end function eos_pres_entr

  function eos_pres_entr_zero(input)
  use eos_vars, only: eos_dens,eos_temp,eos_pres,eos_entr,&
                      eos_target_pres,eos_target_entr,&
                      eos_dens_unit,eos_temp_unit,eos_pres_unit,eos_entr_unit                    
  use eos_interface, only: eos_helmholtz
  use  dr_interface, only: dr_abort
  implicit none
  real, dimension(:)           :: input
  real, dimension(size(input)) :: eos_pres_entr_zero
  if (size(input).ne.2) call dr_abort("eos_func","Wrong dimension for eos_func")
  eos_dens = input(1)*eos_dens_unit
  eos_temp = input(2)*eos_temp_unit 
  call eos_helmholtz
  eos_pres_entr_zero(1) = eos_pres/eos_pres_unit - eos_target_pres
  eos_pres_entr_zero(2) = eos_entr/eos_entr_unit - eos_target_entr
  end function eos_pres_entr_zero
