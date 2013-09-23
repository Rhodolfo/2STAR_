  module eos_vars
  implicit none
  real :: eos_pres
  real :: eos_dens
  reaL :: eos_temp
  real :: eos_entr
  real :: eos_target_pres
  real :: eos_target_entr
  real, parameter :: eos_pres_unit = 1e22
  real, parameter :: eos_dens_unit = 1e6
  real, parameter :: eos_entr_unit = 1e7
  real, parameter :: eos_temp_unit = 1e6
  logical         :: eos_has_read_table = .false.
  end module eos_vars
