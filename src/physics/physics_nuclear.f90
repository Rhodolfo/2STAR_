  function ph_triple_alpha_timescale(temp,dens)
  implicit none
! This function computes the triple alpha burning timescale
! with a certain Temperature and Density.
  real :: ph_triple_alpha_timescale,temp,dens
  real :: t9,d6
  t9 = temp / 1.e9
  d6 = dens / 1.e6
  ph_triple_alpha_timescale = 9.e-4*(t9**3)*exp(4.4/t9)/(d6**2)
  end function ph_triple_alpha_timescale
