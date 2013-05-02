function ph_wd_radius(mass_value,composition)
  use   physics, only: solar_mass, chandra_mass, solar_radius
  implicit none
  real    :: mass_value
  real    :: referen_mass
  real    :: chandra_quot
  real    :: referen_quot
  real    :: ph_wd_radius
  character(len=2) :: composition

! For a Helium White Dwarf
  if (composition.eq."he") referen_mass = 5.66E-4*solar_mass
! For a Carbon Oxygen White Dwarf
  if (composition.eq."co") referen_mass = 1.44E-3*solar_mass
  chandra_quot = mass_value / chandra_mass
  referen_quot = mass_value / referen_mass
! Dealing with super chandrashekar masses
  if (mass_value/chandra_mass.ge.1.) then
    ph_wd_radius = 0.
    return
  end if
! On with the fun
  ph_wd_radius = 0.0114 * ( ( (chandra_quot**(-2.0/3.0)) - (chandra_quot**(2.0/3.0)) )**(1.0/2.0) ) &
                              * ( ( 1.0 + 3.5 * (referen_quot**(-2.0/3.0)) + (referen_quot**(-1.0)) )**(-2.0/3.0) )
  ph_wd_radius = solar_radius * ph_wd_radius
return
end function ph_wd_radius



function ph_wd_zeta(mass_value,composition)
  use   physics, only: solar_mass, chandra_mass
  implicit none
  real :: mass_value
  real :: referen_mass
  real :: chandra_quot
  real :: referen_quot
  real :: factor_1
  real :: factor_2
  real :: ph_wd_zeta
  character(len=2) :: composition
! For a Helium White Dwarf
  if (composition.eq."he") referen_mass = 5.66E-4*solar_mass
! For a Carbon Oxygen White Dwarf
  if (composition.eq."co") referen_mass = 1.44E-3*solar_mass
  chandra_quot = mass_value / chandra_mass
  referen_quot = mass_value / referen_mass
  if (mass_value/chandra_mass.ge.1.) then
    ph_wd_zeta = 0.
    return
  end if
! On with the fun
  factor_1 = (1.0+chandra_quot**(4.0/3.0)) / (1.0-chandra_quot**(4.0/3.0))
  factor_2 = (1.0+(7.0/3.0)*(referen_quot**(1.0/3.0)) ) / ( 1.0+(7.0/2.0)*(referen_quot**(1.0/3.0))+referen_quot) 
  factor_1 = factor_1 / 3.0
  factor_2 = 2.0*factor_2 / 3.0
  ph_wd_zeta = - factor_1 + factor_2
return
end function ph_wd_zeta



function ph_wd_k_factor(mass_value)
  use   physics, only: solar_mass
  implicit none
  real    :: mass_value
  real    :: ph_wd_k_factor
  real    :: a = 0.1939
  real    :: b = 1.44885
  real    :: c = 0.1917 
  ph_wd_k_factor = a*((b-mass_value/solar_mass)**c)
return
end function ph_wd_k_factor





function ph_wd_k_zeta(mass_value)
  use   physics, only: solar_mass
  implicit none
  real    :: mass_value
  real    :: ph_wd_k_zeta
  real    :: b = 1.44885
  real    :: c = 0.1917
  ph_wd_k_zeta = - mass_value*c / (b-mass_value)
return
end function ph_wd_k_zeta
