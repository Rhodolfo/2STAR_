  function ph_ns_radius(mass_value)
  use   physics, only: solar_mass
  implicit none
  real :: mass_value
  real :: ph_ns_radius
! Just coding up the mass radius relationship for the NS
  ph_ns_radius = 15.12 / ( (mass_value / solar_mass)**(1.0/3.0) )
  ph_ns_radius = 100000.0*ph_ns_radius
  ph_ns_radius = 1.2e6 ! 12 kilometers
  return
  end function ph_ns_radius





  function ph_ns_zeta(mass_value)
  implicit none
  real    :: mass_value
  real    :: ph_ns_zeta
! This is the mass-radius effective exponent
  ph_ns_zeta = 0.
  return
  end function ph_ns_zeta





  function ph_ns_k_zeta(mass_value)
  implicit none
  real    :: mass_value
  real    :: ph_ns_k_zeta
! Moment of inertia coefficient (effective exponent)
  ph_ns_k_zeta = 0.
  return
  end function ph_ns_k_zeta





  function ph_ns_k_factor(mass_value)
  implicit none
  real    :: mass_value
  real    :: ph_ns_k_factor
! Moment of inertia coefficient
  ph_ns_k_factor = 0.2
  return
  end function ph_ns_k_factor
