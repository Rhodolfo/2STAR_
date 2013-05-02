function stream_source_function(some_phase_point,time_value)
  use component, only: cp_donor_mass,cp_donor_position,&
                       cp_accretor_mass,cp_accretor_position,&
                       cp_binary_frequency
  use physics,   only: ph_roche_acc,ph_coriolis2d,ph_grav_acc
! This is the source function for the Runge-Kutta time integrator, I go into
! phase space in order to reduce the problem to a first order linear ODE system. 
! The first two points of the phase space point are the position and the last
! two store the velocity.
 
  implicit none
  real :: time_value
  real, dimension(:) :: some_phase_point
  real, dimension(size(some_phase_point)) :: stream_source_function
  real, dimension(2) :: dummy_position
  real, dimension(2) :: dummy_velocity
  real, dimension(2) :: dummy_vector

! Separating phase space variables
  dummy_position(1:2) = some_phase_point(1:2)
  dummy_velocity(1:2) = some_phase_point(3:4)

! Calculating sources
  stream_source_function(1:2) = dummy_velocity(1:2)
  dummy_vector = ph_roche_acc(dummy_position,&
                 cp_donor_position,cp_accretor_position,&
                 cp_donor_mass,cp_accretor_mass,cp_binary_frequency)&
               + ph_coriolis2d(dummy_velocity,cp_binary_frequency)
  stream_source_function(3:4) = dummy_vector(1:2)
return
end function stream_source_function





function mdot_source_function(somevector,sometime)
  use    driver, only: dr_mode_separation,&
                       dr_evo_mode, dr_mode_self_consistent, dr_mode_inconsistent,&
                       dr_include_tides
  use component, only: cp_driver_separation, cp_driver_donor_freq, cp_driver_accretor_freq,&
                       cp_zeta_separation,   cp_zeta_donor_freq  , cp_zeta_accretor_freq, &
                       cp_binary_separation, cp_donor_mass, cp_accretor_mass,&
                       cp_donor_freq, cp_accretor_freq, cp_binary_frequency,&
                       cp_mdot_donor, cp_mdot_accretor, &
                       cp_init 
  implicit none
  real              :: dm,am,se,fd,fa
  real              :: sometime
  real,dimension(:) :: somevector
  real,dimension(size(somevector)) :: mdot_source_function
! Setting up
  se = somevector(1)
  dm = somevector(2)
  am = somevector(3)
  fd = cp_binary_frequency
  fa = cp_binary_frequency
  if (dr_include_tides) fd = somevector(4)
  if (dr_include_tides) fa = somevector(5)
  call cp_init(dr_mode_separation,dm,am,se,fd,fa)
! Calculating sources
  mdot_source_function(1) = (cp_driver_separation+cp_zeta_separation*cp_mdot_donor/cp_donor_mass)*cp_binary_separation
  if (dr_evo_mode.eq.dr_mode_self_consistent) then 
    mdot_source_function(2) = cp_mdot_donor
    mdot_source_function(3) = cp_mdot_accretor
  else if (dr_evo_mode.eq.dr_mode_inconsistent) then
    mdot_source_function(2) = 0.0
    mdot_source_function(3) = 0.0
  else
  end if
  if (dr_include_tides) then 
    mdot_source_function(4) = cp_driver_donor_freq+cp_zeta_donor_freq*cp_mdot_donor/cp_donor_mass
    mdot_source_function(5) = cp_driver_accretor_freq+cp_zeta_accretor_freq*cp_mdot_accretor/cp_accretor_mass
  else
  end if
return
end function mdot_source_function
