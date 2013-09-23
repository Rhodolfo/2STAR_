  function stream_source_function(some_phase_point,time_value)
  use cp_vars, only: cp_don_mass,cp_don_pos,&
                     cp_acc_mass,cp_acc_pos,&
                     cp_bin_freq
  use ph_interface, only: ph_roche_acc,ph_coriolis2d,ph_grav_acc
! This is the source function for the Runge-Kutta time integrator, I go into
! phase space in order to reduce the problem to a first order linear ODE system. 
! The first two points of the phase space point are the pos and the last
! two store the vel. 
  implicit none
  real :: time_value
  real, dimension(:) :: some_phase_point
  real, dimension(size(some_phase_point)) :: stream_source_function
  real, dimension(2) :: dummy_pos
  real, dimension(2) :: dummy_vel
  real, dimension(2) :: dummy_vector
! Separating phase space variables
  dummy_pos(1:2) = some_phase_point(1:2)
  dummy_vel(1:2) = some_phase_point(3:4)
! Calculating sources
  stream_source_function(1:2) = dummy_vel(1:2)
  dummy_vector = ph_roche_acc(dummy_pos,&
                 cp_don_pos,cp_acc_pos,&
                 cp_don_mass,cp_acc_mass,cp_bin_freq)&
               + ph_coriolis2d(dummy_vel,cp_bin_freq)
  stream_source_function(3:4) = dummy_vector(1:2)
  return
  end function stream_source_function





  function mdot_source_function(somevector,sometime)
  use dr_vars, only: dr_mode_separation,dr_include_tides,&
                     dr_evo_mode,dr_mode_self_consistent, dr_mode_inconsistent,&
                     sepa_var,mdon_var,macc_var,menv_var,fdon_var,facc_var
  use cp_vars, only: cp_driver_sepa,cp_driver_don_freq,cp_driver_acc_freq,&
                     cp_zeta_sepa  ,cp_zeta_don_freq  ,cp_zeta_acc_freq,&
                     cp_bin_sepa, cp_don_mass, cp_acc_mass,&
                     cp_don_freq, cp_acc_freq, cp_bin_freq,&
                     cp_don_mdot, cp_acc_mdot, cp_env_mdot
  use cp_interface, only: cp_init 
  implicit none
  real              :: dm,am,em,se,fd,fa
  real              :: sometime
  real,dimension(:) :: somevector
  real,dimension(size(somevector)) :: mdot_source_function
! Setting up
  mdot_source_function = 0.
  se = somevector(sepa_var)
  dm = somevector(mdon_var)
  am = somevector(macc_var)
  em = somevector(menv_var) 
  fd = cp_bin_freq
  fa = cp_bin_freq
  if (dr_include_tides) fd = somevector(fdon_var)
  if (dr_include_tides) fa = somevector(facc_var)
  call cp_init(dr_mode_separation,dm,am,em,se,fd,fa)
! Calculating sources
! Separation 
  mdot_source_function(sepa_var) = (cp_driver_sepa+cp_zeta_sepa*cp_don_mdot/cp_don_mass)*cp_bin_sepa
! Stellar mnd envelope masses
  if (dr_evo_mode.eq.dr_mode_self_consistent) then 
    mdot_source_function(mdon_var) = cp_don_mdot
    mdot_source_function(macc_var) = cp_acc_mdot
    mdot_source_function(menv_var) = cp_env_mdot
  else if (dr_evo_mode.eq.dr_mode_inconsistent) then
    mdot_source_function(mdon_var) = 0.0
    mdot_source_function(macc_var) = 0.0
    mdot_source_function(menv_var) = 0.0
  else
  end if
! Stellar frequencies
  if (dr_include_tides) then 
    mdot_source_function(fdon_var) = cp_driver_don_freq+cp_zeta_don_freq*cp_don_mdot/cp_don_mass
    mdot_source_function(facc_var) = cp_driver_acc_freq+cp_zeta_acc_freq*cp_acc_mdot/cp_acc_mass
  else
  end if
  return
  end function mdot_source_function
