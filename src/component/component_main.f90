  subroutine cp_init(mode,donmass,accmass,envmass,invar,donfreq,accfreq)
  use dr_vars, only: dr_integration_mode,dr_mode_ballistic,dr_mode_mdot,&
                     dr_setup_mode,dr_eddington,dr_hyper_eddington
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_setup_var,&
                     cp_don_freq,cp_acc_freq,cp_env_mass
  use cp_interface, only: cp_auxiliary_parameters
  use io_interface, only: io_write_header,io_2string,io_log
  implicit none 
  integer, intent(in) :: mode
  real   , intent(in) :: donmass,accmass,envmass,invar,donfreq,accfreq
! Setting up the module
  dr_setup_mode = mode 
  cp_don_mass   = donmass
  cp_acc_mass   = accmass
  cp_env_mass   = envmass
  cp_setup_var  = invar
  cp_don_freq   = donfreq
  cp_acc_freq   = accfreq
  if (cp_don_mass.le.0.) then 
    call io_log("[component] Warning: Donor mass is negative = "//trim(adjustl(io_2string(cp_don_mass))))
    return
  end if
  if (cp_acc_mass.le.0.) then 
    call io_log("[component] Warning: Accretor mass is negative = "//trim(adjustl(io_2string(cp_acc_mass))))
  end if 
  call cp_auxiliary_parameters
  return
  end subroutine cp_init





  subroutine cp_auxiliary_parameters
  use dr_vars, only: dr_hybrid
  use dr_interface, only: dr_perform_ballistic_evolution
  use cp_interface, only: cp_star_parameters,cp_binary_parameters,cp_star_parameters
  implicit none
! This routine simply sets the stage for the numerical integration, I define the
! total mass, the mass quotient, the reduced mass and other parameters relevant
! to the test particle trayectory.
! Donor and Accretor parameters, as well as adimensional scalable quantities
  call cp_star_parameters
! Binary system parameters are set here, once a peri, sepa or mass
! transfer rate is given -> Non-scalable parameters
  call cp_binary_parameters
! Stream parameters are set here, mostly used for the test particle evolution
  call cp_stream_parameters
! So, one can do a ballistic evolution to get some extra parameters in this
! case, I don't know how fruitful it is to do so, if the hybrid mode is off,
! it won't even be called
  if (dr_hybrid) then 
    call dr_perform_ballistic_evolution
  end if
! Driver terms and effective exponents for the mdot evolution
  call cp_evolution_coefficients
  return
  end subroutine cp_auxiliary_parameters
