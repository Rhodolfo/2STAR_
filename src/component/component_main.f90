  subroutine cp_init(mode,donmass,accmass,invar,donfreq,accfreq)
  use    driver, only: dr_integration_mode,dr_mode_ballistic,dr_mode_mdot,&
                       dr_setup_mode,dr_eddington,dr_hyper_eddington,dr_abort
  use component, only: cp_donor_mass,cp_accretor_mass,cp_setup_var,&
                       cp_donor_freq,cp_accretor_freq,cp_auxiliary_parameters
  use        IO, only: IO_write_header,IO_2string
  implicit none 
  integer, intent(in) :: mode
  real   , intent(in) :: donmass,accmass,invar,donfreq,accfreq
! Setting up the module
  dr_setup_mode    = mode 
  cp_donor_mass    = donmass
  cp_accretor_mass = accmass
  cp_setup_var     = invar
  cp_donor_freq    = donfreq
  cp_accretor_freq = accfreq
  if (cp_donor_mass.le.0.) then 
    call dr_abort("cp_init","Donor mass is negative = "//trim(adjustl(IO_2string(cp_donor_mass))))
  end if
  if (cp_accretor_mass.le.0.) then 
    call dr_abort("cp_init","Donor mass is negative = "//trim(adjustl(IO_2string(cp_accretor_mass))))
  end if 
  call cp_auxiliary_parameters
  return
  end subroutine cp_init





  subroutine cp_auxiliary_parameters
  use driver   , only: dr_hybrid,dr_perform_ballistic_evolution
  use component, only: cp_star_parameters,cp_binary_parameters,cp_star_parameters
  implicit none
! This routine simply sets the stage for the numerical integration, I define the
! total mass, the mass quotient, the reduced mass and other parameters relevant
! to the test particle trayectory.
! Donor and Accretor parameters, as well as adimensional scalable quantities
  call cp_star_parameters
! Binary system parameters are set here, once a period, separation or mass
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
