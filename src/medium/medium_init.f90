  subroutine medium_init
  use physics, only: nucleon_mass
  use  medium, only: gamma_ism,gamma_bub,kappa,factor,&
                     xi,eta,zeta,A,B,ism_dens
  use     ode, only: ode_ref_scale
  implicit none
! Some fundamental things about the ism and bubble gas
  gamma_ism = 5./3.
  gamma_bub = 4./3.
  kappa     = (1.+gamma_ism)/(gamma_ism-1.)
  factor    = 1.-(1./kappa)
  ism_dens  = nucleon_mass
! Other stuff
  xi   = (1.-(factor**(1./3.)))/(factor**(1./3.))
  eta  = ( factor / (gamma_bub - 1.) ) + ( factor / (kappa*(gamma_ism - 1.)) )
  zeta = 3. * ( (factor / (kappa*(gamma_ism-1.))) + ((factor**2) / 2.) )
! Coefficients for the DE
  A    = 3.*eta + zeta
  B    = 2.*eta + xi
! ODE considerations
  if (allocated(ode_ref_scale)) deallocate(ode_ref_scale)
  allocate(ode_ref_scale(2))
  ode_ref_scale = (/ 1e15, 1e8 /)
  end subroutine medium_init
