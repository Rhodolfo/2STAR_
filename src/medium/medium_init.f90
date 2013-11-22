  subroutine md_init
  use ph_vars, only: ph_amu
  use md_vars, only: md_gamma_ism,md_gamma_bub,md_kappa,md_factor,&
                     md_xi,md_eta,md_zeta,md_A,md_B,md_ism_dens
  use ode, only: ode_ref_scale
  implicit none
! Some fundamental things about the ism and bubble gas
  md_gamma_ism = 5./3.
  md_gamma_bub = 4./3.
  md_kappa     = (1.+md_gamma_ism)/(md_gamma_ism-1.)
  md_factor    = 1.-(1./md_kappa)
  md_ism_dens  = ph_amu / 1.
! Other stuff
  md_xi   = (1.-(md_factor**(1./3.)))/(md_factor**(1./3.))
  md_eta  = ( md_factor / (md_gamma_bub - 1.) ) + ( md_factor / (md_kappa*(md_gamma_ism - 1.)) )
  md_zeta = 3. * ( (md_factor / (md_kappa*(md_gamma_ism-1.))) + ((md_factor**2) / 2.) )
! Coefficients for the DE
  md_A    = 3.*md_eta + md_zeta
  md_B    = 2.*md_eta + md_xi
! ODE considerations
! if (allocated(ode_ref_scale)) deallocate(ode_ref_scale)
! allocate(ode_ref_scale(2))
! ode_ref_scale = (/ 1e15, 1e8 /)
  end subroutine md_init
