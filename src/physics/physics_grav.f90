! Gravity

  function ph_grav_pot(gravpos,gravmass)
  use ph_vars, only: ph_G
  implicit none
  real :: gravmass,gravpos,ph_grav_pot
  ph_grav_pot = - ph_G*gravmass/gravpos
  end function ph_grav_pot

  function ph_grav_acc(gravpos,gravmass)
  use ph_vars, only: ph_G
  use ph_interface, only: ph_norm
  implicit none
  real                           :: gravmass
  real, dimension(:)             :: gravpos
  real, dimension(size(gravpos)) :: ph_grav_acc
  ph_grav_acc = - ph_G*gravmass*(gravpos)/((ph_norm(gravpos))**(3))
  end function ph_grav_acc

  function ph_kepler_period(totmass,separ)
  use ph_vars, only: ph_pi,ph_G
  implicit none
  real :: totmass,separ,ph_kepler_period
  ph_kepler_period = 2d0*ph_pi*sqrt( (separ**3)/(ph_G*totmass) )
  end function ph_kepler_period

  function ph_kepler_separation(totmass,per)
  use ph_vars, only: ph_pi,ph_G
  implicit none
  real :: totmass,per,ph_kepler_separation
  ph_kepler_separation = ( (ph_G*totmass/(4.0d0*(ph_pi**2)))*(per**2) )**(1.0d0/3.0d0)
  end function ph_kepler_separation 

  function ph_kepler_jorb(m1,m2,a)
  use ph_vars, only: ph_G
  implicit none
  real :: m1,m2,a,ph_kepler_jorb
  ph_kepler_jorb = m1*m2*sqrt(ph_G*a/(m1+m2))
  end function ph_kepler_jorb

  function ph_grw_jdot(m1,m2,a)
  use ph_vars, only: ph_G,ph_c
  use ph_interface, only: ph_kepler_jorb
  implicit none
  real :: m1,m2,a,ph_grw_jdot,dummy
  dummy         = (ph_G**(3.d0/5.d0))/ph_c
  dummy         = dummy**5
  ph_grw_jdot   = - (32.0d0/5.0d0)*( dummy )   &
                  * ( (m1*m2*(m1+m2))/(a**4) ) &
                  * ph_kepler_jorb(m1,m2,a)
  end function ph_grw_jdot
