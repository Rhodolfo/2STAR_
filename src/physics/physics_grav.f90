! Gravity

  function ph_grav_pot(gravpos,gravmass)
  use physics, only: G
  implicit none
  real :: gravmass,gravpos,ph_grav_pot
  ph_grav_pot = - G*gravmass/gravpos
  end function ph_grav_pot

  function ph_grav_acc(gravpos,gravmass)
  use physics, only: G, ph_norm
  implicit none
  real                           :: gravmass
  real, dimension(:)             :: gravpos
  real, dimension(size(gravpos)) :: ph_grav_acc
  ph_grav_acc = - G*gravmass*(gravpos)/((ph_norm(gravpos))**(3))
  end function ph_grav_acc

  function ph_kepler_period(totmass,separ)
  use physics, only: pi,G
  implicit none
  real :: totmass,separ,ph_kepler_period
  ph_kepler_period = 2*pi*sqrt( (separ**3)/(G*totmass) )
  end function ph_kepler_period

  function ph_kepler_separation(totmass,per)
  use physics, only: pi,G
  implicit none
  real :: totmass,per,ph_kepler_separation
  ph_kepler_separation = ( (G*totmass/(4.*pi*pi))*(per**2) )**(1./3.)
  end function ph_kepler_separation 

  function ph_kepler_jorb(m1,m2,a)
  use physics, only: G
  implicit none
  real :: m1,m2,a,ph_kepler_jorb
  ph_kepler_jorb = m1*m2*sqrt(G*a/(m1+m2))
  end function ph_kepler_jorb

  function ph_grw_jdot(m1,m2,a)
  use physics, only: G,c,ph_kepler_jorb
  implicit none
  real :: m1,m2,a,ph_grw_jdot
  ph_grw_jdot   = - (32./5.)*(G**3.)/ (c**5.) &
                  * (m1*m2*(m1+m2)) / (a**4.) &
                  * ph_kepler_jorb(m1,m2,a)
  end function ph_grw_jdot
