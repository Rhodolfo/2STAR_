module physics
! Mathematical and physical constants
  real, parameter :: pi               = 3.141592
  real, parameter :: c                = 2.998e10
  real, parameter :: G                = 6.67259e-8
  real, parameter :: k_B              = 1.380658e-16
  real, parameter :: a_rad            = 7.5646e-15
  real, parameter :: stefan_boltzmann = 5.6704e-5
  real, parameter :: h                = 6.6260755e-27
  real, parameter :: electron_mass    = 9.1093897e-28
  real, parameter :: nucleon_mass     = 1.6726231e-24
  real, parameter :: solar_mass       = 1.99e+33
  real, parameter :: chandra_mass     = 1.44*solar_mass
  real, parameter :: solar_radius     = 6.96e+10
  real, parameter :: solar_luminosity = 3.9e+33
  real, parameter :: H_0              = 2.0e-18
! Units
  real, parameter :: parsec           = 3.08e18
  real, parameter :: year             = 31556926.0
  real, parameter :: bar              = 1e6

  interface



! Mathmematical functions

    function ph_norm(vec)
    implicit none
    real :: ph_norm
    real, dimension(:) :: vec
    end function ph_norm

    function ph_cross(vec1,vec2)
    implicit none
    real, dimension(3) :: vec1,vec2,ph_cross
    end function ph_cross

    function ph_midpoint_root(func,lowbound,higbound,rtol,ntol)
    implicit none
    interface 
      function func(arg)
      implicit none
      real :: func,arg
      end function func
    end interface
    integer  :: ntol
    real     :: lowbound,higbound,rtol,ph_midpoint_root
    end function ph_midpoint_root



! Gravity

    function ph_grav_pot(gravpos,gravmass)
    implicit none
    real :: gravmass,gravpos,ph_grav_pot
    end function ph_grav_pot

    function ph_grav_acc(gravpos,gravmass)
    implicit none
    real                           :: gravmass
    real, dimension(:)             :: gravpos
    real, dimension(size(gravpos)) :: ph_grav_acc
    end function ph_grav_acc

    function ph_kepler_period(totmass,separ)
    implicit none
    real :: totmass,separ,ph_kepler_period
    end function ph_kepler_period
 
    function ph_kepler_separation(totmass,per)
    implicit none
    real :: totmass,per,ph_kepler_separation
    end function ph_kepler_separation 

    function ph_kepler_jorb(m1,m2,a)
    implicit none
    real :: m1,m2,a,ph_kepler_jorb
    end function ph_kepler_jorb

    function ph_grw_jdot(m1,m2,a)
    implicit none
    real :: m1,m2,a,ph_grw_jdot
    end function ph_grw_jdot



! Interia terms, or ficticious forces

    subroutine ph_center_of_momentum(v1,v2,m1,m2,d)
    implicit none
    real               :: m1,m2
    real, dimension(:) :: v1,v2,d
    end subroutine ph_center_of_momentum

    function ph_cent_pot(pos,ome)
    implicit none
    real :: ome,pos,ph_cent_pot
    end function ph_cent_pot

    function ph_cent_acc(pos,ome)
    implicit none
    real                       :: ome
    real, dimension(:)         :: pos
    real, dimension(size(pos)) :: ph_cent_acc
    end function ph_cent_acc

    function ph_coriolis2d(vec,ome)
    implicit none
    real               :: ome
    real, dimension(2) :: vec, ph_coriolis2d
    end function ph_coriolis2d

    function ph_coriolis3d(vec,ome)
    implicit none
    real, dimension(3) :: vec, ome, ph_coriolis3d
    end function ph_coriolis3d



! Roche field

    function ph_roche_pot(v,v1,v2,m1,m2,ome)
    implicit none
    real, dimension(:) :: v,v1,v2
    real               :: m1,m2,ome,ph_roche_pot
    end function ph_roche_pot

    function ph_roche_acc(v,v1,v2,m1,m2,ome)
    implicit none 
    real, dimension(:)       :: v,v1,v2
    real                     :: m1,m2,ome
    real, dimension(size(v)) :: ph_roche_acc
    end function ph_roche_acc 




! White dwarf quantities

    function ph_wd_radius(mass,comp)
    implicit none
    real :: mass,ph_wd_radius
    character(len=2) :: comp
    end function ph_wd_radius

    function ph_wd_zeta(mass,comp)
    implicit none
    real :: mass,ph_wd_zeta
    character(len=2) :: comp
    end function ph_wd_zeta

    function ph_wd_k_factor(mass)
    implicit none
    real :: mass,ph_wd_k_factor
    end function ph_wd_k_factor

    function ph_wd_k_zeta(mass)
    implicit none
    real :: mass,ph_wd_k_zeta
    end function ph_wd_k_zeta



! Neutron star quantities

    function ph_ns_radius(mass)
    implicit none
    real :: mass,ph_ns_radius
    end function ph_ns_radius

    function ph_ns_zeta(mass)
    implicit none
    real :: mass,ph_ns_zeta
    end function ph_ns_zeta

    function ph_ns_k_factor(mass)
    implicit none
    real :: mass,ph_ns_k_factor
    end function ph_ns_k_factor

    function ph_ns_k_zeta(mass)
    implicit none
    real :: mass,ph_ns_k_zeta
    end function ph_ns_k_zeta



! Nuclear timescales

    function ph_triple_alpha_timescale(temp,dens)
    implicit none
    real :: ph_triple_alpha_timescale,temp,dens
    end function ph_triple_alpha_timescale



! Eggleton things

    function ph_eggleton_formula(q)
    implicit none
    real :: q,ph_eggleton_formula
    end function ph_eggleton_formula

    function ph_eggleton_formula_zeta(q,some_eff)
    implicit none
    real :: q,some_eff,ph_eggleton_formula_zeta
    end function ph_eggleton_formula_zeta

    function ph_eggleton_L1(q)
    implicit none
    real :: q,ph_eggleton_L1
    end function ph_eggleton_L1

    function ph_eggleton_rmin(q)
    implicit none
    real :: q,ph_eggleton_rmin
    end function ph_eggleton_rmin

   function ph_eggleton_rcirc(q)
   implicit none
   real :: q,ph_eggleton_rcirc
   end function ph_eggleton_rcirc



  end interface

end module physics
