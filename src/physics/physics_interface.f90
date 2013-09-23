  module ph_interface
  interface



! Mathematical functions

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

    subroutine ph_newton_raphson(iguess,fguess,dx,f,ntol,rtol,relax)
    implicit none
    interface
      function f(x)
      implicit none
      real, dimension(:)       :: x
      real, dimension(size(x)) :: f
      end function f
    end interface
    real, dimension(:) :: iguess,fguess,dx
    integer, optional :: ntol
    real   , optional :: rtol
    real   , optional :: relax
    end subroutine ph_newton_raphson

    subroutine ph_newton_raphson_step(gc,gn,dg,f,ce)
    implicit none
    interface
      function f(x)
      implicit none
      real, dimension(:)       :: x
      real, dimension(size(x)) :: f
      end function f
    end interface
    real, dimension(:) :: gc,gn,dg
    real, dimension(:), optional :: ce
    end subroutine ph_newton_raphson_step 



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

  end module ph_interface
