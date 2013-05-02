  module medium
  implicit none



! Properties of the ISM, hot bubble and driving energy
  integer :: jjcurrent
  real    :: gamma_ism,gamma_bub,kappa,factor,ism_dens
  real    :: xi,eta,zeta,A,B
  real, dimension(2)                :: sol,solnew
  real, dimension(36)               :: tem
  real, dimension(:)  , allocatable :: mate,mdte,vesc,dens,mdot,time,posi,taup,mdotdon,mdotedd,mdotacc,Lum,sepa,teff



  interface 


    subroutine medium_calc(fullpath,ma,md)
    implicit none
    real                          :: ma,md
    character(len=100)            :: fullpath 
    end subroutine medium_calc



    subroutine medium_init
    implicit none
    end subroutine medium_init



    subroutine medium_main
    implicit none
    end subroutine medium_main



    function medium_Lfunc(t)
    implicit none
    real :: t,medium_Lfunc
    end function medium_Lfunc



    function medium_func(x,t)
    implicit none
    real                     :: t
    real, dimension(:)       :: x
    real, dimension(size(x)) :: medium_func
    end function medium_func



  end interface
  end module medium
