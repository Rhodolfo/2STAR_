  module cp_interface
  implicit none
  interface



    subroutine cp_reset
    implicit none
    end subroutine cp_reset



  ! Basic Computation of parameters

    subroutine cp_init(mode,m1,m2,me,invar,f1,f2)
    implicit none
    integer, intent(in) :: mode
    real,    intent(in) :: m1,m2,me,invar,f1,f2
    end subroutine cp_init

    subroutine cp_auxiliary_parameters
    implicit none
    end subroutine cp_auxiliary_parameters

    subroutine cp_star_parameters
    implicit none
    end subroutine cp_star_parameters
     
    subroutine cp_stream_parameters
    implicit none
    end subroutine cp_stream_parameters

    subroutine cp_getvirtemp(E,T,N,tol)
    implicit none
    real    :: E,T,N
    integer :: tol 
    end subroutine cp_getvirtemp



  ! Parameters of the binary system as a whole

    subroutine cp_binary_parameters
    implicit none
    end subroutine cp_binary_parameters

    subroutine cp_binary_parameters_period
    implicit none
    end subroutine cp_binary_parameters_period

    subroutine cp_binary_parameters_separation
    implicit none
    end subroutine cp_binary_parameters_separation

    subroutine cp_binary_parameters_contact
    implicit none
    end subroutine cp_binary_parameters_contact

    subroutine cp_binary_parameters_roche_limit
    implicit none
    end subroutine cp_binary_parameters_roche_limit

    subroutine cp_binary_parameters_mdot(x,n)
    implicit none
    real, intent(in) :: x
    integer, intent(in), optional :: n
    end subroutine cp_binary_parameters_mdot



  ! Properties of the circumbinary envelope

    subroutine cp_envelope
    implicit none 
    end subroutine cp_envelope



  ! Lagrangian points
    
    subroutine cp_get_L1
    implicit none
    end subroutine cp_get_L1

    function cp_roche_1D(x)
    implicit none
    real :: x,cp_roche_1D
    end function cp_roche_1D



  !  Mass transfer rates

    subroutine cp_get_mdot
    implicit none
    end subroutine cp_get_mdot

    subroutine cp_get_mdot_donor
    implicit none
    end subroutine cp_get_mdot_donor

    subroutine cp_get_mdot_accretor
    implicit none
    end subroutine cp_get_mdot_accretor


 
   ! Super Eddington terms

   subroutine cp_super_Eddington
   implicit none
   end subroutine cp_super_Eddington

   subroutine cp_hyper_Eddington
   implicit none
   end subroutine cp_hyper_Eddington



  ! Parameters relevant to the source terms of mdot

    subroutine cp_evolution_coefficients
    implicit none
    end subroutine cp_evolution_coefficients

    subroutine cp_driver_terms
    implicit none
    end subroutine cp_driver_terms

    subroutine cp_zeta_terms
    implicit none
    end subroutine cp_zeta_terms

    subroutine cp_timescales
    implicit none
    end subroutine cp_timescales
  
    subroutine cp_get_pdots
    implicit none
    end subroutine cp_get_pdots



  end interface 

  end module cp_interface
