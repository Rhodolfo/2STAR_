  module eos_interface

  interface

      subroutine eos_helmholtz
      implicit none
      end subroutine eos_helmholtz

      subroutine eos_read_helm_table
      implicit none
      end subroutine eos_read_helm_table

      subroutine eos_helmeos
      implicit none 
      end subroutine eos_helmeos

      subroutine eos_pretty_eos_out(whose)
      implicit none
      character*(*) whose
      end subroutine eos_pretty_eos_out

      function eos_pres_entr(input)
      implicit none
      real, dimension(:) :: input
      real, dimension(size(input)) :: eos_pres_entr
      end function eos_pres_entr

      function eos_pres_entr_zero(input)
      implicit none
      real, dimension(:) :: input
      real, dimension(size(input)) :: eos_pres_entr_zero
      end function eos_pres_entr_zero

      subroutine eos_newton_raphson(iguess,fguess,dx,f,ntol,rtol,relax)
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
      end subroutine eos_newton_raphson

      subroutine eos_newton_raphson_step(gc,gn,dg,f,ce)
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
      end subroutine eos_newton_raphson_step 

  end interface

  end module eos_interface
