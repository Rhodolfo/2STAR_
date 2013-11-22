  module md_interface
  implicit none
  interface 
    subroutine md_calc(fullpath,ma,md)
    implicit none
    real                          :: ma,md
    character(len=100)            :: fullpath 
    end subroutine md_calc
    subroutine md_init
    implicit none
    end subroutine md_init
    subroutine md_main
    implicit none
    end subroutine md_main
  end interface
  end module md_interface
