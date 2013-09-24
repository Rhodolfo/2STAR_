  module io_interface
  interface

  ! Command line handling
    subroutine io_cli_options
    implicit none
    end subroutine io_cli_options



  ! Data handling, always allocate and deallocate data
    subroutine io_allocate_data(ii)
    implicit none
    integer, intent(in) :: ii
    end subroutine io_allocate_data
    subroutine io_deallocate_data
    implicit none
    end subroutine io_deallocate_data
    subroutine io_save_data(path,file)
    implicit none
    character*(*), intent(in), optional :: path,file
    end subroutine io_save_data



  ! File handling, file opening and such things
    subroutine io_open(path,file)
    implicit none
    character*(*), intent(in), optional :: path,file
    end subroutine io_open
    subroutine io_newline(p,f)
    implicit none
    character*(*) :: p,f
    end subroutine io_newline
    subroutine io_write_header(unit,path,file)
    implicit none
    integer      , intent(in), optional :: unit
    character*(*), intent(in), optional :: path,file
    end subroutine io_write_header
    subroutine io_quick_write(path,file,message)
    implicit none
    character*(*), intent(in), optional :: path,file
    character*(*), intent(in)           :: message
    end subroutine io_quick_write



  ! Logging 
    subroutine io_log(message)
    implicit none 
    character*(*) :: message
    end subroutine io_log



  ! This should go somewhere else, but well
    subroutine io_write_header_sweep(p,f)
    implicit none
    character*(*) :: p,f
    end subroutine io_write_header_sweep



! Plotting
    subroutine io_plot
    implicit none
    end subroutine io_plot






  end interface





! String transformation

  interface io_2string 
    module procedure io_int2string,io_real2string,io_real2string_array
  end interface io_2string





  contains

    function io_int2string(ii)
    use io_vars, only: io_ilen,io_ifrm
    implicit none
    integer                :: ii 
    character(len=io_ilen) :: io_int2string
    write(io_int2string,io_Ifrm) ii 
    end function io_int2string

    function io_real2string(ii)
    use io_vars, only: io_rlen,io_rfrm
    implicit none
    real                   :: ii
    character(len=io_rlen) :: io_real2string
    write(io_real2string,io_rfrm) ii
    end function io_real2string

    function io_real2string_array(ii)
    use io_vars, only: io_rlen,io_rfrm
    implicit none
    real, dimension(:)                          :: ii
    character(len=io_rlen), dimension(size(ii)) :: io_real2string_array
    integer :: jj
    do jj = 1,size(ii)
    write(io_real2string_array(jj),io_rfrm) ii(jj)
    end do
    end function io_real2string_array
 





  end module io_interface
