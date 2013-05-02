module IO

  integer                         :: io_unit = 46
  integer, parameter              :: io_clen = 100
  character(len=io_clen)          :: io_path = "data/default" 
  character(len=io_clen)          :: io_file = "data.dat"
  character(len=io_clen)          :: io_logf = "logfile"
  character(len=10)               :: io_rfrm = "(ES9.2)"
  integer                         :: io_rlen = 9
  character(len=4)                :: io_ifrm = "(I5)"
  integer                         :: io_ilen = 5
  logical                         :: io_save = .true.
  logical                         :: io_verb = .true.
  logical                         :: io_plot_now   = .true.
  logical                         :: io_first_pass = .true.
  real, dimension(:), allocatable :: io_data

  interface

  ! Command line handling
    subroutine IO_read_command_line_options
    implicit none
    end subroutine IO_read_command_line_options



  ! Data handling, always allocate and deallocate data
    subroutine IO_allocate_data(ii)
    implicit none
    integer, intent(in) :: ii
    end subroutine IO_allocate_data
    subroutine IO_deallocate_data
    implicit none
    end subroutine IO_deallocate_data
    subroutine IO_save_data(path,file)
    implicit none
    character*(*), intent(in), optional :: path,file
    end subroutine IO_save_data



  ! File handling, file opening and such things
    subroutine IO_open(path,file)
    implicit none
    character*(*), intent(in), optional :: path,file
    end subroutine IO_open
    subroutine IO_newline(p,f)
    implicit none
    character*(*) :: p,f
    end subroutine IO_newline
    subroutine IO_write_header(unit,path,file)
    implicit none
    integer      , intent(in), optional :: unit
    character*(*), intent(in), optional :: path,file
    end subroutine IO_write_header
    subroutine IO_quick_write(path,file,message)
    implicit none
    character*(*), intent(in), optional :: path,file
    character*(*), intent(in)           :: message
    end subroutine IO_quick_write



  ! Logging 
    subroutine IO_log(message)
    implicit none 
    character*(*) :: message
    end subroutine IO_log



  ! This should go somewhere else, but well
    subroutine IO_write_header_sweep(p,f)
    implicit none
    character*(*) :: p,f
    end subroutine IO_write_header_sweep



! Plotting
    subroutine IO_plot
    implicit none
    end subroutine IO_plot
    subroutine IO_plot_ballistic
    implicit none
    end subroutine IO_plot_ballistic
    subroutine IO_plot_mdot
    implicit none
    end subroutine IO_plot_mdot
    subroutine IO_plot_stability
    implicit none
    end subroutine IO_plot_stability






  end interface





! String transformation

  interface IO_2string 
    module procedure IO_int2string,IO_real2string,IO_real2string_array
  end interface IO_2string





  contains

    function IO_int2string(ii)
    implicit none
    integer                :: ii 
    character(len=io_ilen) :: IO_int2string
    write(IO_int2string,IO_Ifrm) ii 
    end function IO_int2string

    function IO_real2string(ii)
    implicit none
    real                   :: ii
    character(len=io_rlen) :: IO_real2string
    write(IO_real2string,IO_rfrm) ii
    end function IO_real2string

    function IO_real2string_array(ii)
    implicit none
    real, dimension(:)                          :: ii
    character(len=io_rlen), dimension(size(ii)) :: IO_real2string_array
    integer :: jj
    do jj = 1,size(ii)
    write(IO_real2string_array(jj),IO_rfrm) ii(jj)
    end do
    end function IO_real2string_array
 





end module IO
