  module IO_vars
  implicit none
  integer                         :: io_unit = 46
  integer, parameter              :: io_clen = 100
  character(len=io_clen)          :: io_path = "data/default" 
  character(len=io_clen)          :: io_file = "data.dat"
  character(len=io_clen)          :: io_logf = "logfile"
  character(len=10)               :: io_rfrm = "(ES9.2)"
  integer                         :: io_rlen = 9
  character(len=5)                :: io_ifrm = "(I10)"
  integer                         :: io_ilen = 10
  logical                         :: io_save = .true.
  logical                         :: io_verb = .true.
  logical                         :: io_first_pass = .true.
  real, dimension(:), allocatable :: io_data
  end module IO_vars
