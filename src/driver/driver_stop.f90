  subroutine dr_stop
  use io_vars, only: io_path
  use io_interface, only: io_plot,io_log
  implicit none
  call io_plot
  call io_log("[driver] Done, check "//trim(adjustl(io_path))//" for data and plots")
  stop
  end subroutine dr_stop
