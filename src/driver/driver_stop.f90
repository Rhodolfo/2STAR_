  subroutine dr_stop
  use driver, only: dr_integration_mode,dr_mode_ballistic
  use     IO, only: IO_plot,IO_path,IO_log
  implicit none
  call IO_plot
  call IO_log("[driver] Done, check "//trim(adjustl(IO_path))//" for data and plots")
  stop
  end subroutine dr_stop
