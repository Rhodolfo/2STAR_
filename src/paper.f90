  program paper
  use driver, only: dr_restore_defaults,dr_perform
  use     IO, only: IO_read_command_line_options,IO_log,IO_path
  use medium, only: medium_main
  implicit none
  call dr_restore_defaults
  call IO_read_command_line_options
  call IO_log("[main] Hey there, man. I will make your figures.")
  call IO_log("[main] Data directory is set to "//trim(adjustl(IO_path))//" and has been created") 
  call medium_main
  end program paper
