  program paper
  use io_vars, only: io_path
  use dr_interface, only: dr_restore_defaults,dr_perform
  use io_interface, only: io_cli_options,io_log
  use md_interface, only: md_main
  implicit none
  call dr_restore_defaults
  call io_cli_options
  call io_log("[main] Hey there, man. I will make your figures.")
  call io_log("[main] Data directory is set to "//trim(adjustl(io_path))//" and has been created") 
  call md_main
  end program paper
