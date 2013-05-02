  program main
  use driver, only: dr_restore_defaults,dr_perform
  use     IO, only: IO_read_command_line_options,IO_log,IO_path
  implicit none

! mdot_time takes several arguments when called, see the routine
! subroutine read_command_line_options for a list

! When called in mdot mode (default), this program solves the evolution 
! for the orbital separation, given a donor and an accretor type and mass. 
! Additional options are included to set the data path (where data is written)
! and whether or not evolve the ballistic trayectories as well.

! When called in ballistic mode, this program solves for the trayectories
! of point particles under the full Roche + Coriolis field produced by the binary.

! Data is written by default on the directory "default"

  call dr_restore_defaults
  call IO_read_command_line_options
  call IO_log("[main] Hey there, man. I am the 2STAR_ code.")
  call IO_log("[main] This code can perform several computations related to mass transfer in compact binaries.")
  call IO_log("[main] For a complete set of CLI options, look at src/IO/IO_main.f90")
  call IO_log("[main] Data directory is set to "//trim(adjustl(IO_path))//" and has been created") 
  call IO_log("[main] The bulk of the 2STAR_ code begins operating here")
  call dr_perform

  end program main
