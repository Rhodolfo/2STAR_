  program main
  use  io_vars     , only: io_path
  use eos_vars     , only: eos_dens,eos_temp,eos_pres,eos_entr,&
                           eos_target_pres,eos_target_entr
  use  io_interface, only: io_cli_options,io_log
  use  dr_interface, only: dr_restore_defaults,dr_perform
  use eos_interface, only: eos_helmholtz,eos_pres_entr,eos_pres_entr_zero,&
                           eos_newton_raphson
  use cp_vars
  use dr_vars
  implicit none
  real :: dens_temp(2),pres_entr(2),dens,temp,dumray(2),diff(2)





! EOS testing, no need to heed this

! dens = 1.0
! temp = 1.0
! dens_temp = (/ dens, temp /)
! pres_entr = eos_pres_entr(dens_temp)
! write(*,*) "1",pres_entr(1),dens_temp(1),dens_temp(2),pres_entr(2) 
! eos_target_pres = pres_entr(1)
! eos_target_entr = pres_entr(2)
! write(*,*) "B",dens_temp
! dens_temp = 2.0*dens_temp
! diff      = 0.001*dens_temp
! dumray    = dens_temp 
! call eos_newton_raphson(dens_temp,dumray,&
!                         diff,eos_pres_entr_zero,&
!                         10000,0.001,1e-1)
! write(*,*) "A",dumray
! stop "MANUAL ABORT"





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
  call io_cli_options
  call io_log("[main] Hey there, man. I am the 2STAR_ code.")
  call io_log("[main] This code can perform several computations related to mass transfer in compact binaries.")
  call io_log("[main] For a complete set of CLI options, look at src/io/io_main.f90")
  call io_log("[main] Data directory is set to "//trim(adjustl(io_path))//" and has been created") 
  call io_log("[main] The bulk of the 2STAR_ code begins operating here")
  call dr_perform

  end program main
