  subroutine dr_abort(routine,message)
  use dr_vars, only: dr_mdot_ref
  use ph_vars, only: ph_msun,ph_year
  use cp_vars, only: cp_don_mass,cp_acc_mass,cp_bin_peri,cp_don_freq,cp_acc_freq,cp_bin_sepa,&
                     cp_don_mdot
  use IO_vars, only: IO_verb
  use IO_interface, only: IO_log,IO_2string
  implicit none
  character*(*)          :: routine,message
  IO_verb = .true.
  call IO_log("["//trim(adjustl(routine))//"] has called dr_abort")
  call IO_log("["//trim(adjustl(routine))//"] "//trim(adjustl(message)))
  call IO_log("Dumping simulation parameters")
  call IO_log("mdonor = "//IO_2string(cp_don_mass/ph_msun)//" msun")
  call IO_log("maccrt = "//IO_2string(cp_acc_mass/ph_msun)//" msun")
  call IO_log("period = "//IO_2string(cp_bin_peri)//" s")
  call IO_log("separa = "//IO_2string(cp_bin_sepa)//" cm")
  call IO_log("fdonor = "//IO_2string(cp_don_freq)//" rad/s")
  call IO_log("faccrt = "//IO_2string(cp_acc_freq)//" rad/s")
  call IO_log("mdot_2 = "//IO_2string(cp_don_mdot*ph_year/ph_msun)//" msun/yr") 
  call IO_log("mdot_o = "//IO_2string(dr_mdot_ref*ph_year/ph_msun)//" msun/yr") 
  stop
  end subroutine dr_abort
