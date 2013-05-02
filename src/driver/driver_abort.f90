  subroutine dr_abort(routine,message)
  use        IO, only: IO_log,IO_2string,IO_verb
  use   physics, only: solar_mass
  use component, only: cp_donor_mass,cp_accretor_mass,cp_binary_period,cp_donor_freq,cp_accretor_freq
  implicit none
  character*(*)          :: routine,message
  IO_verb = .true.
  call IO_log("["//trim(adjustl(routine))//"] has called dr_abort")
  call IO_log("["//trim(adjustl(routine))//"] "//trim(adjustl(message)))
  call IO_log("Dumping simulation parameters")
  call IO_log("mdonor = "//IO_2string(cp_donor_mass/solar_mass)//" msun")
  call IO_log("maccrt = "//IO_2string(cp_accretor_mass/solar_mass)//" msun")
  call IO_log("period = "//IO_2string(cp_binary_period)//" s")
  call IO_log("fdonor = "//IO_2string(cp_donor_freq)   //" rad/s")
  call IO_log("faccrt = "//IO_2string(cp_accretor_freq)//" rad/s")
  stop
  end subroutine dr_abort
