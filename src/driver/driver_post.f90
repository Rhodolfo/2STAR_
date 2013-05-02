subroutine dr_perform_mdot_postprocessing
  use    physics, only: solar_mass,year
  use     driver, only: dr_time,dr_time_step,dr_setup_mode,&
                        dr_mode_post,dr_mode_ballistic,dr_mode_period,&
                        stream_source_function,dr_interrupting,&
                        dr_mode_post,dr_file_counter,&
                        dr_stop_on_hit,dr_integration_mode,&
                        dr_store_full_mdot_data,dr_res_factor
  use  component, only: cp_binary_period,cp_binary_separation,&
                        cp_donor_mass,cp_accretor_mass,cp_setup_var,&
                        cp_impact_density,cp_initial_density,cp_virial_temperature,&
                        cp_mintemp,cp_maxtemp
  use         IO, only: IO_save,IO_unit,IO_verb,IO_file,IO_2string,&
                        IO_read_command_line_options,IO_write_header,IO_path,IO_log
  implicit none
  integer            :: i
  integer            :: trigger
  integer            :: line_comment
  real               :: sim_time
  real               :: sim_time_step
  real               :: sim_donor_mass
  real               :: sim_accretor_mass
  real               :: sim_separation
  real               :: sim_period
  real,dimension(26) :: dummy_array
  character          :: dummy
  logical            :: header
  call IO_log("Postprocessing data")
! We are reading mdot.dat in to get the data
  header = .true.
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/mdot.dat",status="old")
  trigger      = 0
  line_comment = 0
  do while (trigger.ge.0)
    read(IO_unit+1,*) dummy
    if (.not.(dummy.eq."#")) trigger = -1
    line_comment = line_comment + 1
  end do
  close(IO_unit+1)
  trigger      = 0
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/mdot.dat",status="old")
  dr_file_counter = 0
  do i = 1,line_comment
    read(IO_unit+1,*) dummy
  end do
! Now to the evolution
  do while (trigger.ge.0)
    read(IO_unit+1,*,iostat=trigger) dummy_array(1:26)
    sim_time             = dummy_array(1)
    sim_time_step        = dummy_array(2)
    sim_separation       = dummy_array(3)
    sim_period           = dummy_array(4)
    sim_donor_mass       = dummy_array(6)
    sim_accretor_mass    = dummy_array(10)
    cp_binary_period     = sim_period
    cp_binary_separation = sim_separation
    cp_donor_mass        = sim_donor_mass*solar_mass
    cp_accretor_mass     = sim_accretor_mass*solar_mass
    dr_integration_mode  = dr_mode_ballistic
    dr_setup_mode        = dr_mode_period
    cp_setup_var         = cp_binary_period
    IO_verb              = .false. 
    IO_save              = .false.
    dr_interrupting      = .false.
    dr_stop_on_hit       = .true.
    dr_res_factor        = 0.001
    call dr_perform_ballistic_evolution
    sim_time             = dummy_array(1)
    sim_time_step        = dummy_array(2)
    dr_time              = sim_time*year
    dr_time_step         = sim_time_step*year
    IO_save              = .true.
    dr_integration_mode  = dr_mode_post
    if (header) then 
      IO_file = "mdot_post.dat"
      call IO_write_header
      cp_mintemp = cp_virial_temperature
      cp_maxtemp = cp_virial_temperature
      header = .false.
    end if
    IO_verb = .true.
    call IO_log(trim(adjustl(IO_2string(sim_time)))//" "//trim(adjustl(IO_2string(cp_virial_temperature))))
    IO_verb = .false.
    dr_file_counter = dr_file_counter + 1
    cp_mintemp = min(cp_virial_temperature,cp_mintemp)
    cp_maxtemp = max(cp_virial_temperature,cp_maxtemp)
    call dr_store_full_mdot_data
  end do
  close(IO_unit+1)
  dr_integration_mode = dr_mode_post
  call IO_log("Done postprocessing data")
  call IO_read_command_line_options
end subroutine dr_perform_mdot_postprocessing
