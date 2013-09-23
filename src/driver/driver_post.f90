  subroutine dr_perform_mdot_postprocessing
  use ph_vars, only: ph_msun,ph_year
  use dr_vars, only: dr_time,dr_time_step,dr_setup_mode,&
                     dr_mode_post,dr_mode_ballistic,dr_mode_period,&
                     dr_interrupting,dr_mode_post,dr_file_counter,&
                     dr_stop_on_hit,dr_integration_mode,dr_res_factor
  use cp_vars, only: cp_bin_peri,cp_bin_sepa,&
                     cp_don_mass,cp_acc_mass,cp_setup_var,&
                     cp_impact_dens,cp_initial_dens,cp_virtemp,&
                     cp_mintemp,cp_maxtemp
  use io_vars, only: io_save,io_unit,io_verb,io_file,io_path
  use dr_interface, only: dr_perform_ballistic_evolution,dr_store_full_mdot_data
  use io_interface, only: io_log,io_cli_options,io_write_header,io_2string
  implicit none
  integer            :: i
  integer            :: trigger
  integer            :: line_comment
  real               :: sim_time
  real               :: sim_time_step
  real               :: sim_don_mass
  real               :: sim_acc_mass
  real               :: sim_sepa
  real               :: sim_peri
  real,dimension(26) :: dummy_array
  character          :: dummy
  logical            :: header
  call io_log("Postprocessing data")
! We are reading mdot.dat in to get the data
  header = .true.
  open(unit=io_unit+1,file=trim(adjustl(io_path))//"/mdot.dat",status="old")
  trigger      = 0
  line_comment = 0
  do while (trigger.ge.0)
    read(io_unit+1,*) dummy
    if (.not.(dummy.eq."#")) trigger = -1
    line_comment = line_comment + 1
  end do
  close(io_unit+1)
  trigger      = 0
  open(unit=io_unit+1,file=trim(adjustl(io_path))//"/mdot.dat",status="old")
  dr_file_counter = 0
  do i = 1,line_comment
    read(io_unit+1,*) dummy
  end do
! Now to the evolution
  do while (trigger.ge.0)
    read(io_unit+1,*,iostat=trigger) dummy_array(1:26)
    sim_time             = dummy_array(1)
    sim_time_step        = dummy_array(2)
    sim_sepa             = dummy_array(3)
    sim_peri             = dummy_array(4)
    sim_don_mass         = dummy_array(6)
    sim_acc_mass         = dummy_array(10)
    cp_bin_peri          = sim_peri
    cp_bin_sepa          = sim_sepa
    cp_don_mass          = sim_don_mass*ph_msun
    cp_acc_mass          = sim_acc_mass*ph_msun
    dr_integration_mode  = dr_mode_ballistic
    dr_setup_mode        = dr_mode_period
    cp_setup_var         = cp_bin_peri
    io_verb              = .false. 
    io_save              = .false.
    dr_interrupting      = .false.
    dr_stop_on_hit       = .true.
    dr_res_factor        = 0.001
    call dr_perform_ballistic_evolution
    sim_time             = dummy_array(1)
    sim_time_step        = dummy_array(2)
    dr_time              = sim_time*ph_year
    dr_time_step         = sim_time_step*ph_year
    io_save              = .true.
    dr_integration_mode  = dr_mode_post
    if (header) then 
      io_file = "mdot_post.dat"
      call io_write_header
      cp_mintemp = cp_virtemp
      cp_maxtemp = cp_virtemp
      header = .false.
    end if
    io_verb = .true.
    call io_log(trim(adjustl(io_2string(sim_time)))//" "//trim(adjustl(io_2string(cp_virtemp))))
    io_verb = .false.
    dr_file_counter = dr_file_counter + 1
    cp_mintemp = min(cp_virtemp,cp_mintemp)
    cp_maxtemp = max(cp_virtemp,cp_maxtemp)
    call dr_store_full_mdot_data
  end do
  close(io_unit+1)
  dr_integration_mode = dr_mode_post
  call io_log("Done postprocessing data")
  call io_cli_options
  end subroutine dr_perform_mdot_postprocessing
