  subroutine IO_plot
! IO_plot hogs IO_unit + 1, rmemeber that!
  use driver, only: dr_integration_mode,dr_mode_ballistic,&
                    dr_mode_mdot,dr_mode_post,dr_mode_stability,dr_hybrid
  use     IO, only: IO_save,IO_plot_ballistic,IO_plot_mdot,IO_plot_stability,IO_log
  implicit none
  select case (dr_integration_mode)
  case (dr_mode_ballistic) 
    call IO_log("[io_plot] Writing gnuplot files for ballistic")
    call IO_plot_ballistic
  case (dr_mode_mdot)
    call IO_log("[io_plot] Writing gnuplot files for mdot")
    call IO_plot_mdot
    if (dr_hybrid) call IO_plot_post
  case (dr_mode_post)
    call IO_log("[io_plot] Writing gnuplot files for mdot postprocessing")
    call IO_plot_post
  case (dr_mode_stability)
    call IO_log("[io_plot] Writing gnuplot files for stability")
    call IO_plot_stability
  end select
  end subroutine IO_plot



  subroutine IO_plot_ballistic
  use   physics, only: pi
  use        IO, only: IO_unit,IO_path,IO_plot_now
  use component, only: cp_donor_position,cp_accretor_position,cp_donor_radius,cp_accretor_radius,&
                       cp_roche_radius,cp_hit_radius
  implicit none
  integer            :: step
  real               :: angle
  real,dimension(2)  :: unit_vector,donor_circle,accretor_circle,roche_circle
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_ballistic.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_ballistic.gpi")
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/circle.dat",status="unknown")
  do step = 0,100
    angle = (2.*pi/100.)*step
    unit_vector = (/ cos(angle), sin(angle) /)
    donor_circle    = cp_donor_position    + unit_vector*cp_donor_radius
    roche_circle    = cp_donor_position    + unit_vector*cp_roche_radius
    accretor_circle = cp_accretor_position + unit_vector*cp_hit_radius
    write(IO_unit+1,*) angle,accretor_circle,donor_circle,roche_circle
  end do
  close(IO_unit+1)
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_ballistic.gpi")
  return
  end subroutine IO_plot_ballistic



  subroutine IO_plot_mdot
  use   physics, only: solar_mass,year
  use    driver, only: dr_time,dr_res_factor,dr_eddington_switch,&
                       dr_time_eddington,dr_time_eddington_elapsed,dr_time_eddington_exit,&
                       dr_accretion_flow,dr_is_super_eddington
  use component, only: cp_mdot_eddington,cp_mdot_donor_max,cp_mdot_donor,cp_mdot_eq,&
                       cp_maxtemp,cp_mintemp,cp_mdot_donor
  use        IO, only: IO_unit,IO_path,IO_plot_now
  implicit none
  real :: a,b
  a = min(abs(cp_mdot_donor),abs(cp_mdot_eddington))
  if (cp_mdot_eq.lt.0.) a = min(abs(cp_mdot_eq),abs(a))
  a = a / 10.
  b = 10.*abs(cp_mdot_donor_max)
! Sub eddington stuff
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'suffix     = ""'
  write(IO_unit+1,*) 'res_factor = ',dr_res_factor
  write(IO_unit+1,*) 'year       = ',year
  write(IO_unit+1,*) 'a          = ',0.
  write(IO_unit+1,*) 'b          = ',dr_time/year
  write(IO_unit+1,*) 'set xrange [a:b]' 
  write(IO_unit+1,*) 'minmdot    = ',a*year/solar_mass
  write(IO_unit+1,*) 'maxmdot    = ',b*year/solar_mass
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_mdot.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_mdot.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_mdot.gpi")
! Zooming into the super eddington
  if (.not.dr_eddington_switch) return
  if (dr_accretion_flow.eq.dr_is_super_eddington) then 
    dr_time_eddington_exit    = dr_time
    dr_time_eddington_elapsed = dr_time_eddington_exit-dr_time_eddington
  else
    dr_time_eddington_exit = dr_time_eddington_exit + 0.1*dr_time_eddington_elapsed
  end if
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'suffix     = "_zoom"'
  write(IO_unit+1,*) 'res_factor = ',dr_res_factor
  write(IO_unit+1,*) 'year       = ',year
  write(IO_unit+1,*) 'a          = ',max(0.,dr_time_eddington-0.1*dr_time_eddington_elapsed)/year
  write(IO_unit+1,*) 'b          = ',min(dr_time,dr_time_eddington_exit)/year
  write(IO_unit+1,*) 'set xrange [a:b]'
  write(IO_unit+1,*) 'minmdot    = ',a*year/solar_mass
  write(IO_unit+1,*) 'maxmdot    = ',b*year/solar_mass
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_mdot.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_mdot_zoom.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_mdot_zoom.gpi")
  end subroutine IO_plot_mdot



  subroutine IO_plot_post 
  use   physics, only: solar_mass,year
  use    driver, only: dr_time,dr_res_factor,dr_eddington_switch,&
                       dr_time_eddington,dr_time_eddington_elapsed,dr_time_eddington_exit,&
                       dr_accretion_flow,dr_is_super_eddington
  use component, only: cp_mdot_eddington,cp_mdot_donor_max,cp_mdot_donor,cp_mdot_eq,&
                       cp_maxtemp,cp_mintemp
  use        IO, only: IO_unit,IO_path,IO_plot_now
  implicit none
! Sub eddington stuff
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'suffix     = ""'
  write(IO_unit+1,*) 'res_factor = ',dr_res_factor
  write(IO_unit+1,*) 'year       = ',year
  write(IO_unit+1,*) 'a          = ',0.
  write(IO_unit+1,*) 'b          = ',dr_time/year
  write(IO_unit+1,*) 'set xrange [a:b]' 
  write(IO_unit+1,*) 'mintemp    = ',cp_mintemp
  write(IO_unit+1,*) 'maxtemp    = ',cp_maxtemp
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_post.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_post.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_post.gpi")
! Zooming into the super eddington
  if (.not.dr_eddington_switch) return
  if (dr_accretion_flow.eq.dr_is_super_eddington) then 
    dr_time_eddington_exit    = dr_time
    dr_time_eddington_elapsed = dr_time_eddington_exit-dr_time_eddington
  else
    dr_time_eddington_exit = dr_time_eddington_exit + 0.1*dr_time_eddington_elapsed
  end if
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'suffix     = "_zoom"'
  write(IO_unit+1,*) 'res_factor = ',dr_res_factor
  write(IO_unit+1,*) 'year       = ',year
  write(IO_unit+1,*) 'a          = ',max(0.,dr_time_eddington-0.1*dr_time_eddington_elapsed)/year
  write(IO_unit+1,*) 'b          = ',min(dr_time,dr_time_eddington_exit)/year
  write(IO_unit+1,*) 'set xrange [a:b]'
  write(IO_unit+1,*) 'mintemp    = ',cp_mintemp
  write(IO_unit+1,*) 'maxtemp    = ',cp_maxtemp
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_post.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_post_zoom.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_post_zoom.gpi")
  end subroutine IO_plot_post



  subroutine IO_plot_stability
  use  driver, only: dr_low_mass,dr_hig_mass,dr_phase
  use      IO, only: IO_unit,IO_path,IO_plot_now
  implicit none
! Basic runs
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'minmass    = ',dr_low_mass
  write(IO_unit+1,*) 'maxmass    = ',dr_hig_mass
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_stability.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_stability.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_stability.gpi")
! Batch of simulations
  if (dr_phase.lt.3) return
  open(unit=IO_unit+1,file=trim(adjustl(IO_path))//"/plot_decs.gpi",status="unknown")
  write(IO_unit+1,*) 'data_path  = "'//trim(adjustl(IO_path)),'"'
  write(IO_unit+1,*) 'minmass    = ',dr_low_mass
  write(IO_unit+1,*) 'maxmass    = ',dr_hig_mass
  close(IO_unit+1)
  call system("cat "//trim(adjustl(IO_path))//"/plot_decs.gpi src/IO/templates/plot_stability_sims.gpi &
               & > "//trim(adjustl(IO_path))//"/plot_stability_sims.gpi")
  if (IO_plot_now) call system("gnuplot "//trim(adjustl(IO_path))//"/plot_stability_sims.gpi")
  end subroutine IO_plot_stability
