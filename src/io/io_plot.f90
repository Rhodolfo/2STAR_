  subroutine io_plot
! io_plot hogs io_unit + 1, rmemeber that!
  use dr_vars, only: dr_integration_mode,dr_mode_ballistic,&
                    dr_mode_mdot,dr_mode_post,dr_mode_stability
  use io_vars, only: io_save
  use io_interface, only: io_log
  implicit none
  select case (dr_integration_mode)
  case (dr_mode_ballistic) 
    call io_log("[io_plot] Writing gnuplot files for ballistic")
    call io_plot_ballistic
  case (dr_mode_mdot)
    call io_log("[io_plot] Writing gnuplot files for mdot")
    call io_plot_mdot
  case (dr_mode_post)
    call io_log("[io_plot] Writing gnuplot files for mdot postprocessing")
    call io_plot_post
  case (dr_mode_stability)
    call io_log("[io_plot] Writing gnuplot files for stability")
    call io_plot_stability
  end select
  contains



    subroutine io_plot_ballistic
    use ph_vars, only: ph_pi
    use io_vars, only: io_unit,io_path
    use cp_vars, only: cp_don_pos,cp_acc_pos,cp_don_radius,cp_acc_radius,&
                       cp_roche_radius,cp_hit_radius
    implicit none
    integer            :: step
    real               :: angle
    real,dimension(2)  :: unit_vector,donor_circle,accretor_circle,roche_circle
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_ballistic.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_ballistic.gpi")
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/circle.dat",status="unknown")
    do step = 0,100
      angle = (2.*ph_pi/100.)*step
      unit_vector = (/ cos(angle), sin(angle) /)
      donor_circle    = cp_don_pos    + unit_vector*cp_don_radius
      roche_circle    = cp_don_pos    + unit_vector*cp_roche_radius
      accretor_circle = cp_acc_pos + unit_vector*cp_hit_radius
      write(io_unit+1,*) angle,accretor_circle,donor_circle,roche_circle
    end do
    close(io_unit+1)
    return
    end subroutine io_plot_ballistic



    subroutine io_plot_mdot
    use ph_vars, only: ph_msun,ph_year
    use dr_vars, only: dr_time,dr_res_factor,dr_eddington_switch,&
                       dr_time_eddington,dr_time_eddington_elapsed,dr_time_eddington_exit,&
                       dr_accretion_flow,dr_is_super_eddington
    use cp_vars, only: cp_mdot_edd,cp_mdot_max,cp_don_mdot,cp_mdot_eq,&
                       cp_maxtemp,cp_mintemp,cp_don_mdot
    use io_vars, only: io_unit,io_path
    implicit none
    real :: a,b
    a = min(abs(cp_don_mdot),abs(cp_mdot_edd))
    if (cp_mdot_eq.lt.0.) a = min(abs(cp_mdot_eq),abs(a))
    a = a / 10.
    b = 10.*abs(cp_mdot_max)
  ! Sub eddington stuff
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'suffix     = ""'
    write(io_unit+1,*) 'res_factor = ',dr_res_factor
    write(io_unit+1,*) 'year       = ',ph_year
    write(io_unit+1,*) 'a          = ',0.
    write(io_unit+1,*) 'b          = ',dr_time/ph_year
    write(io_unit+1,*) 'set xrange [a:b]' 
    write(io_unit+1,*) 'minmdot    = ',a*ph_year/ph_msun
    write(io_unit+1,*) 'maxmdot    = ',b*ph_year/ph_msun
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_mdot.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_mdot.gpi")
  ! Zooming into the super eddington
    if (.not.dr_eddington_switch) return
    if (dr_accretion_flow.eq.dr_is_super_eddington) then 
      dr_time_eddington_exit    = dr_time
      dr_time_eddington_elapsed = dr_time_eddington_exit-dr_time_eddington
    else
      dr_time_eddington_exit = dr_time_eddington_exit + 0.1*dr_time_eddington_elapsed
    end if
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'data_path  = "'//trim(adjustl(io_path)),'"'
    write(io_unit+1,*) 'suffix     = "_zoom"'
    write(io_unit+1,*) 'res_factor = ',dr_res_factor
    write(io_unit+1,*) 'year       = ',ph_year
    write(io_unit+1,*) 'a          = ',max(0.,dr_time_eddington-0.1*dr_time_eddington_elapsed)/ph_year
    write(io_unit+1,*) 'b          = ',min(dr_time,dr_time_eddington_exit)/ph_year
    write(io_unit+1,*) 'set xrange [a:b]'
    write(io_unit+1,*) 'minmdot    = ',a*ph_year/ph_msun
    write(io_unit+1,*) 'maxmdot    = ',b*ph_year/ph_msun
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_mdot.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_mdot_zoom.gpi")
    end subroutine io_plot_mdot
  


    subroutine io_plot_post 
    use ph_vars, only: ph_msun,ph_year
    use dr_vars, only: dr_time,dr_res_factor,dr_eddington_switch,&
                       dr_time_eddington,dr_time_eddington_elapsed,dr_time_eddington_exit,&
                       dr_accretion_flow,dr_is_super_eddington
    use cp_vars, only: cp_mdot_edd,cp_mdot_max,cp_don_mdot,cp_mdot_eq,&
                       cp_maxtemp,cp_mintemp
    use io_vars, only: io_unit,io_path
    implicit none
  ! Sub eddington stuff
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'data_path  = "'//trim(adjustl(io_path)),'"'
    write(io_unit+1,*) 'suffix     = ""'
    write(io_unit+1,*) 'res_factor = ',dr_res_factor
    write(io_unit+1,*) 'year       = ',ph_year
    write(io_unit+1,*) 'a          = ',0.
    write(io_unit+1,*) 'b          = ',dr_time/ph_year
    write(io_unit+1,*) 'set xrange [a:b]' 
    write(io_unit+1,*) 'mintemp    = ',cp_mintemp
    write(io_unit+1,*) 'maxtemp    = ',cp_maxtemp
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_post.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_post.gpi")
  ! Zooming into the super eddington
    if (.not.dr_eddington_switch) return
    if (dr_accretion_flow.eq.dr_is_super_eddington) then 
      dr_time_eddington_exit    = dr_time
      dr_time_eddington_elapsed = dr_time_eddington_exit-dr_time_eddington
    else
      dr_time_eddington_exit = dr_time_eddington_exit + 0.1*dr_time_eddington_elapsed
    end if
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'data_path  = "'//trim(adjustl(io_path)),'"'
    write(io_unit+1,*) 'suffix     = "_zoom"'
    write(io_unit+1,*) 'res_factor = ',dr_res_factor
    write(io_unit+1,*) 'year       = ',ph_year
    write(io_unit+1,*) 'a          = ',max(0.,dr_time_eddington-0.1*dr_time_eddington_elapsed)/ph_year
    write(io_unit+1,*) 'b          = ',min(dr_time,dr_time_eddington_exit)/ph_year
    write(io_unit+1,*) 'set xrange [a:b]'
    write(io_unit+1,*) 'mintemp    = ',cp_mintemp
    write(io_unit+1,*) 'maxtemp    = ',cp_maxtemp
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_post.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_post_zoom.gpi")
    end subroutine io_plot_post



    subroutine io_plot_stability
    use dr_vars, only: dr_low_mass,dr_hig_mass,dr_phase
    use io_vars, only: io_unit,io_path
    implicit none
  ! Basic runs
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'data_path  = "'//trim(adjustl(io_path)),'"'
    write(io_unit+1,*) 'minmass    = ',dr_low_mass
    write(io_unit+1,*) 'maxmass    = ',dr_hig_mass
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_stability.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_stability.gpi")
  ! Batch of simulations
    if (dr_phase.lt.3) return
    open(unit=io_unit+1,file=trim(adjustl(io_path))//"/plot_decs.gpi",status="unknown")
    write(io_unit+1,*) 'data_path  = "'//trim(adjustl(io_path)),'"'
    write(io_unit+1,*) 'minmass    = ',dr_low_mass
    write(io_unit+1,*) 'maxmass    = ',dr_hig_mass
    close(io_unit+1)
    call system("cat "//trim(adjustl(io_path))//"/plot_decs.gpi src/io/templates/plot_stability_sims.gpi &
                 & > "//trim(adjustl(io_path))//"/plot_stability_sims.gpi")
    end subroutine io_plot_stability

  end subroutine io_plot
