  subroutine md_main
  use io_vars, only: io_path
  use md_vars, only: md_ism_dens
  use md_interface, only: md_calc,md_init
  implicit none
  real                          :: ma,md
  integer                       :: stat1,stat2
  character(len=100)            :: machar,mdchar,path,fullpath
  character(len=6)  , parameter :: f = "(f6.4)"




  call md_init
! Writting some headers

  open(unit=66,file=trim(adjustl(IO_path))//"/stability_4.dat",status="unknown")
    write(66,*) "# This is a datafile"
    write(66,*) "# 1  macc(msun)"
    write(66,*) "# 2  mdon(msun)"
    write(66,*) "# 3  rtau(cm)"
    write(66,*) "# 4  dens(g/cc)"
    write(66,*) "# 5  mdot_eje(msun/yr)"
    write(66,*) "# 6  v_esc(cm/s)"
    write(66,*) "# 7  tau_total"
    write(66,*) "# 8  tau_rtau"
    write(66,*) "# 9  t_sim(yr)"
    write(66,*) "# 10 t_edd_ini(yr)"
    write(66,*) "# 11 t_edd_end(yr)"
    write(66,*) "# 12 L_edd(erg/s)"
    write(66,*) "# 13 r_bubble(cm)"
    write(66,*) "# 14 r_shock(cm)"
    write(66,*) "# 15 evotype"
    write(66,*) "# 16 mdot_eje_mean(msun/yr)"
    write(66,*) "# 17 mdot_final(msun/yr)"
  close(66)

  open(unit=66,file=trim(adjustl(IO_path))//"/plot_dens.gpi",status="unknown")
    write(66,*) "# This is the density profile plotting script, autogenerated"
    write(66,*) "set term postscript enhanced color"
    write(66,*) "set output 'dens.ps'"
    write(66,*) "set palette rgbformulae 33,13,10"
    write(66,*) "set format cb '%g'"
    write(66,*) "set log x"
    write(66,*) "set log y"
    write(66,*) "set xlabel 'r (cm)'"
    write(66,*) "set ylabel '{/Symbol r} (g/cc)'"
    write(66,*) "set format x '10^{%L}'"
    write(66,*) "set format y '10^{%L}'"
    write(66,*) " " 
  close(66)

  open(unit=66,file=trim(adjustl(IO_path))//"/plot_teff.gpi",status="unknown")
    write(66,*) "# This is the temperature evolution plotting script, autogenerated"
    write(66,*) "set term postscript enhanced color"
    write(66,*) "set output 'temp.ps'"
    write(66,*) "set palette rgbformulae 33,13,10"
    write(66,*) "set format cb '%g'"
    write(66,*) "set log x"
    write(66,*) "set log y"
    write(66,*) "set xlabel 't (yr)'"
    write(66,*) "set ylabel 'T (K)'"
    write(66,*) "set format x '10^{%L}'"
    write(66,*) "set format y '10^{%L}'"
    write(66,*) " " 
  close(66)




! Bulk of routine

  open(unit=10,file=trim(adjustl(IO_path))//"/list",status="old")
  stat1 = 0
  stat2 = 0
  acc: do while (stat1.eq.0) 
    read(10,*,iostat=stat1) ma
    if (stat1.ne.0) exit
    write(machar,f) ma
    path = trim(adjustl(IO_path))//"/"//trim(adjustl(machar))
    call system("ls "//trim(adjustl(path))//" > "//trim(adjustl(path))//"/list")
    open(unit=20,file=trim(adjustl(path))//"/list",status="old")
    don: do while(stat2.eq.0)
      read(20,*,iostat=stat2) md
      if (stat2.ne.0) exit
      write(mdchar,f) md
      fullpath = trim(adjustl(path))//"/"//trim(adjustl(mdchar))
      call medium_calc(fullpath,ma,md)
    end do don
    close(20) 
    open(unit=20,file=trim(adjustl(IO_path))//"/stability_4.dat",status="old",position="append")
      write(20,*) " "
    close(20)
    stat2 = 0
  end do acc
  close(10)





! Finishing the job

  open(unit=66,file=trim(adjustl(IO_path))//"/plot_dens.gpi",status="old",position="append")
    write(66,*) md_ism_dens,"lt -1 lc -1 notitle"
  close(66) 

  open(unit=66,file=trim(adjustl(IO_path))//"/plot_teff.gpi",status="old",position="append")
    write(66,*) "10**4 lt -1 lc -1 notitle"
  close(66) 





  end subroutine md_main
