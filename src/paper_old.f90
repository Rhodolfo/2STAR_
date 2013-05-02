  program paper
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
  call IO_log("[main] Hey there, man. I will make your figures.")
  call IO_log("[main] Data directory is set to "//trim(adjustl(IO_path))//" and has been created") 
  call doit
  end program paper





  subroutine doit
  use physics
  use driver
  use IO
  implicit none
  integer                       :: stat1,stat2,stat3,comment,alloc,ii,jj,kk,iimin,iimax,ll,evotype
  logical                       :: switch,super,ex,opti
  real                          :: ma,md,t,kappa,tau,timeedd,n0,Ledd,rterm,l36,T6,teff,Lkin,Einj,timefin
  real                          :: t0,r0,r23,P12,Pres,m6,v3,rinit,Luse,mdmn,interp
  real, dimension(36)               :: tem
  real, dimension(:)  , allocatable :: mate,mdte,vesc,dens,mdot,time,posi,taup,mdotdon,mdotedd
  character(len=1)              :: dum
  character(len=100)            :: machar,mdchar,path,fullpath
  character(len=6)  , parameter :: f = "(f6.4)"
  open(unit=66,file=trim(adjustl(IO_path))//"/papers",status="unknown")
  open(unit=10,file=trim(adjustl(IO_path))//"/list",status="old")
  open(unit=88,file=trim(adjustl(IO_path))//"/plot_dens.gpi",status="unknown")
! First loop
  stat1 = 0
  stat2 = 0
  stat3 = 0
  do while (stat1.eq.0) 
    read(10,*,iostat=stat1) ma
    if (stat1.eq.0) then
      else
      exit
    end if
    write(machar,f) ma
    path = trim(adjustl(IO_path))//"/"//trim(adjustl(machar))
    call system("ls "//trim(adjustl(path))//" > "//trim(adjustl(path))//"/list")
    open(unit=20,file=trim(adjustl(path))//"/list",status="old")
! Second loop
    do while(stat2.eq.0)
      read(20,*,iostat=stat2) md
      if (stat2.eq.0) then
        else
        exit
      end if
      write(mdchar,f) md
      fullpath = trim(adjustl(path))//"/"//trim(adjustl(mdchar))
      open(unit=30,file=trim(adjustl(fullpath))//"/mdot.dat",status="old")
      dum = "#"
    ! Going through comments 
      comment = -1
      do while (dum.eq."#")
        read(30,*) dum
        comment = comment + 1
      end do
    ! Counting data points
      alloc   = -1
      do while (stat3.eq.0)
        read(30,*,iostat=stat3) t
        alloc = alloc + 1
      end do
      stat3 = 0
      write(*,*) "ma =",ma,"md =",md,comment,"comment lines",alloc,"data points"
      close(30) 
    ! Opening again with full knowledge
      open(unit=30,file=trim(adjustl(fullpath))//"/mdot.dat",status="old")
      do ii = 1,comment
        read(30,*) dum
      end do
      if (allocated(time)) deallocate(time,mdot,mdte,mate,posi,vesc,dens,taup,mdotdon,mdotedd)
      allocate(time(alloc),mdot(alloc),mdte(alloc),mate(alloc),posi(alloc),vesc(alloc),dens(alloc),taup(alloc))
      allocate(mdotdon(alloc),mdotedd(alloc))
    ! Now I have all data, can I get the wind thing going? 
      open(unit=67,file=trim(adjustl(fullpath))//"/dens.dat",status="unknown")
      open(unit=68,file=trim(adjustl(fullpath))//"/rad.dat",status="unknown")
      kappa   = 0.2
      switch  = .false.
      ex      = .true.
      ll      = 0
      timeedd = 0.
      timefin = 0.
      Einj    = 0.
      mdmn    = 0.
      iimin   = alloc
      do ii = 1,alloc
      ! Reading in data
        read(30,*) tem
        time(ii)    = tem(1)*year
        mdotdon(ii) = abs(tem(5)*solar_mass/year)
        mdotedd(ii) = tem(16)*solar_mass/year
        mdte(ii)    = tem(6)*solar_mass
        mate(ii)    = tem(10)*solar_mass
        if (ii.eq.1) then 
          vesc(ii)    = sqrt(2.*G*(mdte(ii)+mate(ii))/tem(3))
        else
          vesc(ii)    = vesc(1)
        end if
        mdotdon(ii) = abs(tem(5))*solar_mass/year
      ! Super eddington or not?
        if (mdotdon(ii).ge.mdotedd(ii)) then
          iimax    = ii
          mdot(ii) = abs(abs(tem(5)) - abs(tem(9)))*solar_mass/year
          if (.not.switch) then 
            iimin    = ii
            iimax    = ii 
            timeedd  = time(ii)
            switch   = .true.
            ex       = .false.
          else 
          end if
          timefin = time(ii)
        else
          mdot(ii) = 0.
        end if
      ! Energy integral
        if (ii.eq.1) then 
          Einj = 0.5*mdot(ii)*(vesc(ii)**2)*(time(ii+1)-time(ii))
        else
          Einj = Einj + 0.5*mdot(ii)*(vesc(ii)**2)*(time(ii)-time(ii-1))
        end if
      ! Putting in the density profile if we have gone super eddington
        tau    = 0.
      ! Sitch if
        if (switch) then 
          opti   = .false.
          jj     = iimin
          do while (jj.lt.iimax)
            posi(jj) = vesc(jj)*max((time(ii)-time(jj)),0.)
            if (posi(jj).gt.0.) then 
              dens(jj) = abs(mdot(jj)/(4.*pi*vesc(jj)*(posi(jj)**2)))
            else 
              dens(jj) = 0.
            end if
      ! Density profile is defined
      ! Now to do the optical depth integral
            if (jj.eq.iimin) then 
              tau = 0.
            else 
              tau = tau + kappa*dens(jj)*abs(posi(jj-1)-posi(jj))
            end if
            if (jj.eq.iimax-1) then
            end if
            taup(jj) = tau
          ! Detecting optical radius
            if (tau.ge.1.0) then 
              if (.not.opti) then 
              kk   = jj
              opti = .true.
              interp = posi(kk-1) + (posi(kk)-posi(kk-1))*(1.-taup(kk-1))/(taup(kk)-taup(kk-1))
              end if
            else
              if (.not.opti) then 
              kk   = iimin
              end if
            end if
          ! I have the optical radius
            if (ii.eq.alloc) then 
              if (jj.eq.iimin) write(67,*) "# r d mdot tau vesc"
              write(67,*) posi(jj),dens(jj),mdot(jj),taup(jj),vesc(jj),&
                          mdte(1)/solar_mass,mate(1)/solar_mass,opti,tau
            end if
            jj = jj + 1
          end do
        ! Some parameters
          n0    = 1.
          r0    = n0*nucleon_mass
          r23   = r0/1e-23
          t0    = max((time(ii)-timeedd),0.)
          t6    = t0/(1e6*year)
          if (switch) then     
            ll = ll + 1
            if (ll.eq.1) then 
              mdmn  = mdot(ii)
            else if (ll.gt.1) then 
              mdmn  = ( real(ll)*mdmn + mdot(ii) ) / real(ll+1)
            else 
              stop "WHAT IS THIS???"
            end if
          else
          end if
          m6    = abs(mdmn)/(1e-6*solar_mass/year)
          v3    = vesc(ii)/(1e3*1e3*1e2)
        ! Computing Luminosities
          Ledd  = 4.*pi*G*c*mate(ii)/kappa
          Lkin  = 0.5*mdmn*(vesc(ii)**2)
          Luse  = Ledd
          L36   = Luse/1e36
        ! Computing gas pressure
          Pres  = (7./((3850.*pi)**(2./5.)))*(Luse**(2./5.))*(r0**(3./5.))*(t0**(-4./5.))
          P12   = Pres/1e-12
        ! Computing Raddi and effective T
          rinit =  7.8*((sqrt(m6))*(sqrt(v3))/(sqrt(P12)))*parsec
          rterm = 52.9*(r23**(-1./5.))*(L36**(1./5.))*(t6**(3./5.))*parsec
          teff  = sqrt(sqrt(Ledd/(4.*pi*(rterm**2)*stefan_boltzmann)))
          write(68,*) time(ii)/year,interp,dens(kk),mdot(kk),taup(kk),tau,rterm,teff,Ledd,Lkin,Einj,mdmn*year/solar_mass,posi(kk)
        else
          kk = 1
        end if
      ! Switch if
      ! Envelope treatment is done
      end do
    ! Finished looping through time
      evotype = 0 
      if (switch) then 
        if (mdotdon(alloc).ge.mdotedd(alloc)) then 
          if (mdotdon(alloc).ge.0.1*(ma+md)*solar_mass/year) then 
            write(88,*) " '"//trim(adjustl(fullpath))//"/dens.dat' u 1:2 lc 3 lt -1 with lines notitle,\"
            evotype = 3
          else
            write(88,*) " '"//trim(adjustl(fullpath))//"/dens.dat' u 1:2 lc 2 lt -1 with lines notitle,\"
            evotype = 2
          end if
        else
            write(88,*) " '"//trim(adjustl(fullpath))//"/dens.dat' u 1:2 lc 1 lt -1 with lines notitle,\"
            evotype = 1
        end if
      else 
      end if
      if (tau.ge.1.) then 
        write(66,*) ma,md,posi(kk),dens(kk),mdot(kk)*year/solar_mass,vesc(kk),tau,taup(kk),time(alloc)/year,&
                    timeedd/year,timefin/year,Ledd,rinit,rterm,evotype,mdmn*year/solar_mass,mdotdon(alloc)*year/solar_mass
      else
        write(66,*) ma,md,1.0      ,1.0     ,1.0                    ,vesc(kk),tau,taup(kk),time(alloc)/year,&
                    timeedd/year,timefin/year,Ledd,rinit,rterm,evotype,mdmn*year/solar_mass,mdotdon(alloc)*year/solar_mass
      end if 
      close(67)
      close(68)
    end do
    close(20) 
    stat2 = 0
    write(* ,*) "Moving to a higher accretor mass"
    write(66,*) " "
! End of second loop 
  end do
! End of first loop
  end subroutine doit





  subroutine analyze(path)
  implicit none

  end subroutine analyze(path) 




  function L_depo(t)
  implicit none
  real :: L_depo,t
  L_depo
  end function L_depo 
