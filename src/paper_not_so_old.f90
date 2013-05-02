  program paper
  use driver, only: dr_restore_defaults,dr_perform
  use     IO, only: IO_read_command_line_options,IO_log,IO_path
  implicit none
  call dr_restore_defaults
  call IO_read_command_line_options
  call IO_log("[main] Hey there, man. I will make your figures.")
  call IO_log("[main] Data directory is set to "//trim(adjustl(IO_path))//" and has been created") 
  call doit
  end program paper










  subroutine doit
  use IO, only: IO_path
  implicit none
  real                          :: ma,md
  integer                       :: stat1,stat2
  character(len=100)            :: machar,mdchar,path,fullpath
  character(len=6)  , parameter :: f = "(f6.4)"

! Opening some files
  open(unit=66,file=trim(adjustl(IO_path))//"/papers",status="unknown")
  write(66,*) "# This is a test, and nothing more"
  close(66)
  open(unit=66,file=trim(adjustl(IO_path))//"/plot_dens.gpi",status="unknown")
  write(66,*) "# This is a test, and nothing more"
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
      call analyze(fullpath,ma,md)
    end do don
    close(20) 
    stat2 = 0
  end do acc
  close(10)
  end subroutine doit










  subroutine analyze(fullpath,ma,md)
  use IO     , only: IO_path
  use physics, only: c,pi,G,nucleon_mass,parsec,year,stefan_boltzmann,solar_mass
  implicit none
! Inout
  real                          :: ma,md
  character(len=100)            :: fullpath 
! Local
  character(len=1)              :: dum
  integer                       :: comment,alloc,ii,jj,kk,iimin,iimax,ll,evotype,stat
  logical                       :: switch,super,opti
  real                          :: t,kappa,tau,timeedd,n0,Ledd,rterm,l36,T6,teff,Lkin,Einj,timefin
  real                          :: t0,r0,r23,P12,Pres,m6,v3,rinit,Luse,mdmn,interp
  real, dimension(36)               :: tem
  real, dimension(:)  , allocatable :: mate,mdte,vesc,dens,mdot,time,posi,taup,mdotdon,mdotedd,mdotacc


! Let's get some info before doing anything
  open(unit=30,file=trim(adjustl(fullpath))//"/mdot.dat",status="old")
  dum = "#"
! Going through comments 
  comment = -1
  do while (dum.eq."#")
    read(30,*) dum
    comment = comment + 1
  end do
! Counting data points
  stat    = 0
  alloc   = -1
  do while (stat.eq.0)
    read(30,*,iostat=stat) t
    alloc = alloc + 1
  end do
  stat = 0
  close(30)

! Opening again with full knowledge
  open(unit=30,file=trim(adjustl(fullpath))//"/mdot.dat",status="old")
! Getting rid of comments
  do ii = 1,comment
    read(30,*) dum
  end do
  if (allocated(time)) deallocate(time,mdot,mdte,mate,posi,vesc,dens,taup,mdotdon,mdotedd,mdotacc)
  allocate(time(alloc),mdot(alloc),mdte(alloc),mate(alloc),posi(alloc),vesc(alloc),dens(alloc),taup(alloc))
  allocate(mdotdon(alloc),mdotedd(alloc),mdotacc(alloc))
! Now I have all data, can I get the wind thing going? 
  open(unit=67,file=trim(adjustl(fullpath))//"/dens.dat",status="unknown")
  open(unit=68,file=trim(adjustl(fullpath))//"/rad.dat",status="unknown")
  kappa   = 0.2
  switch  = .false.
  ll      = 0
  timeedd = 0.
  timefin = 0.
  Einj    = 0.
  mdmn    = 0.
  iimin   = alloc
  full: do ii = 1,alloc
  ! Reading in data
    read(30,*) tem
    time(ii)    = tem(1)*year
    mdotdon(ii) = abs(tem(5)*solar_mass/year)
    mdotacc(ii) = abs(tem(9)*solar_mass/year)
    mdotedd(ii) = tem(16)*solar_mass/year
    mdte(ii)    = tem(6)*solar_mass
    mate(ii)    = tem(10)*solar_mass
    if (ii.eq.1) vesc(ii) = sqrt(2.*G*(mdte(ii)+mate(ii))/tem(3))
    vesc(ii)    = vesc(1)
  ! Super eddington or not?
    if (mdotdon(ii).ge.mdotedd(ii)) then
      iimax    = ii
      mdot(ii) = abs(mdotdon(ii)-mdotacc(ii))
      if (.not.switch) then 
        iimin    = ii
        iimax    = ii 
        timeedd  = time(ii)
        switch   = .true.
      else 
      end if
      timefin = time(ii)
    else
      mdot(ii) = 0.
    end if
  ! Energy integral
    if (ii.eq.1) Einj = 0.5*mdot(ii)*(vesc(ii)**2)*(time(ii+1)-time(ii))
    if (ii.ne.1) Einj = Einj + 0.5*mdot(ii)*(vesc(ii)**2)*(time(ii)-time(ii-1))
  ! Putting in the density profile if we have gone super eddington
    tau    = 0.
  ! Eddington switch
    eddington: if (switch) then 
      opti   = .false.
      jj     = iimin
      taui: do while (jj.lt.iimax)
    ! Putting in data for the density and position
        posi(jj) = vesc(jj)*max((time(ii)-time(jj)),0.)
        dens(jj) = 0.
        if (posi(jj).gt.0.) dens(jj) = abs(mdot(jj)/(4.*pi*vesc(jj)*(posi(jj)**2)))
      ! Now to do the optical depth integral
        if (jj.eq.iimin) tau = 0.
        tau = tau + kappa*dens(jj)*abs(posi(jj-1)-posi(jj))
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
      end do taui
    ! Some parameters
      n0    = 1.
      r0    = n0*nucleon_mass
      r23   = r0/1e-23
      t0    = max((time(ii)-timeedd),0.)
      t6    = t0/(1e6*year)
      ll = ll + 1
    ! Taking the average of mdot
      if (ll.eq.1) then 
        mdmn  = mdot(ii)
      else if (ll.gt.1) then 
        mdmn  = ( real(ll)*mdmn + mdot(ii) ) / real(ll+1)
      else 
        stop "WHAT IS THIS???"
      end if
    ! Computing parameters for envelope treatment
      m6    = abs(mdmn)/(1e-6*solar_mass/year)
      v3    = vesc(ii)/(1e3*1e3*1e2)
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
    end if eddington
  end do full
! Finished looping through time



! Let's classify the system
  open(unit=88,file=trim(adjustl(IO_path))//"/plot_dens.gpi",status="unknown",position="append")
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
  close(88)



! Saving the data in appropriate places
  open(unit=88,file=trim(adjustl(IO_path))//"/papers",status="unknown",position="append") 
  if (tau.ge.1.) then 
    write(88,*) ma,md,posi(kk),dens(kk),mdot(kk)*year/solar_mass,vesc(kk),tau,taup(kk),time(alloc)/year,&
                timeedd/year,timefin/year,Ledd,rinit,rterm,evotype,mdmn*year/solar_mass,mdotdon(alloc)*year/solar_mass
  else
    write(88,*) ma,md,1.0      ,1.0     ,1.0                    ,vesc(kk),tau,taup(kk),time(alloc)/year,&
                timeedd/year,timefin/year,Ledd,rinit,rterm,evotype,mdmn*year/solar_mass,mdotdon(alloc)*year/solar_mass
  end if
  close(88)

  write(*,*) trim(adjustl(fullpath))," ",evotype,tau

  end subroutine analyze





  function paper_source(x,t) 
  implicit none
! Global things
  real :: t
  real, dimension(:)       :: x
  real, dimension(size(x)) :: paper_source
! Local things
  real    :: gamma_ism,gamma_bub,kappa,factor
  real    :: xi,eta,zeta,A,B
  integer :: s
  s = size(x)
! Some fundamental things about the ism and bubble gas
  gamma_ism = 5./3.
  gamma_bub = 4./3.
  kappa     = (1.+gamma_ism)/(gamma_ism-1.)
  factor    = 1.-(1./kappa)
! Other stuff
  xi   = (1.-(factor**(1./3.)))/(factor**(1./3.))
  eta  = ( factor / (gamma_bub - 1.) ) + ( factor / (kappa*(gamma_ism - 1.)) )
  zeta = 3. * ( (factor / (kappa*(gamma_ism-1.))) + ((factor**2) / 2.) )
! Coefficients for the DE
  A    = 3.*eta + zeta
  B    = 2.*eta + xi
  end function paper_source





  function L_depo(t)
  implicit none
  real :: L_depo,t
  end function L_depo 
