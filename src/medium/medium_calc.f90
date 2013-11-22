  subroutine medium_calc(fullpath,ma,md)
  use io_vars, only: io_path
  use ph_vars, only: ph_c,ph_pi,ph_G,ph_amu,ph_parsec,ph_year,ph_sigma_sb,ph_msun
  use md_interface, only: md_init
  implicit none
! Inout
  real                          :: ma,md
  character(len=100)            :: fullpath 
! Local
  character(len=1)              :: dum
  integer                       :: comment,alloc,ii,jj,kk,iimin,iimax,ll,evotype,stat,jjcurrent
  logical                       :: switch,super,opti
  real                          :: t,opact,tau,timeedd,n0,Ledd,rterm,l36,T6,teffs,tefft,Lkin,Einj,timefin
  real                          :: t0,r0,r23,P12,Pres,m6,v3,rinit,Luse,mdmn,interp
! Data arrays
  real, dimension(30)               :: tem
  real, dimension(:)  , allocatable :: mate,mdte,vesc,dens,mdot,time,posi,taup,mdotdon,mdotedd,mdotacc,Lum,sepa,teff

! I do three things in this routine, all of which only apply when super eddington
! 1. Compute density profiles for free winds, then integrating for the optical depth
! 2. Compute the bubble model parameters according to an analytical model
! 3. Compute the bubble model parameters according to a numerical scheme ! ON HOLD
  call md_init 





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
  if (allocated(time)) deallocate(time,mdot,mdte,mate,posi,vesc,dens,taup,mdotdon,mdotedd,mdotacc,Lum,sepa,teff)
  allocate(time(alloc),mdot(alloc),mdte(alloc),mate(alloc),posi(alloc),vesc(alloc),dens(alloc),taup(alloc))
  allocate(mdotdon(alloc),mdotedd(alloc),mdotacc(alloc),Lum(alloc),sepa(alloc),teff(alloc))
! Now I have all data, can I get the wind thing going? 
  open(unit=67,file=trim(adjustl(fullpath))//"/dens.dat",status="unknown")
  open(unit=68,file=trim(adjustl(fullpath))//"/rad.dat",status="unknown")
  opact   = 0.2
  switch  = .false.
  ll      = 0
  timeedd = 0.
  timefin = 0.
  Einj    = 0.
  mdmn    = 0.
  iimin   = alloc





! Main loop goes here
  full: do ii = 1,alloc


  ! Reading in data
    read(30,*) tem
    time(ii)    = tem(1)*ph_year
    mdotdon(ii) = abs(tem(5)*ph_msun/ph_year)
    mdotacc(ii) = abs(tem(9)*ph_msun/ph_year)
    mdotedd(ii) = tem(16)*ph_msun/ph_year
    mdte(ii)    = tem(6)*ph_msun
    mate(ii)    = tem(10)*ph_msun
    Ledd  = 4.*ph_pi*ph_G*ph_c*mate(ii)/opact
    if (ii.eq.1) then 
      sepa(ii) = tem(3)
      vesc(ii) = sqrt(2.*ph_G*(mdte(ii)+mate(ii))/sepa(ii))
    end if
    sepa(ii)    = sepa(1)
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
      opti      = .false.
      jj        = iimin
      jjcurrent = jj - 1



    ! This is the wind calculation 
      taui: do while (jj.lt.iimax)
    ! Putting in data for the density and pos
        if (ii.ge.jj) then 
          posi(jj) = vesc(jj)*(time(ii)-time(jj)) + sepa(ii)
          dens(jj) = abs(mdot(jj)/(4.*ph_pi*vesc(jj)*(posi(jj)**2)))
        else
          posi(jj) = 0.
          dens(jj) = 0.
        end if
      ! Now to do the optical depth integral
        if (jj.eq.iimin) then 
          tau = 0.
        else
          tau = tau + opact*((dens(jj)+dens(jj-1))/2.)*abs(posi(jj-1)-posi(jj))
        ! tau = tau + opact*dens(jj)*abs(posi(jj-1)-posi(jj))
        end if
        taup(jj) = tau
      ! Detecting optical radius
        if (tau.ge.1.0) then 
          if (.not.opti) then 
          kk   = jj
          opti = .true.
          interp = posi(kk-1) + (posi(kk)-posi(kk-1))*(1.-taup(kk-1))/(taup(kk)-taup(kk-1))
          tefft  = sqrt(sqrt(Ledd/(4.*ph_pi*ph_sigma_sb*(interp**2))))
          end if
        else
          if (.not.opti) then 
          kk   = iimin
          end if
        end if
        teff(jj) = sqrt(sqrt(Ledd/(4.*ph_pi*ph_sigma_sb*(posi(jj)**2))))
      ! I have the optical radius
        if (ii.eq.alloc) then 
          if (jj.eq.iimin) write(67,*) "# radius dens mdot tau vesc teff mdon macc optiflag tautot"
          write(67,*) posi(jj),dens(jj),mdot(jj),taup(jj),vesc(jj),teff(jj),&
                      mdte(1)/ph_msun,mate(1)/ph_msun,opti,tau
        end if
        jj = jj + 1
      end do taui

    ! This is the analytical bubble calculation
      n0    = 1.
      r0    = n0*ph_amu
      r23   = r0/1e-23
      t0    = max(timefin-timeedd,0.)
      t6    = t0/(1e6*ph_year)
      ll = ll + 1
    ! Taking the average of mdot
      if (ll.eq.1) then 
        mdmn  = mdot(ii)
      else if (ll.gt.1) then 
        mdmn  = ( real(ll)*mdmn + mdot(ii) ) / real(ll+1)
      else 
        stop "WHAT IS THIS? WHAT IS THIS???"
      end if
    ! Computing parameters for envelope treatment
      m6    = abs(mdmn)/(1e-6*ph_msun/ph_year)
      v3    = vesc(ii)/(1e3*1e3*1e2)
      Lkin  = 0.5*mdmn*(vesc(ii)**2)
      Luse  = Ledd
      L36   = Luse/1e36
    ! Computing gas pressure
      Pres  = (7./((3850.*ph_pi)**(2./5.)))*(Luse**(2./5.))*(r0**(3./5.))*(t0**(-4./5.))
      P12   = Pres/1e-12
    ! Computing Raddi and effective T
      rinit =  7.8*((sqrt(m6))*(sqrt(v3))/(sqrt(P12)))*ph_parsec
      rterm = 52.9*(r23**(-1./5.))*(L36**(1./5.))*(t6**(3./5.))*ph_parsec
      teffs = sqrt(sqrt(Ledd/(4.*ph_pi*(rterm**2)*ph_sigma_sb)))

    ! Writting the data to a file
      write(68,*) time(ii)/ph_year,interp,dens(kk),mdot(kk)*ph_year/ph_msun,taup(kk),tefft,tau,&
                  rinit,rterm,teffs,Ledd,Lkin,Einj,mdmn*ph_year/ph_msun,md,ma

    else
      kk = 1
    end if eddington
  end do full





! Let's classify the system
  open(unit=88,file=trim(adjustl(io_path))//"/plot_dens.gpi",status="old",position="append")
  open(unit=89,file=trim(adjustl(io_path))//"/plot_teff.gpi",status="old",position="append") 
  evotype = 0 
  if (switch) then 
    if (mdotdon(alloc).ge.mdotedd(alloc)) then 
      if (mdotdon(alloc).ge.0.1*(ma+md)*ph_msun/ph_year) then 
        write(88,*) " '"//trim(adjustl(fullpath))//"/dens.dat' u 1:2:($7/$8)   lt palette with lines notitle,\" 
        write(89,*) " '"//trim(adjustl(fullpath))//"/rad.dat'  u 1:6:($15/$16) lt palette with lines notitle,\" 
        evotype = 3
      else
        evotype = 2
      end if
    else
        evotype = 1
    end if
  else 
  end if
  close(88)
  close(89)





! Saving the data in appropriate places
  open(unit=88,file=trim(adjustl(io_path))//"/stability_4.dat",status="old",position="append") 
  if (tau.ge.1.) then 
    write(88,*) ma,md,posi(kk),dens(kk),mdot(kk)*ph_year/ph_msun,&
                vesc(kk),tau,taup(kk),time(alloc)/ph_year,timeedd/ph_year,&
                timefin/ph_year,Ledd,rinit,rterm,evotype,&
                mdmn*ph_year/ph_msun,mdotdon(alloc)*ph_year/ph_msun
  else
    write(88,*) ma,md,1.0      ,1.0     ,1.0                    ,&
                vesc(kk),tau,taup(kk),time(alloc)/ph_year,timeedd/ph_year,&
                timefin/ph_year,Ledd,rinit,rterm,evotype,&
                mdmn*ph_year/ph_msun,mdotdon(alloc)*ph_year/ph_msun
  end if
  close(88)





! Exit
  write(*,*) trim(adjustl(fullpath))," ",evotype,log10(interp),log10(rinit),log10(rterm)
  deallocate(time,mdot,mdte,mate,posi,vesc,dens,taup,mdotdon,mdotedd,mdotacc,Lum,sepa,teff)
  return 
  end subroutine medium_calc
