  module eos_root_table



  use eos_table_storage, only: imax,jmax
  implicit none
  character(len=2) :: eos_root_file
  integer          :: eos_root_pres
  integer          :: eos_id,eos_ih
  integer          :: eos_jd,eos_jh
  real :: eos_dens_rt(imax)
  real :: eos_temp_rt(jmax)
  real :: eos_pres_rt(imax,jmax)
  real :: eos_entr_rt(imax,jmax)
  contains



    subroutine eos_create_root_table
    use eos_vars, only: eos_dens,eos_temp,eos_pres,eos_entr
    use io_vars, only: io_unit
    use eos_interface, only: eos_helmholtz
    use eos_table_storage, only: d,t,imax,jmax
    implicit none
    integer :: i,j,u
  ! Setting arrays to zero
    eos_dens_rt = 0
    eos_temp_rt = 0 
  ! Reading table
    u = io_unit + 10
    call eos_read_helm_table
    eos_dens_rt = d
    eos_temp_rt = t
    call system("mkdir -p src/eos/rtable")
    open(unit=u,file="src/eos/rtable/rtable.dat",status="unknown")
    do i = 1,imax
      eos_dens = eos_dens_rt(i)
      do j = 1,jmax
        eos_temp = eos_temp_rt(j)
        call eos_helmholtz
        eos_pres_rt(i,j) = eos_pres  
        eos_entr_rt(i,j) = eos_entr
        write(u,*) eos_dens_rt(i),eos_temp_rt(j),eos_pres_rt(i,j),eos_entr_rt(i,j)
      end do
      write(u,*) " "
    end do
    close(u)
    return
    end subroutine eos_create_root_table



    subroutine eos_create_root_subtable(pmin,pmax,fname)
    use eos_vars, only: eos_dens,eos_temp,eos_pres,eos_entr
    use io_vars, only: io_unit
    use eos_interface, only: eos_helmholtz
    use eos_table_storage, only: d,t,imax,jmax
    implicit none  
    real, intent(in) :: pmin,pmax
    character*(*)    :: fname
    integer :: i,j,u
    real    :: pcurr
    u = io_unit + 10 
    open(unit=u,file="src/eos/rtable/rtable."//trim(adjustl(fname)),status="unknown")
    do i = 1,imax
      eos_dens = eos_dens_rt(i)
      do j = 1,jmax
        eos_temp = eos_temp_rt(j)  
        pcurr    = eos_pres_rt(i,j)
        if ((pcurr.ge.pmin).and.(pcurr.le.pmax)) then 
        write(u,*) eos_dens_rt(i),eos_temp_rt(j),eos_pres_rt(i,j),eos_entr_rt(i,j)
        end if
      end do
    end do
    close(u) 
    end subroutine eos_create_root_subtable


    subroutine eos_create_pres_brackets
    implicit none
    integer :: i     
    real    :: lo,hi
    character(2) :: na
    call eos_create_root_subtable(0.00,1e10,"lo") 
    do i = 10,44
      lo = 10**(real(i))
      hi = 10**(real(i+1))
      write(na,"(I2)") i
      call eos_create_root_subtable(lo,hi,trim(adjustl(na)))
    end do
    return
    end subroutine eos_create_pres_brackets



    subroutine eos_set_root_file(pres)
    implicit none
    real, intent(in) :: pres
    integer :: lp,i   
    lp = floor(log10(pres))
    write(eos_root_file,"(I2)") lp
    if (lp.lt.10) eos_root_file = "lo"
    return
    end subroutine eos_set_root_file
 
  end module eos_root_table
