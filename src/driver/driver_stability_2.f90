  subroutine dr_perform_phase_2
  use driver, only: dr_critical_mass
  use     IO, only: IO_save,IO_unit,IO_data,IO_allocate_data,IO_deallocate_data,&
                    IO_save_data,IO_open,IO_path,IO_log,IO_2string
  implicit none
  integer :: stat
  integer :: T_T_CRITERION    = 0 
  integer :: EDD_CRITERION    = 1
  integer :: DIS_CRITERION    = 2
  real :: m_stable,m_stable_hi,m_stable_lo
  real :: m_disk,m_disk_hi,m_disk_lo
  real :: m_edd,m_edd_hi,m_edd_lo,err,m
! This file holds the original guesses for the critical masses
  call IO_log("Phase 2 of stability analysis has been called")
  call IO_log("Refining values for critical mass, disk mass and eddington mass.")
  open(unit=IO_unit+7,file=trim(adjustl(IO_path))//"/stability_1.dat",status="old")
  call IO_open(IO_path,"stability_2.dat") 
  stat = 0
  do while (stat.eq.0)
    read(IO_unit+7,*,iostat=stat) m,m_stable_hi,m_disk_hi,m_edd_hi,&
                                    m_stable_lo,m_disk_lo,m_edd_lo
    m_stable = dr_critical_mass(m,m_stable_lo,m_stable_hi,T_T_CRITERION,err) 
    m_disk   = dr_critical_mass(m,m_disk_lo,m_disk_hi,DIS_CRITERION,err)
    m_edd    = dr_critical_mass(m,m_edd_lo ,m_edd_hi ,EDD_CRITERION,err) 
    call IO_allocate_data(4)
    IO_data = (/ m,m_stable,m_disk,m_edd /)
    call IO_save_data(IO_path,"stability_2.dat")
    call IO_deallocate_data
  end do
  close(IO_unit+7)
  call IO_log("Phase 2 of stability analysis is done")
  end subroutine dr_perform_phase_2
