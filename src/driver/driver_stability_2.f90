  subroutine dr_perform_phase_2
  use io_vars, only: io_save,io_unit,io_data,io_path
  use io_interface, only: io_log,io_allocate_data,io_deallocate_data,&
                          io_save_data,io_open,io_log,io_2string
  use dr_interface, only: dr_critical_mass
  implicit none
  integer :: stat
  integer :: T_T_CRITERioN    = 0 
  integer :: EDD_CRITERioN    = 1
  integer :: DIS_CRITERioN    = 2
  real :: m_stable,m_stable_hi,m_stable_lo
  real :: m_disk,m_disk_hi,m_disk_lo
  real :: m_edd,m_edd_hi,m_edd_lo,err,m
! This file holds the original guesses for the critical masses
  call io_log("Phase 2 of stability analysis has been called")
  call io_log("Refining values for critical mass, disk mass and eddington mass.")
  open(unit=io_unit+7,file=trim(adjustl(io_path))//"/stability_1.dat",status="old")
  call io_open(io_path,"stability_2.dat") 
  stat = 0
  do while (stat.eq.0)
    read(io_unit+7,*,iostat=stat) m,m_stable_hi,m_disk_hi,m_edd_hi,&
                                    m_stable_lo,m_disk_lo,m_edd_lo
    m_stable = dr_critical_mass(m,m_stable_lo,m_stable_hi,T_T_CRITERioN,err) 
    m_disk   = dr_critical_mass(m,m_disk_lo,m_disk_hi,DIS_CRITERioN,err)
    m_edd    = dr_critical_mass(m,m_edd_lo ,m_edd_hi ,EDD_CRITERioN,err) 
    call io_allocate_data(4)
    io_data = (/ m,m_stable,m_disk,m_edd /)
    call io_save_data(io_path,"stability_2.dat")
    call io_deallocate_data
  end do
  close(io_unit+7)
  call io_log("Phase 2 of stability analysis is done")
  end subroutine dr_perform_phase_2
