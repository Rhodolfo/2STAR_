  function medium_Lfunc(t) 
  use medium, only: time,Lum,jjcurrent
  implicit none
  real :: t,medium_Lfunc
! Sanity checks
  if (.not.allocated(time)) stop "t unallocated"
  if (.not.allocated(Lum )) stop "L unallocated" 
  if (t.lt.time(jjcurrent  )) then 
    write(*,*) "t undershot",t,time(jjcurrent),time(jjcurrent+1)
    stop
  end if
  if (t.gt.time(jjcurrent+1)) then 
    write(*,*) "t overshot" ,t,time(jjcurrent),time(jjcurrent+1)
    stop
  end if
! Interpolate
  if (jjcurrent.lt.1) then 
    medium_Lfunc = 0.
    return
  else 
    medium_Lfunc = Lum(jjcurrent) + (Lum(jjcurrent+1)-Lum(jjcurrent))*(t-time(jjcurrent))/(time(jjcurrent+1)-time(jjcurrent))
  end if
  end function medium_Lfunc
