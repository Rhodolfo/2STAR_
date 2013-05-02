! Non-inertial reference frame forces

subroutine ph_center_of_momentum(v1,v2,m1,m2,d)
  implicit none
  real               :: m1,m2
  real, dimension(:) :: v1,v2,d
  v1 =   d*m2/(m1+m2)
  v2 = - d*m1/(m1+m2)
end subroutine ph_center_of_momentum

function ph_cent_pot(pos,ome)
  implicit none
  real :: ome,pos,ph_cent_pot
  ph_cent_pot = (ome**2)*pos/2.
end function ph_cent_pot

function ph_cent_acc(pos,ome)
  implicit none
  real                       :: ome
  real, dimension(:)         :: pos
  real, dimension(size(pos)) :: ph_cent_acc
  ph_cent_acc = - (ome**2)*pos
end function ph_cent_acc

function ph_coriolis2d(vec,ome)
  implicit none
  real               :: ome
  real, dimension(2) :: vec, ph_coriolis2d
! This is OxV
  ph_coriolis2d(1) = - ome*vec(2)
  ph_coriolis2d(2) = + ome*vec(1)
! This is -2 Oxv
  ph_coriolis2d    = - 2.*ph_coriolis2d
end function ph_coriolis2d

function ph_coriolis3d(vec,ome)
  use physics, only: ph_cross
  implicit none
  real, dimension(3) :: vec, ome, ph_coriolis3d
  ph_coriolis3d    = - 2.*ph_cross(ome,vec)
end function ph_coriolis3d
