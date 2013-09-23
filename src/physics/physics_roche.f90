! These fields assume all vectors are measured 
! from the center of mass

  function ph_roche_pot(v,v1,v2,m1,m2,ome)
  use ph_interface, only: ph_norm,ph_grav_pot,ph_cent_pot
  implicit none
  real, dimension(:) :: v,v1,v2
  real               :: m1,m2,ome,ph_roche_pot
  ph_roche_pot = ph_grav_pot(ph_norm(v-v1),m1) &
               + ph_grav_pot(ph_norm(v-v2),m2) &
               + ph_cent_pot(ph_norm(v),ome)
  return
  end function ph_roche_pot



  function ph_roche_acc(v,v1,v2,m1,m2,ome)
  use ph_interface, only: ph_norm,ph_grav_acc,ph_cent_acc
  implicit none 
  real, dimension(:) :: v,v1,v2
  real               :: m1,m2,ome
  real, dimension(size(v)) :: ph_roche_acc
  ph_roche_acc = ph_grav_acc(v-v1,m1) &
               + ph_grav_acc(v-v2,m2) &
               + ph_cent_acc(v,ome)
  end function ph_roche_acc
