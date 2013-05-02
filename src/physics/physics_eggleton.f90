function ph_eggleton_formula(some_number)
  implicit none
  real :: some_number
  real :: some_number_2_3
  real :: some_number_1_3
  real :: ph_eggleton_formula
! Coding up the Eccleton formula
  some_number_1_3 = some_number**(1.0/3.0)
  some_number_2_3 = some_number**(2.0/3.0)
  ph_eggleton_formula = ( 0.49*some_number_2_3 ) / & 
  ( 0.60*some_number_2_3 + log( 1.0 + some_number_1_3) )
return
end function ph_eggleton_formula





function ph_eggleton_formula_zeta(some_number,some_eff)
  implicit none
  real :: some_number
  real :: some_eff
  real :: some_number_1_3
  real :: some_number_2_3
  real :: factor_1
  real :: factor_2
  real :: factor_3
  real :: denominator
  real :: ph_eggleton_formula_zeta
! Coding up the Eggleton formula logarithmic derivative
  some_number_1_3 = some_number**(1.0/3.0)
  some_number_2_3 = some_number**(2.0/3.0)
! Now for realsie**(-1.0))**(-1.0)s
  factor_1    = (1.0 + some_number) / 3.0
  factor_1    = (1.0 + some_eff*some_number) / 3.0
  factor_2    = 2.0*log(1.0+some_number_1_3)
  factor_3    = (1.0+(some_number_1_3)**(-1.0))**(-1.0)
  denominator = 0.6*some_number_2_3 + log(1.0+some_number_1_3)
  ph_eggleton_formula_zeta = factor_1*(factor_2-factor_3)/denominator
return
end function ph_eggleton_formula_zeta





function ph_eggleton_L1(some_number)
  implicit none
  real :: some_number,ph_eggleton_L1
  real :: some_number_1_3
  some_number_1_3 = some_number**(1./3.)
  ph_eggleton_L1  = log(1.+some_number_1_3)/(((some_number**2.)/3.)**(1./3.))
  ph_eggleton_L1  = 1. / (1.+ph_eggleton_L1)
end function ph_eggleton_L1





function ph_eggleton_rmin(q)
  implicit none
  real :: q,ph_eggleton_rmin
  real :: logq
  logq = log10(q)
  ph_eggleton_rmin = 0.04948 - 0.03815*logq + 0.04752*(logq**2) - 0.006973*(logq**3)      
end function ph_eggleton_rmin





function ph_eggleton_rcirc(q)
 implicit none
 real :: q,ph_eggleton_rcirc
 real :: logq
  logq = log10(q)
  ph_eggleton_rcirc = 0.08830 - 0.04858*logq + 0.11489*(logq**2) + 0.020475*(logq**3) 
end function ph_eggleton_rcirc
