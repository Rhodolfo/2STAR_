  subroutine cp_envelope
  use physics  , only: pi,solar_mass,year,stefan_boltzmann,G
  use driver   , only: dr_time
  use component, only: cp_cons_wind,cp_rtau_wind,cp_teff_wind, &
                       cp_cons_rad,cp_rtau_rad,cp_teff_rad, &
                       cp_cons_bubble,cp_rtau_bubble,cp_teff_bubble, &
                       cp_mdot_donor,cp_accretion_efficiency,cp_v_escape,&
                       cp_total_mass,cp_ejected_mass,cp_L_eddington,cp_opacity
  implicit none
  real :: mdot,n0,parsec,Lkin
  n0     = 1.
  mdot   = max(abs(cp_mdot_donor)*(1.-cp_accretion_efficiency),1e-16*solar_mass/year)
  parsec = 3.08567758e18
  Lkin   = 0.5*mdot*(cp_v_escape**2)

! Properties of a wind emvelope
  cp_cons_wind = abs(mdot)/(4.*pi*cp_v_escape)
  cp_rtau_wind = cp_cons_wind*cp_opacity
  cp_teff_wind = sqrt(sqrt(cp_L_eddington/(4.*pi*(cp_rtau_wind**2)*stefan_boltzmann)))
  if (cp_rtau_wind.ge.Huge(cp_rtau_wind)) cp_rtau_wind = Huge(cp_rtau_wind)
  if (cp_teff_wind.ge.Huge(cp_teff_wind)) cp_teff_wind = Huge(cp_teff_wind)

! Properties of a radiation supported envelope
  cp_cons_rad  = (G*cp_total_mass/(4.*1.))
  cp_rtau_rad  = 1.7e15*sqrt(2.*cp_ejected_mass/solar_mass)
  cp_teff_rad  = sqrt(sqrt(cp_L_eddington/(4.*pi*(cp_rtau_rad **2)*stefan_boltzmann)))
  if (cp_rtau_rad .ge.Huge(cp_rtau_rad )) cp_rtau_rad  = Huge(cp_rtau_rad )
  if (cp_teff_rad .ge.Huge(cp_teff_rad )) cp_teff_rad  = Huge(cp_teff_rad )

! Properties of the reverse shock bubble thing
  cp_cons_bubble = 1.
  cp_rtau_bubble = (27./(n0**(1./5.)))*((Lkin/1e36)**(1./5.))*((dr_time/(year*1e6))**(3./5.))*parsec
  cp_teff_bubble = sqrt(sqrt(cp_L_eddington/(4.*pi*(cp_rtau_bubble**2)*stefan_boltzmann)))
  
  end subroutine cp_envelope
