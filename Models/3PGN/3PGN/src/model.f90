subroutine model(logL1,y)
use declarations
!use weather_site
use routines
implicit none
real (kind=8) :: logL1
integer, parameter :: nvariables = 8
real (kind=8) :: y(nmonths,nvariables,noOfSites)

	!assign parameters		
!	pFS2	=	0.3
!	pFS20	=	0.09
!	StemConst	=	0.095
!	StemPower	=	2.4
!	pRx	=	0.8
!	pRn	=	0.25
!	Tmin	=	6
!	Topt	=	16
!	Tmax	=	40
!	m0	=	0
!	fN0	=	1
!	MaxAge	=	50
!	nAge	=	4
!	rAge	=	0.95
!	gammaF1	=	0.013
!	gammaF0	=	0.001
!	tgammaF	=	6
!	gammaR	=	0.01
!	MaxCond	=	0.02
!	LAIgcx	=	3.33
!	CoeffCond	=	0.05
!	BLcond	=	0.2
!	SLA0	=	12
!	SLA1	=	4
!	tSLA	=	1.5
!	k	=	0.5
!	fullCanAge	=	2
!	MaxIntcptn	=	0.15
!	LAImaxIntcptn	=	0
!	alpha	=	0.055
!	klmax	=	1
!	krmax	=	1
!	komax	=	1
!	hc	=	1
!	aH	=	2.281282
!	bW	=	0.1302594
!	cD	=	0.528354
!	rg	=	0.47
!	dmC	=	0.48
!	Yr_C_i	=	1
!	Yl_C_i	=	1
!	O_C_i	=	1
!	fracBB0	=	0.75
!	fracBB1	=	0.15
!	tBB	=	2
!	rho1	=	0.45
!	kF	=	1
!	FR_site(1)	=	0.5
!	FR_site(2)	=	0.5
!	FR_site(3)	=	0.7
!	FR_site(4)	=	0.6
!	FR_site(5)	=	0.8
			
	fCalpha700	=	1.4
	fCg700	=	0.7
	fNn	=	1
	gammaN1	=	0
	gammaN0	=	0
	tgammaN	=	0
	ngammaN	=	1
	wSx1000	=	300
	thinPower	=	1.5
	mF	=	0
	mR	=	0.2
	mS	=	0.2
	MinCond	=	0
	rho0	=	0.45
	tRho	=	0
	aV	=	0
	nVB	=	0
	nVN	=	0
	Qa	=	-90
	Qb	=	0.8
	gDM_mol	=	24
	molPAR_MJ	=	2.3
	CO2	=	350
	Yr_N_i	=	0
	Yl_N_i	=	2
	O_N_i	=	6
	qir	=	300
	qil	=	21.9
	qh	=	29
	qbc	=	10
	el	=	0.2
	er	=	0.2
	Ncf	=	1.75

do siteNo = 1, noOfSites
Yr_C        = Yr_C_i
Yl_C        = Yl_C_i
O_C         = O_C_i
Yr_N        = Yr_N_i
Yl_N        = Yl_N_i
O_N         = O_N_i

!initialize
Irrig       = 0.0   
call initialiseSiteData (siteNo)
!Assign SWconstant and SWpower as function of Soil Class
SWconst	=	0.8 - 0.1 * SoilClass
SWpower	=	11. - 2. * SoilClass

MAI=0.
If (fNn==0.) then 
  fN0 = 1.
end if
fCalphax = fCalpha700 / (2. - fCalpha700)
fCg0 = fCg700 / (2. *fCg700 - 1.)
!initial ASW must be between min and max asw
if (MinASW > MaxASW) then 
  MinASW = MaxASW
end if 
if (ASW <= MinASW) then
  ASW = MinASW
else if (ASW >= maxASW) then 
  ASW = maxASW
end if
poolFractn = Max(0.,min(1.,poolFractn))
Irrig = 0. 		!to check
pooledSW = 0.
LAI = 0.

!initialize climate for the site
 do ii = 1, nmonths
    Tn(ii) = Tn_site(ii,siteNo)
    Tx(ii) = Tx_site(ii,siteNo)
    SolarRad(ii) = SolarRad_site(ii,siteNo)
    Rain(ii) = Rain_site(ii,siteNo)
    FrostDays(ii) = FrostDays_site(ii,siteNo)
 end do

!calculate climate variables  
 do ii = 1, nmonths
    Tav(ii) = 0.5 * (Tn(ii) + Tx(ii))
    VPDx(ii) = 6.1078 * Exp(17.269 * Tx(ii) / (237.3 + Tx(ii)))
    VPDn(ii) = 6.1078 * Exp(17.269 * Tn(ii) / (237.3 + Tn(ii)))
    VPD(ii) = 0.5 * (VPDx(ii) - VPDn(ii))
 end do

!initialize
  !Set age-dependent factors for this time step
  SLA = SLA1 + (SLA0 - SLA1) * Exp(-ln2 * (StandAge / tSLA) ** 2.)
  fracBB = fracBB1 + (fracBB0 - fracBB1) * Exp(-ln2 * (StandAge / tBB))
  Density = rho1 + (rho0 - rho1) * Exp(-ln2 * (StandAge / tRho))
  If (tgammaF * gammaF1 == 0) Then
    gammaFoliage = gammaF1
  Else
    kgammaF = 12 * Log(1 + gammaF1 / gammaF0) / tgammaF
    gammaFoliage = gammaF1 * gammaF0 / (gammaF0 + (gammaF1 - gammaF0) * Exp(-kgammaF * StandAge))
  End If
  gammaF = gammaFoliage
           
  pfsPower = Log(pFS20 / pFS2) / Log(20./2.)
  pfsConst = pFS2 / 2. ** pfsPower
  AvStemMass = WS * 1000. / StemNo                                 !kg/tree
  avD = StemConst*(((WS+WF) * 1000. )/ StemNo) ** (StemPower) !(AvStemMass/StemConst) ** (1/StemPower) !
  BasArea = (((avD / 200.) ** 2.) * Pi) * StemNo
  Height = aH*((WS + WF) * 1000. / StemNo)**bW !* StemNo ** cD
  LAI = WF * SLA * 0.1
  avLAI = LAI
  
  if (aV > 0) then
    StandVol = aV * avD ** nVB * StemNo ** nVN 
  else
    StandVol = WS * (1 - fracBB) / Density  
  end if
  oldVol = StandVol  

month=startMonth
 do ii = 1, nmonths
  month = month + 1
  if (month > 12) then
	month = 1
  end if
  !gets fraction of day when sun is "up"
  SLAt = Sin(Pi * Lat / 180.)
  cLat = Cos(Pi * Lat / 180.)
  sinDec = 0.4 * Sin(0.0172 * (dayOfYear(month) - 80.))
  cosH0 = -sinDec * SLAt / (cLat * Sqrt(1. - (sinDec) ** 2))
  If (cosH0 > 1.) Then
      fDayLength(ii) = 0.
    ElseIf (cosH0 < -1.) Then
      fDayLength(ii) = 1.
    Else
      fDayLength(ii) = Acos(cosH0) / Pi
  End If
  !calculate seconds of the day when the sun is up
  DayLength(ii) = fDayLength(ii) * 86400. 

  !calculate temperature modifier
  if (Tav(ii) <= Tmin) then 
    fT = 0
    else if (Tav(ii) >= Tmax) then
    fT = 0
    else 
    fT = ((Tav(ii) - Tmin) / (Topt - Tmin)) * ((Tmax - Tav(ii)) / (Tmax - Topt))**((Tmax - Topt) / (Topt - Tmin))
  end if
  
  !calculate VPD modifier
  fVPD = Exp(-CoeffCond * VPD(ii))
  
  !calculate soil water modifier
  MoistRatio = ASW / MaxASW
  fSW = 1. / (1. + ((1. - MoistRatio) / SWconst) ** SWpower)

  !calculate soil nutrition modifier
  if (fNn == 0) then
    fNutr = 1.
  else
    fNutr = 1. - (1. - fN0) * (1. - FR) ** fNn
  end if

  !calculate frost modifier
  fFrost = 1. - kF * (FrostDays (ii) /30.)
  
  !calculate age modifier
  if (nAge == 0) then
	fAge = 1
  else	
	RelAge = StandAge / MaxAge
	fAge = (1. / (1. + (RelAge / rAge) ** nAge))
  end if

   !calculate CO2 modifiers
   fCalpha = fCalphax * CO2 / (350 * (fCalphax - 1) + CO2)
   fCg = fCg0 / (1 + (fCg0 - 1) * CO2 / 350)

  !calculate physiological modifier applied to conductance and APARu
  PhysMod = min(fVPD, fSW) * fAge
  
  !determine gross and net biomass production

  !canopy cover and light interception.
  If (fullCanAge > 0. .and. StandAge < fullCanAge) then
    CanCover = (StandAge + 0.01) / fullCanAge
  Else
    CanCover = 1.
  end if
  lightIntcptn = (1 - (Exp(-k * LAI / CanCover)))
  
    !calculate PAR, APAR, APARu, GPP and NPP
  alphaC = alpha * fNutr * fT * fFrost * fCalpha * physMod
  epsilon = gDM_mol * molPAR_MJ * alphaC
  RAD = SolarRad(ii) * daysInMonth(month)        !    calculate CO2 modifiers
  RADint = RAD * lightIntcptn * CanCover
  GPP = epsilon * RADint / 100               !tDM/ha
  NPP = GPP * rg                             !assumes respiratory rate is constant
  !NPP = GPPdm - (GPPdm * rg) - (((WF * rf) + ((WS + WR) * rw)) * (Q10 ** ((Tav(ii) - 20) / 10)))   !tDM/ha  
  !if (NPP < 0) then
  !    NPP = 0 
  !end if

    !Now do the water balance ...
  
  !Penman-Monteith equation for computing canopy transpiration
  !in kg/m2/day, which is conmverted to mm/day.

  if (LAI <= LAIgcx) then
     gC = MinCond + (MaxCond - MinCond) * LAI / LAIgcx
    else
     gC = MaxCond
  end if
  CanCond = gC * PhysMod * fCg
      If (CanCond == 0) Then 
          CanCond = 0.0001
      end if
  netRad = Qa + qb * (SolarRad(ii) * 10 ** 6 / DayLength(ii))                ! SolarRad in MJ/m2/day --> W/m2
  defTerm = rhoAir * lambda * (VPDconv * VPD(ii)) * BLcond
  div = CanCond * (1 + e20) + BLcond
  Etransp = CanCond * (e20 * netRad + defTerm) / div           ! in J/m2/s
  CanopyTranspiration = Etransp / lambda * DayLength(ii)         ! converted to kg/m2/day
  
 !transpiration from Penman-Monteith (mm/day converted to mm/month)
  Transp = daysInMonth(month) * CanopyTranspiration
  
  !rainfall interception
      If (LAImaxIntcptn > 0.) Then 
         fracRainIntcptn = MaxIntcptn * Min(1., LAI / LAImaxIntcptn)
      else
         fracRainIntcptn = MaxIntcptn 
      end if
   RainIntcptn = Rain(ii) * fracRainIntcptn

  !do soil water balance
  SupIrrig = 0.
  RunOff = 0.     
  ASW = ASW + Rain(ii) + (100. * Irrig / 12.) + pooledSW
  EvapTransp = Min(ASW, Transp + RainIntcptn)          !ET can not exceed ASW
  excessSW = max(ASW - EvapTransp - MaxASW, 0.)
  ASW = ASW - EvapTransp - excessSW
  pooledSW = poolFractn * excessSW
  RunOff = (1. - poolFractn) * excessSW
  if (ASW < MinASW) then
	SupIrrig = MinASW - ASW
	ASW = MinASW
  end if  

  !correct for actual ET
  TranspScaleFactor = EvapTransp / (Transp + RainIntcptn)   !scales NPP and GPP
  GPP = TranspScaleFactor * GPP
  NPP = TranspScaleFactor * NPP
  WUE = NPP / EvapTransp
  
    !determine biomass increments and losses
  
  !calculate partitioning coefficients


  m = m0 + (1 - m0) * FR
  pFS = pfsConst * avD ** pfsPower
  pR = pRx * pRn / (pRn + (pRx - pRn) * PhysMod * m)
  pS = (1 - pR) / (1 + pFS)
  pF = 1 - pR - pS
  !calculate biomass increments
  incrWF = NPP * pF
  incrWR = NPP * pR
  incrWS = NPP * pS

  !calculate litterfall & root turnover -
  lossWF = gammaF * WF
  lossWR = gammaR * WR

  !Calculate end-of-month biomass
  WF = WF + incrWF - lossWF
  WR = WR + incrWR - lossWR
  WS = WS + incrWS
  WL = WL + lossWF
  TotalW = WF + WR + WS
 
  !Calculate soil carbon balance
  
  !First calculate the decomposition rate...
        
  kr = krmax * fSW * fT
  kl = klmax * fSW * fT
  ko = komax * fSW * fT
        
  !...and then calculate the fluxes in, out and between carbon and nitrogen pools
                   
  Yl_Coutflux = kl * (1 - hc) * Yl_C
  Yr_Coutflux = kr * (1 - hc) * Yr_C
  humification_l = kl * hc * Yl_C
  humification_r = kr * hc * Yr_C
  O_Coutflux = ko * O_C
  Yl_Noutflux = kl * ((1 - hc) / (1 - el)) * (Yl_N - el * (Yl_C / qbc))
  humification_N_l = kl * hc * (Yl_N / qh)
  humification_N_r = kr * hc * (Yr_N / qh)
  Yr_Noutflux = kr * ((1 - hc) / (1 - er)) * (Yr_N - er * (Yr_C / qbc))
  O_Noutflux = ko * O_N
      
  !Now calculate the end-of-month carbon and nitrogen pools
  Yr_C = Yr_C !+ (StemMort / 2) - Yr_Coutflux - humification_r
  Yl_C = Yl_C !+ ((lossWF + lossWR + FoliageMort + RootMort) / 2) - Yl_Coutflux - humification_l
  O_C = O_C + humification_l + humification_r - O_Coutflux
  Yr_N = Yr_N + (StemMort / (2 * qir)) - Yr_Noutflux - humification_N_r
  Yl_N = Yl_N + ((lossWF + lossWR + FoliageMort + RootMort) / (2 * qil)) - Yl_Noutflux - humification_N_l
  O_N = O_N + humification_N_r + humification_N_l - O_Noutflux
      
  TotalCarbo = Yr_C + Yl_C + O_C
  TotalNitro = Yr_N + Yl_N + O_N

  !Calculate the Fertility Rating
    
        !First calculate available nitrogen and uptake
        !Nav = Yr_Noutflux + Yl_Noutflux + O_Noutflux
        !Un = delWF * Ncf / 2
        
        !Now estimate FR
        
        !If Un = 0 Then FR = 1 Else FRin = Nav / Un
        
        !If FRin > 1 Then FR = 1 Else If FRin < 0 Then FR = 0 Else FR = FRin
        
        !FRsum = FRsum + FR
   
  GPP_C = GPPdm * dmC
  NPP_C = NPP * dmC
  Raut = GPP_C - NPP_C
  Rhet = Yl_Coutflux + Yr_Coutflux + O_Coutflux
  Reco = Raut + Rhet
  NEP = GPP_C - Reco

  !Update tree and stand data at the end of this time period,
  !taking mortality, thinning or defoliation into account
  
  StandAge = StandAge + 1./12.

  !Perform thinning or defoliation events for this time period
  !need to add thinning and defoliation rootins
  If (nThinning > 0 .and. countThinning <= nThinning) Then 
   If (StandAge >= site_thinning(countThinning,1)) Then
    If (StemNo > site_thinning(countThinning,2)) Then
      delN = (StemNo - site_thinning(countThinning,2)) / StemNo
      StemNo = StemNo * (1 - delN)
      WF = WF * (1 - delN * site_thinning(countThinning,3))
      WR = WR * (1 - delN * site_thinning(countThinning,4))
      WS = WS * (1 - delN * site_thinning(countThinning,5))
    End If
    countThinning = countThinning + 1
    if (countThinning > nThinning) then
     deallocate(site_thinning)
    end if
   End If
  End If

  !calculate age and stress-related mortality
  gammaN = gammaN1 + (gammaN0 - gammaN1) * Exp(-ln2 * (StandAge / tgammaN) ** ngammaN)
  if (gammaN > 0) then
	delStems = gammaN * StemNo / 12 /100	
	FoliageMort = mF * delStems * (WF / StemNo)
	RootMort = mR * delStems * (WR / StemNo)
	StemMort = mS * delStems * (WS / StemNo)
  	WF = WF - mF * delStems * (WF / StemNo)
	WR = WR - mR * delStems * (WR / StemNo)
	WS = WS - mS * delStems * (WS / StemNo)
	StemNo = StemNo - delStems
	mortality = mortality + delStems
  end if
                                        
  !calculate self-thinning mortality
wSmax = wSx1000 * (1000 / StemNo) ** thinPower
AvStemMass = WS * 1000 / StemNo
delStems = 0 
if (wSmax < AvStemMass) then
  accuracy = 1 / 1000
  n = StemNo / 1000
  x1 = 1000 * mS * WS / StemNo
  j = 0
  do
    j = j + 1
    x2 = wSx1000 * n ** (1 - thinPower)
    fN = x2 - x1 * n - (1 - mS) * WS
    dfN = (1 - thinPower) * x2 / n - x1
    dN = -fN / dfN
    n = n + dN
    If (abs(dN) <= accuracy) exit 
    if (j >= 5) exit
  end do
  getMortality = StemNo - 1000 * n
  delStems = getMortality
  FoliageMort = FoliageMort + mF * delStems * (WF / StemNo)
  RootMort = RootMort + mR * delStems * (WR / StemNo)
  StemMort = StemMort + mS * delStems * (WS / StemNo)
  WF = WF - mF * delStems * (WF / StemNo)
  WR = WR - mR * delStems * (WR / StemNo)
  WS = WS - mS * delStems * (WS / StemNo)
  StemNo = StemNo - delStems
  wSmax = wSx1000 * (1000 / StemNo) ** thinPower
  AvStemMass = WS * 1000 / StemNo
  selfThin = selfThin + delStems
end if

!update age-dependent factors
  SLA = SLA1 + (SLA0 - SLA1) * Exp(-ln2 * (StandAge / tSLA) ** 2.)
  fracBB = fracBB1 + (fracBB0 - fracBB1) * Exp(-ln2 * (StandAge / tBB))
  Density = rho1 + (rho0 - rho1) * Exp(-ln2 * (StandAge / tRho))
  If (tgammaF * gammaF1 == 0) Then
    gammaFoliage = gammaF1
  Else
    kgammaF = 12 * Log(1 + gammaF1 / gammaF0) / tgammaF
    gammaFoliage = gammaF1 * gammaF0 / (gammaF0 + (gammaF1 - gammaF0) * Exp(-kgammaF * StandAge))
  End If
  gammaF = gammaFoliage
  

!update stand characteristics
  LAI = WF * SLA * 0.1
  avD = StemConst*(((WS+WF) * 1000. )/ StemNo) ** (StemPower) !(AvStemMass/StemConst) ** (1/StemPower) !
  BasArea = (((avD / 200.) ** 2) * Pi) * StemNo
  Height = aH*((WS + WF) * 1000. / StemNo)**bW! * StemNo ** cD
  if (aV > 0) then
    StandVol = aV * avD ** nVB * StemNo ** nVN 
  else
    StandVol = WS * (1 - fracBB) / Density  
  end if
  CVI = StandVol - oldVol
  oldVol = StandVol  
  if (StandAge > 0) then
	MAI = StandVol / StandAge
  else
	MAI = 0
  end if

!!calculate annual NEP
do ijj=1,11
	NEPmat(ijj)=NEPmat(ijj+1)
end do
NEPmat(12)=NEP
aNEP=sum(NEPmat)

!store output
y(ii,1,siteNo) = StandAge
y(ii,2,siteNo) = NEP !aNEP !
y(ii,3,siteNo) = avD
y(ii,4,siteNo) = Height
y(ii,5,siteNo) = WF
y(ii,6,siteNo) = WR
y(ii,7,siteNo) = WS
y(ii,8,siteNo) = StandVol
!open (20, file='output.txt', action='write')
!write (20,21) y(ii,1), y(ii,2), y(ii,3)
!21 format (7f20.10)
end do
end do  
end subroutine model


