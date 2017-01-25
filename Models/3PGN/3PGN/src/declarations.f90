module declarations
implicit none
 
!general
!time of simulation
integer :: nmonths, noOfSites
 
 character(8), allocatable, dimension (:) :: number_site
 character(8), allocatable, dimension (:) :: name_site
integer :: ii, siteNo, month, startMonth, jj, ij, totThinning,ijj
!real(kind=8), dimension(nmonths,5,noOfSites) :: climate !the third dimension is noOfSites
real (kind=8), dimension(12) :: NEPmat=0
real(kind=8) :: aNEP
real(kind=8), allocatable, dimension(:) :: Lat_site, StemNo_site, ASW_site, MinASW_site, MaxASW_site, &
FR_site, poolFractn_site, startAge_site, WF_i_site,WR_i_site,WS_i_site, startMonth_site, SoilClass_site, endAge_site !(noOfSites)
integer, allocatable, dimension(:) :: nThinning_site
real (kind=8), dimension(12) :: daysInMonth = (/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./), &
dayOfYear = (/16.,44.,75.,105.,136.,166.,197.,228.,258.,289.,319.,350./)
					
real (kind=8) :: Pi = 3.141592654, ln2 = 0.693147181
real (kind=8) :: Lat
integer :: i
real (kind=8) :: SLAt 
real (kind=8) :: cLat
real (kind=8) :: sinDec
real (kind=8) :: cosH0
real (kind=8), allocatable, dimension(:) :: fDayLength
real (kind=8) :: MoistRatio
real (kind=8) :: RelAge
real (kind=8) :: alphaC
real (kind=8) :: m
  !The following are constants in the PM formula (Landsberg & Gower, 1997)
  real (kind=8), parameter :: e20 = 2.2          !rate of change of saturated VP with T at 20C
  real (kind=8), parameter :: rhoAir = 1.2       !density of air, kg/m3
  real (kind=8), parameter :: lambda = 2460000   ! latent heat of vapourisation of H2O (J/kg)
  real (kind=8), parameter :: VPDconv = 0.000622 ! convert VPD to saturation deficit = 18/29/1000
  real (kind=8) :: netRad, defTerm, div, Etransp

!climate input
real(kind=8), allocatable, dimension (:) :: Tav,SolarRad,VPD,Tn,Tx,Rain,VPDx,VPDn,RainDays,FrostDays,DayLength
real(kind=8), allocatable, dimension (:,:) :: SolarRad_site, Tn_site,Tx_site,Rain_site,FrostDays_site

! fT  
real (kind=8) :: fT, Tmin, Topt, Tmax

!fVPD
real (kind=8) :: fVPD, CoeffCond

!fSW
real (kind=8) :: fSW, ASW, SWconst, SWpower,SoilClass

!fNutr
real (kind=8) :: fNutr, fN0, FR, fNn

!fCalpha
real (kind=8) :: fCalpha

!fFrost
real (kind=8) :: fFrost, kF

!CO2 modifier
real (kind=8) :: fCalpha700, fCalphax, fCg0, fCg700

!fAge
!real (kind=8), dimension(nmonths) :: StandAge
real (kind=8) :: fAge, MaxAge, rAge, nAge, StandAge

!PhysMod
real (kind=8) :: PhysMod

!gross and net biomass production
real (kind=8) :: CanCover, fullCanAge, lightIntcptn, k, LAI

!calculate PAR, APAR, APARu, GPP and NPP
real (kind=8) :: RAD, PAR, molPAR_MJ, APAR, APARu, alpha, GPPmolc, GPPdm, gDM_mol, NPP, WF, WS, WR, TotalW, &
                  WF_month, WS_month, WR_month, epsilon, GPP, CO2, fCg, RADint

!respiration parameters
real (kind=8) :: rg, rf, rw, Q10

!derived parameters
real (kind=8) :: pfsPower, pfsConst

!carbon allocation routine and mensurational data
real (kind=8) :: m0, pFS20, pFS2, AvStemMass, avD, StemConst, StemPower, BasArea, Height, aH, bW, &
        cD, SLA, SLA1, SLA0, tSLA, avLAI, pFS, pR, pRx, pRn, pS, pF, fracBB0, fracBB1, tBB, Density, &
         StemNo, fracBB, StandVol, MAI, aV, nVB, nVN, oldVol, rho0,rho1, tRho, CVI

!calculate soil carbon balance
!mortality, litterfall and turnover, self thinning law        
real (kind=8) :: incrWF, incrWR, incrWS, Littfall, gammaF1, gammaF0, tgammaF, gammaR, kgammaF, &
        delStems, mS, mF, mR, WL, wSx1000, thinPower, n, x1, x2, fN, gammaN, gammaN1, gammaN0, tgammaN, ngammaN, &
        getMortality, accuracy, delStemNo, dfN, dN, j, wSmax,gammaF, gammaFoliage, &
	lossWF,lossWR, mortality, FoliageMort, RootMort, StemMort, selfThin
        
! Soil carbon and nitrogen balances
real (kind=8) :: kr, krmax, kl, klmax, ko, komax, Yl_Coutflux, hc, Yl_C, Yr_Coutflux, Yr_C, humification_l, humification_r, & 
        O_Coutflux, O_C, Yl_Noutflux, el, Yl_N, qbc, humification_N_l, qh, humification_N_r, Yr_N, Yr_Noutflux, &
        er, O_Noutflux, O_N, qir, qil, Ncf, TotalCarbo, TotalNitro
 
! Water balance
real (kind=8) :: CanCond, MaxCond, LAIgcx, Qa, qb, BLcond, gC, CanopyTranspiration, Transp, LAImaxIntcptn, MaxIntcptn, &
        EvapTransp, Irrig, cumIrrig, MinASW, MaxASW, poolFractn, MinCond, pooledSW, fracRainIntcptn,RainIntcptn, &
	excessSW, RunOff, supIrrig,TranspScaleFactor, WUE
        
! Carbon fluxes (MG/ha)
real (kind=8) :: GPP_C, NPP_C, Raut, Rhet, Reco, dmC, NEP

! systematic error for height, DBH and NEP in the Bayesian Calibration
real (kind=8) :: ESYSH, ESYSDBH, ESYSNEP

! thinning
integer :: nThinning,countThinning
real(kind=8) :: delN
real(kind=8), allocatable, dimension(:,:) :: thinning
real(kind=8), allocatable, dimension(:,:) :: site_thinning

!management aspects
!stand caratheristics
real (kind=8) :: WF_i
real (kind=8) :: WS_i
real (kind=8) :: WR_i

!site caractheristics
real (kind=8) :: Yr_C_i
real (kind=8) :: Yl_C_i
real (kind=8) :: O_C_i
real (kind=8) :: Yr_N_i
real (kind=8) :: Yl_N_i
real (kind=8) :: O_N_i

end module declarations
