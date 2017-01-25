module routines
use declarations
!use parameters
!use weather
implicit none

contains


subroutine readSiteData()
implicit none

  open(30,file='input/siteData.txt')
  do i=1,noOfSites
    read(30,*)  number_site(i),name_site(i),lat_site(i),StemNo_site(i),&
		SoilClass_site(i),ASW_site(i),MinASW_site(i),MaxASW_site(i),&
		poolFractn_site(i),startAge_site(i), endAge_site(i) ,startMonth_site(i),&
		WF_i_site(i),WR_i_site(i),WS_i_site(i),nThinning_site(i)!,FR_site(i)
  enddo
  close(30)

  totThinning = sum(nThinning_site)
  if (totThinning > 0) then
   allocate(thinning(totThinning,6))
   open(15, file='input/thinning.txt')
    do i = 1,totThinning
	read(15,*) thinning(i,:)
    enddo
   close(15)
  end if

end subroutine readSiteData

subroutine initialiseSiteData (siteNo)
implicit none
integer :: siteNo
!name	=	name_site	(	siteNo	)
Lat	=	Lat_site	(	siteNo	)
StemNo	=	StemNo_site	(	siteNo	)
SoilClass =	SoilClass_site		(	siteNo	)
FR	=	FR_site		(	siteNo	)
ASW	=	ASW_site	(	siteNo	)
MinASW	=	MinASW_site	(	siteNo	)
MaxASW	=	MaxASW_site	(	siteNo	)
poolFractn	=	poolFractn_site(siteNo)
standAge	=	startAge_site(siteNo)
startMonth	=	startMonth_site(siteNo)
WF	=	WF_i_site(siteNo)
WR	=	WR_i_site(siteNo)
WS	=	WS_i_site(siteNo)
nThinning	=	nThinning_site(siteNo)
if (nThinning > 0) then
 countThinning	=	1
 allocate(site_thinning(nThinning,6))
 ij = 0
 do jj = 1,totThinning
  if (thinning(jj,6) == siteNo) then
   ij = ij+1
   site_thinning(ij,:) = thinning(jj,:)
  end if
 end do
end if
end subroutine initialiseSiteData

subroutine readMonthlyClimateData
!Read monthly climatic factors.
!read for a single site in a monthly run
implicit none

  do siteNo = 1,noOfSites
!initialize climate for the site
	open(140,file='input/weather_'//number_site(siteNo)//'.txt')!name_site(i))!//'.txt')
	do i=1,nmonths
	  read(140,*) Tx_site(i,siteNo),Tn_site(i,siteNo),Rain_site(i,siteNo),SolarRad_site(i,siteNo),FrostDays_site(i,siteNo) !read climatic variables (Tx, Tn, Rain, SolarRad, RainDays, FrostDays,daysInMonth, dayOfYear)
	enddo
	close(140) 

  end do
end subroutine readMonthlyClimateData

subroutine allocaz (nmts,nSts)
 implicit none
 integer :: nmts,nSts

! allocate(climate(nmonths,5,noOfSites))
 allocate(Lat_site(noOfSites))
 allocate(StemNo_site(noOfSites))
 allocate(ASW_site(noOfSites))
 allocate(MinASW_site(noOfSites))
 allocate(MaxASW_site(noOfSites))
 allocate(FR_site(noOfSites))
 allocate(poolFractn_site(noOfSites))
 allocate(startAge_site(noOfSites))
 allocate(WF_i_site(noOfSites))
 allocate(WR_i_site(noOfSites))
 allocate(WS_i_site(noOfSites))
 allocate(nThinning_site(noOfSites))
 allocate(startMonth_site(noOfSites))
 allocate(SoilClass_site(noOfSites))
 allocate(endAge_site(noOfSites))
 allocate(fDayLength(nmonths))
 allocate(Tav(nmonths))
 allocate(SolarRad(nmonths))
 allocate(VPD(nmonths))
 allocate(Tn(nmonths))
 allocate(Tx(nmonths))
 allocate(Rain(nmonths))
 allocate(VPDx(nmonths))
 allocate(VPDn(nmonths))
 allocate(RainDays(nmonths))
 allocate(FrostDays(nmonths))
 allocate(DayLength(nmonths))
 allocate(SolarRad_site(nmonths,noOfSites))
 allocate(Tn_site(nmonths,noOfSites))
 allocate(Tx_site(nmonths,noOfSites))
 allocate(Rain_site(nmonths,noOfSites))
 allocate(FrostDays_site(nmonths,noOfSites))
 allocate(number_site(noOfSites))
 allocate(name_site(noOfSites))

end subroutine allocaz

subroutine deallocaz
 implicit none

! deallocate(climate)
 deallocate(Lat_site)
 deallocate(StemNo_site)
 deallocate(ASW_site)
 deallocate(MinASW_site)
 deallocate(MaxASW_site)
 deallocate(FR_site)
 deallocate(poolFractn_site)
 deallocate(startAge_site)
 deallocate(WF_i_site)
 deallocate(WR_i_site)
 deallocate(WS_i_site)
 deallocate(nThinning_site)
 deallocate(startMonth_site)
 deallocate(SoilClass_site)
 deallocate(endAge_site)
 deallocate(fDayLength)
 deallocate(Tav)
 deallocate(SolarRad)
 deallocate(VPD)
 deallocate(Tn)
 deallocate(Tx)
 deallocate(Rain)
 deallocate(VPDx)
 deallocate(VPDn)
 deallocate(RainDays)
 deallocate(FrostDays)
 deallocate(DayLength)
 deallocate(SolarRad_site)
 deallocate(Tn_site)
 deallocate(Tx_site)
 deallocate(Rain_site)
 deallocate(FrostDays_site)
 deallocate(number_site)
 deallocate(name_site)

end subroutine deallocaz

end module routines
