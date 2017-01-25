Subroutine init_model
use declarations
use routines
implicit none
! wrapper program to initialise 3PGN for R
! David Cameron, CEH-Edinburgh (Sept 2009)
! dcam@ceh.ac.uk

!====================================================
! Read in weather data and set parameters
!====================================================

nmonths = 156
noOfSites = 5
call allocaz(nmonths,noOfSites)
call readSiteData    !read site data
Call readMonthlyClimateData
end subroutine init_model

