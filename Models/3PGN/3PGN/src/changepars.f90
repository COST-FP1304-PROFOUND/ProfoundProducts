Subroutine changepars(pa)
use initbc
use declarations
implicit none
real (kind=8) :: pa(51)
! wrapper program to initialise BASFOR for R
! David Cameron, CEH-Edinburgh (Sept 2009)
! dcam@ceh.ac.uk

!====================================================
! change parameters
!====================================================
call change_pars(pa) 

end subroutine changepars
