Module initbc
use declarations
implicit none  

Contains

Subroutine change_pars(pa)
implicit none
real (kind=8) :: pa(51)

StemPower	=	pa	(	1	)
aH	=	pa	(	2	)
bW	=	pa	(	3	)
klmax	=	pa	(	4	)
alpha	=	pa	(	5	)
Tmin	=	pa	(	6	)
krmax	=	pa	(	7	)
gammaF1	=	pa	(	8	)
fN0	=	pa	(	9	)
rg	=	pa	(	10	)
rho1	=	pa	(	11	)
gammaR	=	pa	(	12	)
Topt	=	pa	(	13	)
MaxCond	=	pa	(	14	)
StemConst	=	pa	(	15	)
pFS20	=	pa	(	16	)
pRn	=	pa	(	17	)
k	=	pa	(	18	)
fracBB1	=	pa	(	19	)
LAIgcx	=	pa	(	20	)
fullCanAge	=	pa	(	21	)
pRx	=	pa	(	22	)
CoeffCond	=	pa	(	23	)
pFS2	=	pa	(	24	)
hc	=	pa	(	25	)
kF	=	pa	(	26	)
SLA1	=	pa	(	27	)
tBB	=	pa	(	28	)
m0	=	pa	(	29	)
tSLA	=	pa	(	30	)
Tmax	=	pa	(	31	)
MaxIntcptn	=	pa	(	32	)
fracBB0	=	pa	(	33	)
SLA0	=	pa	(	34	)
BLcond	=	pa	(	35	)
nAge	=	pa	(	36	)
tgammaF	=	pa	(	37	)
MaxAge	=	pa	(	38	)
rAge	=	pa	(	39	)
gammaF0	=	pa	(	40	)
komax	=	pa	(	41	)
dmC	=	pa	(	42	)
Yl_C_i	=	pa	(	43	)
Yr_C_i	=	pa	(	44	)
O_C_i	=	pa	(	45	)
LAImaxIntcptn	=	pa	(	46	)
FR_site(1)	=	pa	(	47	)
FR_site(2)	=	pa	(	48	)
FR_site(3)	=	pa	(	49	)
FR_site(4)	=	pa	(	50	)
FR_site(5)	=	pa	(	51	)

END subroutine change_pars   

end module initbc
