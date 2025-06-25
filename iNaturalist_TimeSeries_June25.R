#
# Analysis of the time-series of iNaturalist derived indices
# Beware, not  "counts" but metric/effort indices
# Two metrics: (1) total counts, (2) number of different species
# The sampling unit is the combination of Year and Site.
# Years are 2008:2024, and sites are 689 1/2 degree of area hexagons
# partitioning Mexico. We focus on sampling units with at least one
# observation
#
#
library(terra)
library(ggplot2)
library(nlme)

# First get data

# Bombus
pn5=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Tables\\BombusHexsVPR.csv")

# Butterflies
pn5=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Tables\\MaripasHexsVPR.csv")

# Odonata
pn5=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Tables\\OdonataHexsVPR.csv")

# And Solanaceae
pn5=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Tables\\SolanaceaeHexsVPR.csv")

# next subsets as per Potential Vegetation

pn4=pn5 #All cases

# Just for the butterflies, for all of Mexico -----------------------
iiPi=which(pn5[,1]=="Pieridae")
pn4=pn5[iiPi,]

iiPa=which(pn5[,1]=="Papilionidae")
pn4=pn5[iiPa,]

iiNy=which(pn5[,1]=="Nymphalidae")
pn4=pn5[iiNy,]

iiLy=which(pn5[,1]=="Lycaenidae")
pn4=pn5[iiLy,]
# ----------------------------------------------------------------------

# Just for the Odonata, all of Mexico ----------------------------------
# drg and dam defined below
# Families Odonata
dam=c("Calopterygidae","Coenagrionidae","Polythoridae","Lestidae","Perilestidae","Platystictidae","Heteragrionidae","Thaumatoneuridae", "Amphipterygidae")
drg=c("Aeshnidae","Gomphidae","Cordulegastridae","Corduliidae","Libellulidae","Macromiidae")

iiA=which(pn5[,1] %in% drg)
pn4=pn5[iiA,] # The dragonflies, or Anisoptera

iiZ=which(pn5[,1] %in% dam)
pn4=pn5[iiZ,] # The damselflies, or Zygoptera


# Or subset by Potential Vegetation of Rzedowsky

ixs=which(pn5$VPR=="Mx")
pn4=pn5[ixs,] # Matorral xerofilo

ibce=which(pn5$VPR=="Bce")
pn4=pn5[ibce,] # Bosque de coniferas y encino

ipas=which(pn5$VPR=="P")
pn4=pn5[ipas,] # Grasslands

ibtc=which(pn5$VPR=="Btc") # Bosque tropical caducifolio
pn4=pn5[ibtc,]


# select the resolution (column in pn4), name = cl, cl=11 means all of Mexico
# 7 two degs, 8 one deg, 9 3/4 deg, 10 1/2 deg

cl=7
ans=2008:2024
sts=unique(pn4[,cl]) # labels of sites
rgs=unique(pn4[,3]) # labels of observers
pvs=unique(pn4$VPR) # labels of VPRs
sps=table(pn4$Name)
nombres=names(sps)
a=length(ans) # how many years
s=length(sts) # how many hexagons
r=length(rgs) # how many observers
v=length(pvs) # how many potential vegetation classes

# isbc=which(pn5$VPR=="Btc")
# isap=which(pn5$VPR=="Btp")
# pn4=pn5[isap,] # Bosque tropical perennifolio
# pn4=pn5[isbc,] # Bosque tropical caducifolio


# quality of Observers
qual=matrix(0,nrow=nrow(pn4),ncol=6)
for (i in 1:r)
{
  ii=which(pn4[,3]==rgs[i]); # Rows of observations observer 'i' made
  lg=length(ii); # How many observations
  qual[ii,1]=lg; # This is how many observations assigned in each case of observer 'i'
  qual[ii,2]=length(unique(pn4[ii,2])) # how many different species the observer has recorded
  qual[ii,3]=length(unique(pn4[ii,4])) # How long has an observer being active
  qual[ii,4]=min(unique(pn4[ii,4]))# When did an observer start
  if(length(ii) > 2) {qual[ii,5]=1} else {qual[ii,5]=0} #"Reliable" observer, with more than 2 observations
  ti=table(pn4[ii,2]); #table of number of times a species was observed by the observer
  ini=which(nombres %in% names(ti)); # select what species were observed by the 'ii' observer
  qual[ii,6]=mean(sps[ini])/max(sps); # average times species ahve been observed, as a fraction
}
colnames(qual)=c("Observations","Species","Period","FirstYear","MoreThan2","Median 'rarity")
#

# Metric (sps, or records) and effort
indi=matrix(0,ncol=a,nrow=s)# Matrix of species/unit_effort per event (year and site)
sp=matrix(0,ncol=a,nrow=s) # Matrix of different species in events (year and site)
rc=matrix(0,ncol=a,nrow=s) # Matrix of number of observations in events (year and site)
ob=matrix(0,ncol=a,nrow=s) # Matrix of more than twice observers

for(j in 1:a)for(i in 1:s)
{
  iiii=which(pn4[,4]==ans[j] & pn4[,cl]==sts[i]); # Indices of rows in a year and site
  # first, the metrics of results. Species or observations
  spp=length(unique(pn4[iiii,2])); # How many different SPECIES in a year and site
  rcc=length(iiii);# How many RECORDS in a year and site
  sp[i,j]=spp;
  rc[i,j]=rcc;
    # Then the effort: number of different observers
  # if(length(iiii) > 0) {o=length(unique(pn4[iiii,3]))} else{o=0} 
  # Matrix 'indi' will have (for sites X year with observations)
  # the ratio of number of unique species/observers*mean_quality,
  # With the mean quality being the mean length of years observing
  # if (o==0) {indi[i,j]=0} else {indi[i,j]=rcc/o}; # Check e or er
  # indi[i,j]=spp/momB[j] # momB[j] is the mean number of months with observations in year 'j'
  o=sum(qual[iiii,5]); # o is now the number of observers with > 2 observations
  if (o==0) {indi[i,j]=0} else {indi[i,j]=spp/o};
  ob[i,j]=o; # Matrix of twice-observers per
  # Check e or er
  }

# Now the elements of the time-series
# I will be taking means over observers that are non-zero

indipam=ifelse(indi>0,1,0) # a pam...
nnz=colSums(indipam) # for each one of the j=1,2,... a years, the number of non-zeroes
inz=colSums(indi) # for each one of the j=1,2,... a years, the sum of indices
media=inz/nnz # mean with respect to non-zeroes
errs=matrix(0,ncol=1,nrow=a)
for (i in 1:a)
{
  iinz=which(indi[,i]>0); # indices of sites with e/o not zero, for the i-th year 
  errs[i,1]=sd(indi[iinz,i])/sqrt(length(iinz)) # The standard error for the i-th year
}

# The years, in xx, the mean value (over hexagons with non-zero observers)
# of the index
xx=ans
yy=media
es=errs[,1] # For graphs with standard error band
dtf=na.omit(cbind(xx,yy,es))
colnames(dtf)=c("Year","Indice","ErrorSt")
dtf=as.data.frame(dtf)

# -----------------------------------------------------------
# Cute plot of species/unit_effort
x11()
ggplot(dtf, aes(x=Year, y=Indice))+ylab("Index/effort")+xlab("Year")+
  geom_ribbon(aes(ymin=Indice-ErrorSt, ymax=Indice+ErrorSt),fill="grey70")+
  geom_line(aes(y=Indice))+
  geom_point(aes(x=Year,y=Indice),col="black",size=2)+
  ggtitle("Solanaceae, obs, 1/2 deg")+theme_bw()



# A generalized linear regression, that allows for correlated errors and
# heterosdacity, using the function 'gls' of the package 'nlme'
# first, fit a model with no autocorrelation
#then one with an ARIMA of lag=1
dtf=as.matrix(dtf)
mod0=gls(dtf[,2]~dtf[,1],na.action=na.omit)
mod1=update(mod0,correlation = corARMA(p=1),na.action=na.omit)
mod2=update(mod1, weights=varPower(),na.action=na.omit)
# Then, ANOVA the two
nv=anova(mod0,mod2)
pe=nv$`p-value`[2]
if(pe<.01){cual=mod2} else {cual=mod0}
options(digits=3)
summary(cual)$tTable[2,1]
summary(cual)$tTable[2,4]
nrow(pn4)
if(pe<.01){print("ARIMA")} else{print("OLS")}






# PROXIES FAO data on pesticeds


ppuc2=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Proxies\\PesticidesPerUnitCropland.csv")
ppuc=as.data.frame(ppuc2)
ican=which(ppuc$Country=="Canada")
iusa=which(ppuc$Country=="United_States_of_America")
imex=which(ppuc$Country=="Mexico")

x11()
plot(ppuc[iusa,4],ppuc[iusa,6], type="b",col="blue",ylim=c(0,3),pch=19,xlab="Years",ylab="Petsicides kg/ha")
lines(ppuc[ican,4],ppuc[ican,6], type="b",col="red",pch=19)
lines(ppuc[imex,4],ppuc[imex,6], type="b",col="green",pch=19)
legend(2010,0.5,legend=c("USA","CND","MEX"),fill=c("blue","red","green"))
grid()


# Since I get different results from analyzing species or observations
# how do the two look?

spob=matrix(0,ncol=2,nrow=s) # a matrix that will contain species and observations per site
colnames(spob)=c("observations","species")
# The which below defines the location
for(i in 1:s){ihex=which(pn4[,cl]==sts[i]);spob[i,1]=length(ihex);spob[i,2]=length(unique(pn4[ihex,2]))}
x11()
plot(spob) # for each one of the sites, obs vs. species
nz=which(spob[,2]>0)
mod=lm(log10(spob[nz,2])~log10(spob[nz,1]))
x1=seq(0,12000,by=50)
y1=10^mod$coefficients[1]*x1^mod$coefficients[2]
lines(x1,y1)


write.csv(resumen,"C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Table3.csv")

# Now a regression analisis of indices vs. predictors
todo=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\SintesisDataPredictorsHalfDeg.csv")
todo2=todo[14:24,]

# columns are as follows:
# [1] "Year"          "FrtLssS"       "CerProdS"      "TotAgrqS"      "AgrqPerHa"    
# [6] "BombusS"       "NymphalidaeS"  "PapilionidaeS" "PieridaeS"     "AnysopteraS"  
# [11] "ZygopteraS"    "BombusO"       "NymphalidaeO"  "PapilionidaeO" "PieridaeO"    
# [16] "AnysopteraO"   "ZygopteraO"   
tx=11
y1=todo2[,tx]
x1=todo2[,1] # Time in years
x2=todo2[,2] # Loss of forest Standardized (WRI)
x3=todo2[,3] # Cereal Production Standardized (FAO)
x4=todo2[,4] # Agrochem prod SEMARNAT Standardized
x5=todo2[,5] #agrChem per ha, Standardized (FAO)

x11()
plot(x1,y1,pch=19,main=tit[tx-4],xlab="Year",ylab="Records/effort")
plot(x1,y1,pch=19,main=tit[tx-4],xlab="Year",ylab="Species/effort")
lines(2012:2024,y2)
grid()


# Using generalized least squares
mod0=gls(y1~+x1+x2+x5,na.action=na.omit)
mod1=update(mod0,correlation = corARMA(p=1))
mod2=update(mod1, weights=varPower())

nv=anova(mod0,mod2)


# Plot of Species and Observers (more than 2 observations)

sob=read.csv("C:\\Users\\jsoberon\\OneDrive - University of Kansas\\Shared with Everyone\\StatusButterfliesNorthAmerica\\Data\\Tables\\ObserversSpeciesObservations.csv")


x11()
plot(sob[,1],log10(sob[,4]),type="b",pch=0,ylab="log10(Species)",xlab="", ylim=c(-1.5,1.75))   # Nymphs
lines(sob[,1],log10(sob[,5]),type="b",pch=1)  # Paps
lines(sob[,1],log10(sob[,6]),type="b",pch=2)  # Piers
lines(sob[,1],log10(sob[,8]),type="b",pch=15)  # Dragons
lines(sob[,1],log10(sob[,9]),type="b",pch=16)  # Damsels
lines(sob[,1],log10(sob[,10]),type="b",pch=12) # Bumbles
lines(sob[,1],log10(sob[,11]),type="b",pch=8) # Solanacea
grid()
legend("topleft",legend=c("Nymphalids","Papilionids","Pierids","Drgs","Dmsl","Bmblbs","Solanacea"), pch=c(0,1,2,15,16,12,8))


x11()
plot(sob[,1],log10(sob[,14]),type="b",pch=0,ylab="log10(Observers)",xlab="",ylim=c(-1.5,1.75))   # Nymphs
lines(sob[,1],log10(sob[,15]),type="b",pch=1)  # Paps
lines(sob[,1],log10(sob[,16]),type="b",pch=2)  # Piers
lines(sob[,1],log10(sob[,18]),type="b",pch=15)  # Dragons
lines(sob[,1],log10(sob[,19]),type="b",pch=16)  # Damsels
lines(sob[,1],log10(sob[,20]),type="b",pch=12) # Bumbles
lines(sob[,1],log10(sob[,21]),type="b",pch=8) # Solanacea
grid()
legend("topleft",legend=c("Nymphalids","Papilionids","Pierids","Drgs","Dmsl","Bmblbs","Solanacea"), pch=c(0,1,2,15,16,12,8))

# ---------------- ARITMETIC SCALE ---------------------
x11()
plot(sob[,1],sob[,4],type="b",pch=0,ylab="Mean number of species",xlab="", ylim=c(0,15))   # Nymphs
lines(sob[,1],sob[,5],type="b",pch=1)  # Paps
lines(sob[,1],sob[,6],type="b",pch=2)  # Piers
lines(sob[,1],sob[,8],type="b",pch=15)  # Dragons
lines(sob[,1],sob[,9],type="b",pch=16)  # Damsels
lines(sob[,1],sob[,10],type="b",pch=12) # Bumbles
lines(sob[,1],sob[,11],type="b",pch=8) # Solanacea
grid()
legend("topleft",legend=c("Nymphalids","Papilionids","Pierids","Drgs","Dmsl","Bmblbs","Solanacea"), pch=c(0,1,2,15,16,12,8))


x11()
plot(sob[,1],sob[,14],type="b",pch=0,ylab="Mean number of observers)",xlab="",ylim=c(0,100))   # Nymphs
lines(sob[,1],sob[,15],type="b",pch=1)  # Paps
lines(sob[,1],sob[,16],type="b",pch=2)  # Piers
lines(sob[,1],sob[,18],type="b",pch=15)  # Dragons
lines(sob[,1],sob[,19],type="b",pch=16)  # Damsels
lines(sob[,1],sob[,20],type="b",pch=12) # Bumbles
lines(sob[,1],sob[,21],type="b",pch=8) # Solanacea
grid()
legend("topleft",legend=c("Nymphalids","Papilionids","Pierids","Drgs","Dmsl","Bmblbs","Solanacea"), pch=c(0,1,2,15,16,12,8))




