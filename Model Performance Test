#We generate the MeanBeta from the previous data, the total money at the start is always
#3290000, we can buy the amount of Futures accordingly
MeanBeta <- vector()
for(i in 1:length(BetatoMarket))
{
  MeanBeta[i] <- mean(BetatoMarket[[i]])
}
CSIFutureR <- vector()
for(i in 1:6)
{
  CSIFutureR[i] <- - MeanBeta[i] * (CSI[CSI$Date==FirstDate[i+1],]$Close/CSI[CSI$Date==FirstDate[i],]$Close - 1)
}

MovingReturnTotalHedged <- vector()
MovingReturnTotalHedged[1] <- MovingReturn[1]
for(i in 1:6)
{
  MovingReturnTotalHedged[i+1] <- MovingReturnTotal[i+1] + CSIFutureR[i]
}
MovingReturnTotalHedged <- c(1,MovingReturnTotalHedged)
MovingReturnTotalHedged

MovingReturnTotalUnHedged <- vector()
MovingReturnTotalUnHedged[1] <- UnSelectedMovingReturn[1]
for(i in 1:6)
{
  MovingReturnTotalUnHedged[i+1] <- MovingReturnTotalUn[i+1] + CSIFutureR[i]
}
MovingReturnTotalUn <- c(1,MovingReturnTotalUn)
MovingReturnTotalUn
MovingReturnTotalUn<- MovingReturnTotalUn[1:8]

CSIR <- vector()
for(i in 1:7)
{
  CSIR[i] <- CSI[CSI$Date==FirstDate[i],]$Close/CSI[CSI$Date==FirstDate[1],]$Close
}

plot(MovingReturnTotalUn,ylim=c(0.8,1.3),col='red',type = 'l',main = 'Return After Hedge')
points(MovingReturnTotalHedged,col='blue', type = 'l')
points(CSIR,col='black', type = 'l')

#---------Function: find the data of certain company-------#
FindTheCompany <- function(InnerCode)
{
  for(i in 1:length(y))
  {
    if(InnerCode==y[[i]]$InnerCode[1])
      result <- y[[i]]
  }
  return(result)
}

PlotChart <- function(InnerCode)
{
  tt <- FindTheCompany(InnerCode)
  totalreturn <- vector()
  totalreturn[1] <- 1
  tt$NVReturn[is.na(tt$NVReturn)] <- 0
  for(i in 2:dim(tt)[1])
  {
    totalreturn[i] <- totalreturn[i-1] * (1+tt$NVReturn[i-1]/100)
  }
  CSIP <- vector()
  for(i in 1:dim(tt)[1])
  {
    date <- tt$Date[i]
    CSIP[i] <- CSI$Close[CSI$Date==date]
  }
  CSIR <- vector()
  for(i in 1:length(CSIP))
  {
    CSIR[i] <- CSIP[i]/CSIP[1]
  }
  m1 <- max(CSIR)
  m2 <- max(totalreturn)
  m3 <- min(CSIR)
  m4 <- min(totalreturn)
  if(any(InnerCode==SelectedCompany$InnerCode))
    ff <- 'Selected'
  else
    ff <- 'UnSelected'
  Alpha <- LMOut1(lm(totalreturn~CSIR))[1]
  tAlpha <- LMOut1(lm(totalreturn~CSIR))[2]
  b <- LMOut1(lm(totalreturn~CSIR))[3]
  r <- LMOut1(lm(totalreturn~CSIR))[4]
  plot(totalreturn,ylim = c(min(m3,m4),max(m1,m2)),xlab = InnerCode, type = 'l',col = 'blue', main = cbind(ff,Alpha,tAlpha,r,b))
  points(CSIR,type = 'l', col = 'red')
  lmr <- lm(totalreturn~CSIR)
  print(summary(lmr))
}

PlotAll <- function(flag)
{
  if(flag==1)
  {
    for(i in 1:dim(SelectedCompany)[1])
    {
      PlotChart(SelectedCompany[i,2])
    }
  }
  if(flag==0)
  {
    for(i in 20:30)
    {
      PlotChart(UnSelectedCompany[i,2])
    }
  }
}
