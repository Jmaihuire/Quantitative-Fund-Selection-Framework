#-------------------read in the data and do basic analysis-------------------#
FundType <- read.csv("FundTypeNew.csv")
MFReturn <- read.csv("NVReturnNew.csv")
colnames(MFReturn)[4] <- "NVReturn"
colnames(FundType)[2] <- "OpenOrNot"
#here we assume 2 3 4 are open funds, 1101 invest in stock, 1105 invest in bond
colnames(FundType)[4] <- "InvestmentType"
FundPrice <- read.csv("mfprice.csv")
colnames(FundPrice)[1] <- "InnerCode"
colnames(FundPrice)[2] <- "Date"
colnames(FundPrice)[3] <- "FundPrice"
FundType <- FundType[,1:4]
CSI <- read.csv("CSINew.csv")
head(CSI) #2688 from 38275 end 42307
Value <- read.csv("Fvalue2.csv")
Size <- read.csv("FSize2.csv")
#So, our date from 38509 to 41275
#diff <- vector()
Vol <- read.csv("FSigma2.csv")
Lev <- read.csv("FLeverage2.csv")
Mom <- read.csv("FMomentum2.csv")
diff2 <- as.numeric(as.Date(MFReturn$TradingDay)-as.Date("1900-01-01",format = "%Y-%m-%d"))
length(diff2)
MFReturn$Date <- as.vector(diff2) #here the date is comparable with other dates
MFReturn <- MFReturn[,2:5]
EquityFund <- FundType[as.logical(3==(as.integer(FundType$InvestmentType==1101)+as.integer(FundType$OpenOrNot>1)+as.integer(FundType$OpenOrNot<5))),]$InnerCode


CSI[,4] <- NA
colnames(CSI)[4] <- 'CSIReturn'
for(i in 1:dim(CSI)[1])
{
  CSI$CSIReturn[i] <- (CSI$Close[i]/CSI$Open[i]-1)*100
}
CSI$Date <- as.numeric(CSI$Date)
CSI$Open <- NULL


Value <- cbind(CSI$Date[1:2600],Value$x)
Value <- as.data.frame(Value)
Value$ValueReturn <- NA
colnames(Value)[2] <- 'Value'
colnames(Value)[1] <- 'Date'
for(i in 2:dim(Value)[1])
{
  Value$ValueReturn[i] <- (Value$Value[i]/Value$Value[i-1]-1)*100
}

Size <- cbind(CSI$Date[1:dim(Size)[1]],Size$x)
Size <- as.data.frame(Size)
Size$SizeReturn <- NA
colnames(Size)[2] <- 'Size'
colnames(Size)[1] <- 'Date'
for(i in 2:dim(Size)[1])
{
  Size$SizeReturn[i] <- (Size$Size[i]/Size$Size[i-1]-1)*100
}

Vol <- cbind(CSI$Date[1:dim(Vol)[1]],Vol$x)
Vol <- as.data.frame(Vol)
Vol$VolReturn <- NA
colnames(Vol)[2] <- 'Vol'
colnames(Vol)[1] <- 'Date'
for(i in 2:dim(Vol)[1])
{
  Vol$VolReturn[i] <- (Vol$Vol[i]/Vol$Vol[i-1]-1)*100
}

if(1==0)
{
Growth <- cbind(CSI$Date[1:dim(Growth)[1]],Growth$x)
Growth <- as.data.frame(Growth)
Growth$GrowthReturn <- NA
colnames(Growth)[2] <- 'Growth'
colnames(Growth)[1] <- 'Date'
for(i in 2:dim(Growth)[1])
{
  Growth$GrowthReturn[i] <- (Growth$Growth[i]/Growth$Growth[i-1]-1)*100
}
}

total <- merge(MFReturn,CSI)
total <- merge(total,Value)
total <- merge(total,Size)
total <- merge(total,Vol)
mfname <- unique(total$InnerCode)
number <- length(mfname)
y <- list() #store data into a list y
for(i in 1:number) 
{
  temp <- total[total$InnerCode==mfname[i],]
  y[[i]] <- temp
}
yfund <- list()
count <- 1
for(i in 1:number)
{
  if(any(y[[i]]$InnerCode[1]==EquityFund))
  {
    yfund[[count]] <- y[[i]]
    count <- count + 1
  }
}
y <- yfund
NumberofTraining <- vector()
NameofNumber <- vector()
for(i in 1:length(y))
{
  NumberofTraining[i] <- dim(y[[i]])[1]
  NameofNumber[i] <- y[[i]]$InnerCode[1]
}
NameandNumber <- cbind(NumberofTraining,NameofNumber)
NameandNumber <- as.data.frame(NameandNumber)
count <- 1
yWithEnoughNumber <- list()
yWithNumber <- vector()
for(i in 1:length(y))
{
  if(NameandNumber[i,1] > 50)
  {
    yWithNumber[count] <- NameandNumber[i,2]
    yWithEnoughNumber[[count]] <- y[[i]]
    count <- count + 1
  }
} #length(yWithEnoughNumber)=490
y <- yWithEnoughNumber
#-----define LMOut function to output the related values and do basic analysis------#
LMOut <- function(lm)
{
  out <- c(summary(lm)$coefficient[1,1],
           summary(lm)$coefficient[2,1],
           summary(lm)$coefficient[3,1],
           summary(lm)$coefficient[4,1],
           summary(lm)$coefficients[1,3],
           summary(lm)$coefficients[2,3],
           summary(lm)$coefficients[3,3],
           summary(lm)$coefficients[4,3],
           summary(lm)$r.squared
  )
  names(out) <- c("Alpha","BetatoCSI","BetatoValue","BetatoSize","tSofAlpha","tSofCSI","tSofValue","tSofSize","r.squared")
  return(out)
}
LMOut1 <- function(lm)
{
  out <- c(summary(lm)$coefficient[1,1],
           summary(lm)$coefficients[1,3],
           summary(lm)$coefficients[2,1],
           summary(lm)$r.squared
  )
  names(out) <- c("Alpha","tSofAlpha","Beta","r.squared")
  return(out)
}
#-------------------------Assume we start from 2012.01.01----------------------------#
#First step, merge the neccesary data with the data set
#Second step, define a function, given the innercode and the start date, select the previous all dates' data
#do regression with the selected data and sort it using the result, generate the return in the next 12 weeks and after that, use the new date to do the previous steps.
MySplitFromStart <- function(Data,Date,Hold)
{
  result <- list()
  UpperDate <- Date
  RowNow <- which(Data$Date>Date)[1]
  RowEnd <- which(Data$Date>(Date+Hold))[1]
  if(is.na(RowNow)||is.na(RowEnd)||RowNow==1)
  {
    result[[1]] <- NULL
    result[[2]] <- NULL
  }
  else
  {
    ytraintemp <- Data[1:RowNow,] #just change here
    ytesttemp <- Data[RowNow:RowEnd,]
    result[[1]] <- ytraintemp
    result[[2]] <- ytesttemp
  }
  return(result)
}
#------------------Start Training and Testing here----------------------#
#Date from 40666 to 40882
#Step 1, generate the first day in every 3 months
#Get the correlation:
library(dplyr)
#---------------get correlation----------------#

cor(y[[1]][complete.cases(y[[1]]),4:10])
Hold <- 60
FirstDate <- vector()
FundToBuy <- list()
MoneyTotal <- vector()
MoneyStart <- vector()
BetatoMarket <- list()
for(i in 1:7)
{
  FirstDate[i] <- y[[100]]$Date[30+(Hold*5/7)*(i-1)]
}
count = 0
MovingReturn <- vector()
UnSelectedMovingReturn <- vector()
NumberofDatainTraing <- list()
NumberofDatainTesting <- list()
DD <- vector()
Remain <- vector()
for(i in 1:7)
{
  count = count + 1
  NumberofDatainTraing[[count]] <- vector()
  NumberofDatainTesting[[count]] <- vector()
  Date <- FirstDate[i]
  ytrain <- list()
  ytest <- list()
  for(i in 1:length(y))
  {
    UpperDate <- Date
    LowerDate <- Date-3*365
    RowLower <- which(y[[i]]$Date>LowerDate)[1]
    RowNow <- which(y[[i]]$Date>Date)[1]
    if(is.na(RowNow))
      next
    else
    {
      temp <- MySplitFromStart(y[[i]],Date,Hold)
      if(length(temp)==0)
      {
        ytrain[[i]] <- NA
        ytest[[i]] <- NA
      }
      else
      {
        ytrain[[i]] <- temp[[1]]
        ytest[[i]] <- temp[[2]]
      }
    }
  }
  
  modtrain <- list()
  yUsedForTraining <- vector()
  m <- 0
  Deleted <- 0
  for(i in 1:length(ytrain))
  {
    if(!is.na(ytrain[[i]])&&!is.null(ytrain[[i]])&&dim(ytrain[[i]])[1]>1)
    {
      if(dim(ytrain[[i]])[1]<30)
      {
        Deleted <- Deleted + 1
      }
      else
      {
        m <- m +1
        temp <- ytrain[[i]]$InnerCode[1]
        tt <- FindTheCompany(temp)
        totalreturn <- vector()
        totalreturn[1] <- 1
        tt$NVReturn[is.na(tt$NVReturn)] <- 0
        for(j in 2:dim(tt)[1])
        {
          totalreturn[j] <- totalreturn[j-1] * (1+tt$NVReturn[j-1]/100)
        }
        CSIP <- vector()
        for(j in 1:dim(tt)[1])
        {
          date <- tt$Date[j]
          CSIP[j] <- CSI$Close[CSI$Date==date]
        }
        CSIR <- vector()
        for(j in 1:length(CSIP))
        {
          CSIR[j] <- CSIP[j]/CSIP[1]
        }
        modtrain[[paste("run",temp,sep="")]] <- lm(ytrain[[i]]$NVReturn ~ ytrain[[i]]$CSIReturn+ytrain[[i]]$ValueReturn+ytrain[[i]]$SizeReturn+ytrain[[i]]$VolReturn)
        NumberofDatainTraing[[count]][i] <- dim(ytrain[[i]])[1]
        yUsedForTraining[m] <- temp
      }
    }
  }
  DD[count] <- Deleted
  Remain[count] <- length(yUsedForTraining)
  CSIBeta <- vector()
  ValueBeta <- vector()
  SizeBeta <- vector()
  tCSIBeta <- vector()
  tValueBeta <- vector()
  tSizeBeta <- vector()
  Alpha <- vector()
  tAlpha <- vector()
  rsquare <- vector()
  yUsedForSelect <- vector()
  n <- 0
  for(i in 1:length(yUsedForTraining))
  {
    if(length(summary(modtrain[[paste("run",yUsedForTraining[i],sep="")]]))>3){
      if(dim(summary(modtrain[[paste("run",yUsedForTraining[i],sep="")]])$coefficients)[1] >= 2)
      {
        n <- n + 1
        yUsedForSelect[n] <-yUsedForTraining[i]
        Alpha[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[1]
        tAlpha[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[2]
        CSIBeta[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[3]
        rsquare[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[4]
        
      }
    }
  }
  
  Rank <- vector()
  RankCompany <- vector()
  mmm <- 0
  for(i in 1:length(yUsedForSelect))
  {
    mmm <- mmm + 1
    Rank[mmm] <- 0.5*Alpha[i]
    RankCompany[mmm] <- yUsedForSelect[i]
  }
  MergedRank <- cbind(Rank,RankCompany)
  a <- MergedRank[sort.list(MergedRank[,1]), ] #Rank the number we got here
  a <- arrange(as.data.frame(a), -row_number()) #reverse the data to make it from big to small
  a <- a[complete.cases(a), ] #only leave the complete cases
  number <- dim(a)[1]
  split <- floor(number/10)
  SelectedCompany <- a[1:split,] #Select the top 10% as selected company
  UnSelectedCompany <- a[-(1:split),]
  colnames(SelectedCompany)[2] <- "InnerCode"
  
  BetaandName <- cbind(CSIBeta,yUsedForSelect)
  SBeta <- vector()
  for(i in 1:dim(SelectedCompany)[1])
  {
    SBeta[i] <- BetaandName[BetaandName[,2]==SelectedCompany[i,2],1]
  }
  BetatoMarket[[count]] <- SBeta
  #create a list for returns of the selected companies
  #here return rate is selected in a list and the order is in the SelectedCompanySplitDate
  
  
  FundToBuy[[count]] <- SelectedCompany$InnerCode
  #create a list for returns of the selected companies
  #here return rate is selected in a list and the order is in the SelectedCompanySplitDate
  ReturnRateSelected <- list()
  for(i in 1:dim(SelectedCompany)[1])
  {
    ReturnRateSelected[[i]] <- vector()
  }
  for(i in 1:dim(SelectedCompany)[1])
  {
    InnerCode <- SelectedCompany[i,2]
    for(j in 1:length(y))
    {
      if(!is.na(ytest[[j]])&&!is.null(ytest[[j]])&&!is.na(ytest[[j]]$InnerCode[1])&&InnerCode==ytest[[j]]$InnerCode[1])
      {
        ReturnRateSelected[[i]] <- 1+ytest[[j]]$NVReturn/100
      }
    } #If the InnerCode is in the Selected Company, then got the multiply of all the return and save it in ith number
  }
  #get the length of all these return vectors
  TimeHorizon <- vector()
  for(i in 1:dim(SelectedCompany)[1])
  {
    TimeHorizon[i] <- length(ReturnRateSelected[[i]])
  }
  #The Length of the return vectors are not same, so we use risk free rate at the end of these vectors
  HoldPeriod <- max(TimeHorizon)
  for(i in 1:dim(SelectedCompany)[1])
  {
    if(length(ReturnRateSelected[[i]]) < HoldPeriod)
    {
      for(j in length(ReturnRateSelected[[i]]):HoldPeriod)
        ReturnRateSelected[[i]][j] <- 1.000755
    }
    for(j in 1:HoldPeriod)
      if(is.na(ReturnRateSelected[[i]][j]))
        ReturnRateSelected[[i]][j] <- 1.000755
  }
  #plot the chart, make the no data weeks as risk free rate
  everyweekreturn <- vector()
  totalmoney <- vector()
  companymoney <- vector()
  for(i in 1:dim(SelectedCompany)[1])
  {
    companymoney[i] <- 3290000/dim(SelectedCompany)[1]
  }
  for(i in 1:HoldPeriod)
  {
    totalmoney[i] <- sum(companymoney)
    for(j in 1:dim(SelectedCompany)[1])
    {
      companymoney[j] <- companymoney[j] * ReturnRateSelected[[j]][i]
    }
  }
  MoneyStart[count] <- totalmoney[1]
  MoneyTotal[count] <- totalmoney[HoldPeriod]
  MovingReturn[count] <- totalmoney[HoldPeriod]/totalmoney[1]
  #---------------------for the unselected companies-----------------#
  ReturnRateUnSelected <- list()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    ReturnRateUnSelected[[i]] <- vector()
  }
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    InnerCode <- UnSelectedCompany[i,2]
    for(j in 1:length(y))
    {
      if(!is.na(ytest[[j]])&&!is.null(ytest[[j]])&&InnerCode==ytest[[j]]$InnerCode[1])
      {
        ReturnRateUnSelected[[i]] <- 1+ytest[[j]]$NVReturn/100
      }
    } #If the InnerCode is in the Selected Company, then got the multiply of all the return and save it in ith number
  }
  #get the length of all these return vectors
  TimeHorizon <- vector()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    TimeHorizon[i] <- length(ReturnRateUnSelected[[i]])
  }
  #The Length of the return vectors are not same, so we use risk free rate at the end of these vectors
  HoldPeriod <- max(TimeHorizon)
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    if(length(ReturnRateUnSelected[[i]]) < HoldPeriod)
    {
      for(j in length(ReturnRateUnSelected[[i]]):HoldPeriod)
        ReturnRateUnSelected[[i]][j] <- 1.000755
    }
    for(j in 1:HoldPeriod)
      if(is.na(ReturnRateUnSelected[[i]][j]))
        ReturnRateUnSelected[[i]][j] <- 1.000755
  }
  #plot the chart, make the no data weeks as risk free rate
  everyweekreturn <- vector()
  totalmoney <- vector()
  companymoney <- vector()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    companymoney[i] <- 3290000/dim(UnSelectedCompany)[1]
  }
  for(i in 1:HoldPeriod)
  {
    totalmoney[i] <- sum(companymoney)
    for(j in 1:dim(UnSelectedCompany)[1])
    {
      companymoney[j] <- companymoney[j] * ReturnRateUnSelected[[j]][i]
    }
  }
  UnSelectedMovingReturn[count] <- totalmoney[HoldPeriod]/totalmoney[1]
}


#---------Generate the trading cost------------#
MoneyTotal <- c(1,MoneyTotal)
MoneyTotal[1] <- 3290000
nn <- 1
TradingCost <- vector()
TradingCost[1] <- 3290000*0.006
TradingCost[8] <- MoneyTotal[8]*0.005
for(i in 1:6)
{
  nn <- nn+1
  PreviousMoney <- rep(MoneyTotal[i]/length(FundToBuy[[i]]),length(FundToBuy[[i]]))
  PreviousMoneyAndName <- cbind(PreviousMoney,FundToBuy[[i]])
  NowMoney <- rep(MoneyTotal[i+1]/length(FundToBuy[[i+1]]),length(FundToBuy[[i+1]]))
  NowMoneyAndName <- cbind(NowMoney,FundToBuy[[i+1]])
  colnames(NowMoneyAndName)[2] <- "InnerCode"
  colnames(PreviousMoneyAndName)[2] <- "InnerCode"
  Total <- merge(x = NowMoneyAndName, y = PreviousMoneyAndName, by = "InnerCode", all.x = TRUE)
  Total[is.na(Total$PreviousMoney),]$PreviousMoney = 0
  change <- vector()
  change <- Total$NowMoney - Total$PreviousMoney
  num <- 0
  for(i in 1:length(change))
  {
    if(change[i]>0)
      num <- num + change[i]*0.006 #buy in
    else
      num <- num + abs(change[i])*0.005 #sell out
  }
  TradingCost[nn] <- num
}

#------------Get the result after hedge-----------#
Hold <- 60
FirstDate <- vector()
FundToBuy <- list()
MoneyTotal <- vector()
for(i in 1:7)
{
  FirstDate[i] <- y[[100]]$Date[30+(Hold*5/7)*(i-1)]
}
count = 0
MovingReturn <- vector()
UnSelectedMovingReturn <- vector()
NumberofDatainTraing <- list()
NumberofDatainTesting <- list()
DD <- vector()
Remain <- vector()
for(i in 1:7)
{
  count = count + 1
  NumberofDatainTraing[[count]] <- vector()
  NumberofDatainTesting[[count]] <- vector()
  Date <- FirstDate[i]
  ytrain <- list()
  ytest <- list()
  for(i in 1:length(y))
  {
    UpperDate <- Date
    LowerDate <- Date-3*365
    RowLower <- which(y[[i]]$Date>LowerDate)[1]
    RowNow <- which(y[[i]]$Date>Date)[1]
    if(is.na(RowNow))
      next
    else
    {
      temp <- MySplitFromStart(y[[i]],Date,Hold)
      if(length(temp)==0)
      {
        ytrain[[i]] <- NA
        ytest[[i]] <- NA
      }
      else
      {
        ytrain[[i]] <- temp[[1]]
        ytest[[i]] <- temp[[2]]
      }
    }
  }
  modtrain <- list()
  yUsedForTraining <- vector()
  m <- 0
  Deleted <- 0
  for(i in 1:length(ytrain))
  {
    if(!is.na(ytrain[[i]])&&!is.null(ytrain[[i]])&&dim(ytrain[[i]])[1]>1)
    {
      if(dim(ytrain[[i]])[1]<30)
      {
        Deleted <- Deleted + 1
      }
      else
      {
        m <- m +1
        temp <- ytrain[[i]]$InnerCode[1]
        tt <- FindTheCompany(temp)
        totalreturn <- vector()
        totalreturn[1] <- 1
        tt$NVReturn[is.na(tt$NVReturn)] <- 0
        for(j in 2:dim(tt)[1])
        {
          totalreturn[j] <- totalreturn[j-1] * (1+tt$NVReturn[j-1]/100)
        }
        CSIP <- vector()
        for(j in 1:dim(tt)[1])
        {
          date <- tt$Date[j]
          CSIP[j] <- CSI$Close[CSI$Date==date]
        }
        CSIR <- vector()
        for(j in 1:length(CSIP))
        {
          CSIR[j] <- CSIP[j]/CSIP[1]
        }
        modtrain[[paste("run",temp,sep="")]] <- lm(ytrain[[i]]$NVReturn ~ ytrain[[i]]$CSIReturn+ytrain[[i]]$ValueReturn+ytrain[[i]]$SizeReturn+ytrain[[i]]$VolReturn)
        NumberofDatainTraing[[count]][i] <- dim(ytrain[[i]])[1]
        yUsedForTraining[m] <- temp
      }
    }
  }
  DD[count] <- Deleted
  Remain[count] <- length(yUsedForTraining)
  CSIBeta <- vector()
  ValueBeta <- vector()
  SizeBeta <- vector()
  tCSIBeta <- vector()
  tValueBeta <- vector()
  tSizeBeta <- vector()
  Alpha <- vector()
  tAlpha <- vector()
  rsquare <- vector()
  yUsedForSelect <- vector()
  n <- 0
  for(i in 1:length(yUsedForTraining))
  {
    if(length(summary(modtrain[[paste("run",yUsedForTraining[i],sep="")]]))>3){
      if(dim(summary(modtrain[[paste("run",yUsedForTraining[i],sep="")]])$coefficients)[1] >= 2)
      {
        n <- n + 1
        yUsedForSelect[n] <-yUsedForTraining[i]
        Alpha[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[1]
        tAlpha[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[2]
        rsquare[n] <- LMOut1(modtrain[[paste("run",yUsedForTraining[i],sep="")]])[3]
      }
    }
  }
  Rank <- vector()
  RankCompany <- vector()
  mmm <- 0
  for(i in 1:length(yUsedForSelect))
  {
    
    mmm <- mmm + 1
    Rank[mmm] <- 0.5*Alpha[i]
    RankCompany[mmm] <- yUsedForSelect[i]
  }
  MergedRank <- cbind(Rank,RankCompany)
  a <- MergedRank[sort.list(MergedRank[,1]), ] #Rank the number we got here
  a <- arrange(as.data.frame(a), -row_number()) #reverse the data to make it from big to small
  a <- a[complete.cases(a), ] #only leave the complete cases
  number <- dim(a)[1]
  split <- floor(number/10)
  SelectedCompany <- a[1:split,] #Select the top 10% as selected company
  UnSelectedCompany <- a[-(1:split),]
  colnames(SelectedCompany)[2] <- "InnerCode"
  #create a list for returns of the selected companies
  #here return rate is selected in a list and the order is in the SelectedCompanySplitDate
  
  
  FundToBuy[[count]] <- SelectedCompany$InnerCode
  #create a list for returns of the selected companies
  #here return rate is selected in a list and the order is in the SelectedCompanySplitDate
  ReturnRateSelected <- list()
  for(i in 1:dim(SelectedCompany)[1])
  {
    ReturnRateSelected[[i]] <- vector()
  }
  for(i in 1:dim(SelectedCompany)[1])
  {
    InnerCode <- SelectedCompany[i,2]
    for(j in 1:length(y))
    {
      if(!is.na(ytest[[j]])&&!is.null(ytest[[j]])&&!is.na(ytest[[j]]$InnerCode[1])&&InnerCode==ytest[[j]]$InnerCode[1])
      {
        ReturnRateSelected[[i]] <- 1+ytest[[j]]$NVReturn/100
      }
    } #If the InnerCode is in the Selected Company, then got the multiply of all the return and save it in ith number
  }
  #get the length of all these return vectors
  TimeHorizon <- vector()
  for(i in 1:dim(SelectedCompany)[1])
  {
    TimeHorizon[i] <- length(ReturnRateSelected[[i]])
  }
  #The Length of the return vectors are not same, so we use risk free rate at the end of these vectors
  HoldPeriod <- max(TimeHorizon)
  for(i in 1:dim(SelectedCompany)[1])
  {
    if(length(ReturnRateSelected[[i]]) < HoldPeriod)
    {
      for(j in length(ReturnRateSelected[[i]]):HoldPeriod)
        ReturnRateSelected[[i]][j] <- 1.000755
    }
    for(j in 1:HoldPeriod)
      if(is.na(ReturnRateSelected[[i]][j]))
        ReturnRateSelected[[i]][j] <- 1.000755
  }
  #plot the chart, make the no data weeks as risk free rate
  everyweekreturn <- vector()
  totalmoney <- vector()
  companymoney <- vector()
  for(i in 1:dim(SelectedCompany)[1])
  {
    companymoney[i] <- 3290000/dim(SelectedCompany)[1]
  }
  for(i in 1:HoldPeriod)
  {
    totalmoney[i] <- sum(companymoney)
    for(j in 1:dim(SelectedCompany)[1])
    {
      companymoney[j] <- companymoney[j] * ReturnRateSelected[[j]][i]
    }
  }
  MoneyTotal[count] <- totalmoney[HoldPeriod]
  MovingReturn[count] <- (totalmoney[HoldPeriod]-TradingCost[count+1])/totalmoney[1]
  #---------------------for the unselected companies-----------------#
  ReturnRateUnSelected <- list()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    ReturnRateUnSelected[[i]] <- vector()
  }
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    InnerCode <- UnSelectedCompany[i,2]
    for(j in 1:length(y))
    {
      if(!is.na(ytest[[j]])&&!is.null(ytest[[j]])&&InnerCode==ytest[[j]]$InnerCode[1])
      {
        ReturnRateUnSelected[[i]] <- 1+ytest[[j]]$NVReturn/100
      }
    } #If the InnerCode is in the Selected Company, then got the multiply of all the return and save it in ith number
  }
  #get the length of all these return vectors
  TimeHorizon <- vector()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    TimeHorizon[i] <- length(ReturnRateUnSelected[[i]])
  }
  #The Length of the return vectors are not same, so we use risk free rate at the end of these vectors
  HoldPeriod <- max(TimeHorizon)
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    if(length(ReturnRateUnSelected[[i]]) < HoldPeriod)
    {
      for(j in length(ReturnRateUnSelected[[i]]):HoldPeriod)
        ReturnRateUnSelected[[i]][j] <- 1.000755
    }
    for(j in 1:HoldPeriod)
      if(is.na(ReturnRateUnSelected[[i]][j]))
        ReturnRateUnSelected[[i]][j] <- 1.000755
  }
  #plot the chart, make the no data weeks as risk free rate
  everyweekreturn <- vector()
  totalmoney <- vector()
  companymoney <- vector()
  for(i in 1:dim(UnSelectedCompany)[1])
  {
    companymoney[i] <- 3290000/dim(UnSelectedCompany)[1]
  }
  for(i in 1:HoldPeriod)
  {
    totalmoney[i] <- sum(companymoney)
    for(j in 1:dim(UnSelectedCompany)[1])
    {
      companymoney[j] <- companymoney[j] * ReturnRateUnSelected[[j]][i]
    }
  }
  UnSelectedMovingReturn[count] <- totalmoney[HoldPeriod]/totalmoney[1]
}

par(mfcol = c(1,2))

MovingReturnTotal <- vector()
MovingReturnTotal[1] <- MovingReturn[1]
for(i in 2:8)
{
  MovingReturnTotal[i] <- MovingReturnTotal[i-1]*MovingReturn[i]
}
MovingReturnTotal <- c(1,MovingReturnTotal)
MovingReturnTotal
MovingReturnTotal <- MovingReturnTotal[1:8]

MovingReturnTotalUn <- vector()
MovingReturnTotalUn[1] <- UnSelectedMovingReturn[1]
for(i in 2:8)
{
  MovingReturnTotalUn[i] <- MovingReturnTotalUn[i-1]*UnSelectedMovingReturn[i]
}
MovingReturnTotalUn <- c(1,MovingReturnTotalUn)
MovingReturnTotalUn
MovingReturnTotalUn<- MovingReturnTotalUn[1:8]

CSIR <- vector()
for(i in 1:7)
{
  CSIR[i] <- CSI[CSI$Date==FirstDate[i],]$Close/CSI[CSI$Date==FirstDate[1],]$Close
}

plot(MovingReturnTotalUn,ylim=c(0.8,1.3),col='red',type = 'l', main = 'Return Before Hedge')
points(MovingReturnTotal,col='blue', type = 'l')
points(CSIR,col='black', type = 'l')


#------------generate Beta Exposure-----------------#
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
