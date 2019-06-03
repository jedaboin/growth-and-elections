nl_col_growth <- function(year1, year2, electoral){
  require(dplyr)
  require(reshape2)
  n <- year2-year1+1
  nl <- nlcol
  ####Night Lights Growth Rates
  ##Growth Rate year to year
  nl <- nl %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(g1=logint-lag(logint, n=1, default = first(logint)))
  ###Average during period where political connection coincide
  ##Accumulated growth
  nl <- nl %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(gr_avg=logint-lag(logint, n=(n+1), default = first(logint)))
  
  ##Code
  codes <- codes
  colnames(codes) <- c("dpto", "code")
  
  ##Population
  population <- melt(population, id="code")
  colnames(population) <- c("code", "year", "population")
  
  
  ##Tax Income 1993-1999
  condition <- as.character(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999[3,3])
  budget <- subset(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999, CUENTA==condition, select = c("code", 1993:1999))
  f_1993 <- melt(budget, id=c("code"))
  colnames(f_1993) <- c("code", "year", "taxes")
  f_1993 <- merge(codes, f_1993, by="code")
  ###Taking fiscal accounts
  ##Tax Income 2000-2012
  fiscal <- subset(Budget_2000_2017, CdigoCuenta=="A1000", select = c("coddep", "code", 2000:2015))
  fiscal <- melt(fiscal, id=c("coddep", "code"))
  colnames(fiscal) <- c("dpto", "code", "year", "taxes")
  ##Merging both Tax Income
  fiscal <- rbind(f_1993, fiscal)
  fiscal$taxes <- as.numeric(fiscal$taxes)
  ##Real Tax Income (Base 2008)
  fiscal <- merge(fiscal, inflation, by="year") ##Merging Inflation Series into the dataset
  fiscal$taxes.real <- fiscal$taxes/fiscal$gr_ipc
  ##Current Transfers 1993-1999
  condition <- as.character(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999[8,3])
  budget <- subset(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999, CUENTA==condition, select = c("code", 1993:1999))
  f_1993 <- melt(budget, id=c("code"))
  colnames(f_1993) <- c("code", "year", "cur_transfers")
  f_1993 <- merge(codes, f_1993, by="code")
   ##Current Transfers 2000-2012
  transfers <- subset(Budget_2000_2017, CdigoCuenta=="A3010", select = c("coddep", "code", 2000:2015))
  transfers <- melt(transfers, id=c("coddep", "code"))
  colnames(transfers) <- c("dpto", "code", "year", "cur_transfers")
  ##Merging Current Transfers
  transfers <- rbind(f_1993, transfers)
  transfers$cur_transfers <- as.numeric(transfers$cur_transfers)
  fiscal <- merge(fiscal, transfers, by=c("dpto" ,"code", "year"))
  ##Real Current Transfers (Base 2008)
  fiscal$cur_transfers.real <- fiscal$cur_transfers/fiscal$gr_ipc
  ##Capital Transfers 1993-1999
  condition <- as.character(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999[22,3])
  budget <- subset(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999, CUENTA==condition, select = c("code", 1993:1999))
  f_1993 <- melt(budget, id=c("code"))
  colnames(f_1993) <- c("code", "year", "cap_transfers")
  f_1993 <- merge(codes, f_1993, by="code")
  ##Capital Transfers 2000-2012
  transfers <- subset(Budget_2000_2017, CdigoCuenta=="D2000", select = c("coddep", "code", 2000:2015))
  transfers <- melt(transfers, id=c("coddep", "code"))
  colnames(transfers) <- c("dpto", "code", "year", "cap_transfers")
  ##Merging Capital Transfers
  transfers <- rbind(f_1993, transfers)
  transfers$cap_transfers <- as.numeric(transfers$cap_transfers)
  fiscal <- merge(fiscal, transfers, by=c("dpto" ,"code", "year"))
  ##Real Capital Transfers
  fiscal$cap_transfers.real <- fiscal$cap_transfers/fiscal$gr_ipc
  ##Total Real Transfers
  fiscal$totaltransfers <- fiscal$cap_transfers.real+fiscal$cur_transfers.real
  ##Total Income 1993-1999
  condition <- as.character(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999[1,3])
  budget <- subset(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999, CUENTA==condition, select = c("code", 1993:1999))
  f_1993 <- melt(budget, id=c("code"))
  colnames(f_1993) <- c("code", "year", "totalincome")
  f_1993 <- merge(codes, f_1993, by="code")
  ##Total Income 2000-2012
  income <- subset(Budget_2000_2017, CdigoCuenta=="A", select = c("coddep", "code", 2000:2015))
  income <- melt(income, id=c("coddep", "code"))
  colnames(income) <- c("dpto", "code", "year", "totalincome")
  ##Merging Total Income
  income <- rbind(f_1993, income)
  income$totalincome <- as.numeric(income$totalincome)
  fiscal <- merge(fiscal, income, by=c("dpto" ,"code", "year"))
  ##Real Total Income (Base 2008)
  fiscal$totalincome.real <- fiscal$totalincome/fiscal$gr_ipc
  ##Royalties 1993-1999
  condition <- as.character(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999[21,3])
  budget <- subset(Copia_de_EjecucionesPresupuestales_Municipios_1993_1999, CUENTA==condition, select = c("code", 1993:1999))
  f_1993 <- melt(budget, id=c("code"))
  colnames(f_1993) <- c("code", "year", "royalty")
  f_1993 <- merge(codes, f_1993, by="code")
  ##Royalties 2000-2012
  income <- subset(Budget_2000_2017, CdigoCuenta=="D1000", select = c("coddep", "code", 2000:2015))
  income <- melt(income, id=c("coddep", "code"))
  colnames(income) <- c("dpto", "code", "year", "royalty")
  ##Merging Royalties
  income <- rbind(f_1993, income)
  income$royalty <- as.numeric(income$royalty)
  fiscal <- merge(fiscal, income, by=c("dpto" ,"code", "year"))
  ##Real Royalties (Base 2008)
  fiscal$royalty.real <- fiscal$royalty/fiscal$gr_ipc
  ##Fiscal Independence
  fiscal$fiscal.ind <- fiscal$taxes.real/fiscal$totalincome.real
  ##Per capita
  fiscal <- merge(fiscal, population, by=c("code", "year"))
  fiscal$taxespc <- fiscal$taxes.real*1000000/fiscal$population
  fiscal$totalincomepc <- fiscal$totalincome.real*1000000/fiscal$population
  fiscal$transferpc <- fiscal$totaltransfers*1000000/fiscal$population
  fiscal$cap_transferspc <- fiscal$cap_transfers.real*1000000/fiscal$population
  fiscal$cur_transferspc <- fiscal$cur_transfers.real*1000000/fiscal$population
  fiscal$royaltypc <- fiscal$royalty.real*1000000/fiscal$population
  
  ##GDP Calculations
  ##Departmental Tax
  deptax <- aggregate(fiscal$taxes.real, by=list(dpto=fiscal$dpto, year=fiscal$year), FUN=sum)
  colnames(deptax)[3] <- "deptax"
  fiscal <- merge(fiscal, deptax, by=c("dpto", "year"))
  fiscal$gdpshare <- fiscal$taxes.real/fiscal$deptax
  ##Departmental GDP
  depgdp <- melt(gdpdepartmental, id="dpto")
  colnames(depgdp) <- c("dpto", "year", "depgdp")
  fiscal <- merge(fiscal, depgdp, by=c("dpto", "year"), all = TRUE)
  fiscal$mungdp <- fiscal$gdpshare*fiscal$depgdp*1000000
  fiscal$gdppc <- fiscal$mungdp/fiscal$population
  
  fiscal$year <- as.numeric(fiscal$year)
  for(i in 1:23){
    fiscal$year[fiscal$year==i] <- 1992+i
  }
  fiscal <- subset(fiscal, year<=year2 & year>=year1-1)
  
  fiscal <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(cap.transfers.avg=mean(cap_transferspc))
  
  fiscal <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(royalties.avg=mean(royaltypc))
  
  fiscal <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(cur.transfers.avg=mean(cur_transferspc))
  
  fiscal <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(gdp.gr=(log(gdppc)-log(lag(gdppc, n=(n+1), default = first(gdppc)))))
  
  fiscal <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(fiscal.chg=fiscal.ind-lag(fiscal.ind, n=(n+1), default = first(fiscal.ind)))
  
  fiscal  <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(cur.trnf.chg=(log(cur_transferspc)-log(lag(cur_transferspc, n=(n+1), default = first(cur_transferspc)))))
  
  fiscal  <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(cap.trnf.chg=(log(cap_transferspc)-log(lag(cur_transferspc, n=(n+1), default = first(cap_transferspc)))))
  
  fiscal  <- fiscal %>%
    group_by(code) %>%
    arrange(year, .by_group=TRUE) %>%
    mutate(roy.trnf.chg=(log(royaltypc)-log(lag(royaltypc, n=(n+1), default = first(royaltypc)))))
  
  fiscal <- subset(fiscal, year==year2, select = c(code, royalties.avg, cur.transfers.avg, gdp.gr, cap.transfers.avg, fiscal.chg, cur.trnf.chg, cap.trnf.chg, roy.trnf.chg))
  fiscal$gdp.gr[fiscal$gdp.gr<=-1 | fiscal$gdp.gr>=1] <- NA
  fiscal$gdp.gr[fiscal$gdp.gr==Inf] <- NA
  fiscal$gdp.gr[fiscal$gdp.gr==-Inf] <- NA
  fiscal[is.na(fiscal)] <- NA
  fiscal$cur.trnf.chg[fiscal$cur.trnf.chg==-1] <- NA
  fiscal$cur.trnf.chg[fiscal$cur.trnf.chg==Inf] <- NA
  fiscal$cur.trnf.chg[fiscal$cur.trnf.chg==-Inf] <- NA
  fiscal$cap.trnf.chg[fiscal$cap.trnf.chg==-1] <- NA
  fiscal$cap.trnf.chg[fiscal$cap.trnf.chg==Inf] <- NA
  fiscal$cap.trnf.chg[fiscal$cap.trnf.chg==-Inf] <- NA
  fiscal$roy.trnf.chg[fiscal$roy.trnf.chg==-1] <- NA
  fiscal$roy.trnf.chg[fiscal$roy.trnf.chg==Inf] <- NA
  fiscal$roy.trnf.chg[fiscal$roy.trnf.chg==-Inf] <- NA

  if(year2==2015){
    valueadded$codmpio <- as.numeric(valueadded$codmpio)
    colnames(valueadded)[3] <- "code"
    valueadded <- merge(valueadded, population, by=c("year", "code"))
    valueadded$gdppc <- valueadded$valueadded*1000000000/valueadded$population
    valueadded <- valueadded %>%
      group_by(code) %>%
      arrange(year, .by_group=TRUE) %>%
      mutate(gdp.gr=(log(gdppc)-log(lag(gdppc, n=1, default = first(gdppc)))))
    valueadded <- subset(valueadded, year==2015) 
    fiscal <- merge(fiscal, valueadded, by="code")
    fiscal <- fiscal[c(1:3, 17, 5:9)]
    colnames(fiscal)[4] <- "gdp.gr" 
  }
  
  fiscal <<- fiscal
  
  elect <- electoral
  colnames(elect)[3] <- "code"
  nl1 <- subset(nl, year==(year1+1), select = c(code, g1))
  if (year2==2015){nl2 <- subset(nl, year==2013, select = c(code, gr_avg))}
  else{
  nl2 <- subset(nl, year==year2+1, select = c(code, gr_avg))
  }
  nl3 <- merge(nl2, nl1, by="code")
  nl3 <- merge(nl3, fiscal, by="code")
  rds <<- merge(elect, nl3, by="code", all=FALSE)
  if (!exists("rd_final")){
    rd_final <- NA
    rd_final <<- rds
  }
  else {
  rd_final <<- rbind(rds, rd_final)
  }
}