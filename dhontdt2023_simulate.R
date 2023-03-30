# Load the required packages
library(rvest)
library(tidyr)
library(tidytable)
library(pdftools)
library(tabulizer)
library(readxl)
library(zoo)
library(stringr)

rm(list = ls())
options(scipen=999)
set.seed(0606)


df2018 <- readRDS("gen_elections_2018.RDS")

df2018$coal <- df2018$party
df2018$coal[df2018$party=="ADALET VE KALKINMA PARTİSİ"] <- "cumhur"
df2018$coal[df2018$party=="MİLLİYETÇİ HAREKET PARTİSİ"] <- "cumhur"
df2018$coal[df2018$party=="CUMHURİYET HALK PARTİSİ"] <- "millet"
df2018$coal[df2018$party=="İYİ PARTİ"] <- "millet"
df2018$coal[df2018$party=="SAADET PARTİSİ"] <- "millet"


df2018$gen.flag <- as.numeric(df2018$coal=="cumhur" | 
                              df2018$coal=="millet" | 
                              df2018$coal=="HALKLARIN DEMOKRATİK PARTİSİ")

df2018 <- df2018 %>% 
          group_by(elec.region) %>% 
          mutate(tot.region.votes = sum(tot.votes)) %>% 
          ungroup() %>% 
          mutate(vote.share = tot.votes/tot.region.votes*100)

fun.list <- df2018 %>% 
            select(party,elec.region) 

#transition matrix 

pAKP <- c(69.2,6,1.7,3.1,1.5,2.1,2.6,10.9,3)
pCHP <- c(2.9,76.4,2.3,2.7,0.4,4.3,4.9,5.1,1)
pHDP <- c(1.4,9.8,76.1,1.2,0.6,1.6,5.1,2.4,1.7)
pIYIP <- c(2.8,21.1,0,49.9,0,5.7,8.4,10.8,1.5)
pMHP <- c(13.4,4.7,0,12.5,43.1,4.4,5.5,13.6,2.9)

x <- 1 
fun.list <- c("pAKP","pCHP","pHDP","pIYIP","pMHP")

dist.undecided.v1 <- function(x){
  tdf1 <- get(fun.list[x])
  tdf2 <- tdf1[-8]
  tdf2 <- tdf2 + tdf1[8]/100*tdf2
  out1 <- as.data.frame(tdf2) 
  names(out1) <- fun.list[x]
  row.names(out1) <- c("AKP","CHP","HDP","IYIP","MHP","MP","OTH","ABS")
  return(out1)
}

scenario1 <- lapply(1:5, dist.undecided.v1)
scenario1 <- do.call(cbind.data.frame,scenario1)
row.names(scenario1) <- c("AKP","CHP","HDP","IYIP","MHP","MP","OTH","ABS")

scenario2<- scenario1[c(1,2,3,4,5,8),]
scenario2[2,] <- scenario1[2,]  + scenario1[6,] + scenario1[7,]
row.names(scenario2) <- c("AKP","CHP+","HDP","IYIP","MHP","ABS")

 
scenario3 <- scenario1[c(1,2,3,5,8),]
scenario3[2,] <- scenario1[2,]  + scenario1[4,] + scenario1[6,] + scenario1[7,]
row.names(scenario3) <- c("AKP","CHP++","HDP","MHP","ABS")

fun.list <- unique(df2018$elec.region)

pchair.2023.sc1 <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
    filter(elec.region==prov)
  
  tot.pn <- sum(tdf$pn)
  tot.vote <- sum(tdf$tot.votes)
  
  AKP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[1,1] + 
                  tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[1,2] + 
                  tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[1,3] + 
                  tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[1,4] + 
                  tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[1,5] 
  
  CHP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[2,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[2,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[2,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[2,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[2,5] 
  
  HDP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[3,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[3,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[3,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[3,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[3,5] 
  

  IYIP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[4,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[4,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[4,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[4,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[4,5] 
  
  MHP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[5,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[5,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[5,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[5,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[5,5] 
  
  MP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[6,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[6,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[6,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[6,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[6,5] 
  
  OTH <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[7,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[7,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[7,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[7,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[7,5] 
  
  tdf3 <- as.data.frame(rbind(AKP, CHP, HDP, IYIP, MHP, MP, OTH))
  tdf3$party <- c("AKP","CHP","HDP","IYIP","MHP","MP","OTH")
  
  names(tdf3) <- c("tot.votes","party")
  

  hondt1 <- function(x){
    vt <- tdf3
    vt$rnd <- x 
    vt$votes <- vt$tot.votes/x 
    vt$votes[(vt$party=="MP" | vt$party=="OTH")] <- 0  
    return(vt)
  }
  
  out1 <- lapply(1:tot.pn, hondt1)
  out1 <- do.call(rbind.data.frame,out1)
  out1$vrank <- rank(-out1$votes)
  out1$win <- as.numeric(out1$vrank<=tot.pn)
  vote.min <- out1$votes[out1$vrank==tot.pn]
  out1$margin <- out1$votes - vote.min
  out1$pmargin <- out1$margin/tot.vote*100
  out1$elec.region <- fun.list[x]
  return(out1)
}

sim.2023.sc1 <- lapply(1:87, pchair.2023.sc1)
sim.2023.sc1 <- do.call(rbind.data.frame,sim.2023.sc1)



pchair.2023.sc2 <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
    filter(elec.region==prov)
  
  tot.pn <- sum(tdf$pn)
  tot.vote <- sum(tdf$tot.votes)
  
  AKP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[1,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[1,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[1,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[1,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[1,5] 
  
  CHP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[2,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[2,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[2,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[2,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[2,5] 
  
  HDP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[3,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[3,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[3,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[3,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[3,5] 
  
  
  IYIP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[4,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[4,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[4,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[4,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[4,5] 
  
  MHP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[5,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[5,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[5,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[5,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[5,5] 
  
  MP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[6,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[6,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[6,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[6,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[6,5] 
  
  OTH <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[7,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[7,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[7,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[7,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[7,5] 
  
  CHP <- CHP + MP + OTH 
  
  tdf3 <- as.data.frame(rbind(AKP, CHP, HDP, IYIP, MHP, MP, OTH))
  tdf3$party <- c("AKP","CHP","HDP","IYIP","MHP","MP","OTH")
  
  names(tdf3) <- c("tot.votes","party")
  
  tdf3 <- tdf3 %>% 
    filter(party!="MP") %>% 
    filter(party!="OTH")

  
  hondt1 <- function(x){
    vt <- tdf3
    vt$rnd <- x 
    vt$votes <- vt$tot.votes/x 
    return(vt)
  }
  
  out1 <- lapply(1:tot.pn, hondt1)
  out1 <- do.call(rbind.data.frame,out1)
  out1$vrank <- rank(-out1$votes)
  out1$win <- as.numeric(out1$vrank<=tot.pn)
  vote.min <- out1$votes[out1$vrank==tot.pn]
  out1$margin <- out1$votes - vote.min
  out1$pmargin <- out1$margin/tot.vote*100
  out1$elec.region <- fun.list[x]
  return(out1)
}

sim.2023.sc2 <- lapply(1:87, pchair.2023.sc2)
sim.2023.sc2 <- do.call(rbind.data.frame,sim.2023.sc2)


pchair.2023.sc3 <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
    filter(elec.region==prov)
  
  tot.pn <- sum(tdf$pn)
  tot.vote <- sum(tdf$tot.votes)
  
  AKP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[1,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[1,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[1,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[1,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[1,5] 
  
  CHP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[2,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[2,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[2,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[2,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[2,5] 
  
  HDP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[3,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[3,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[3,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[3,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[3,5] 
  
  
  IYIP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[4,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[4,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[4,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[4,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[4,5] 
  
  MHP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[5,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[5,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[5,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[5,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[5,5] 
  
  MP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[6,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[6,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[6,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[6,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[6,5] 
  
  OTH <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[7,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[7,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[7,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[7,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[7,5] 
  
  CHP <- CHP + MP + OTH + IYIP 
  
  tdf3 <- as.data.frame(rbind(AKP, CHP, HDP, IYIP, MHP, MP, OTH))
  tdf3$party <- c("AKP","CHP","HDP","IYIP","MHP","MP","OTH")
  
  names(tdf3) <- c("tot.votes","party")
  
  tdf3 <- tdf3 %>% 
    filter(party!="MP") %>% 
    filter(party!="OTH") %>%
    filter(party!="IYIP")
  
  
  hondt1 <- function(x){
    vt <- tdf3
    vt$rnd <- x 
    vt$votes <- vt$tot.votes/x 
    return(vt)
  }
  
  out1 <- lapply(1:tot.pn, hondt1)
  out1 <- do.call(rbind.data.frame,out1)
  out1$vrank <- rank(-out1$votes)
  out1$win <- as.numeric(out1$vrank<=tot.pn)
  vote.min <- out1$votes[out1$vrank==tot.pn]
  out1$margin <- out1$votes - vote.min
  out1$pmargin <- out1$margin/tot.vote*100
  out1$elec.region <- fun.list[x]
  return(out1)
}

sim.2023.sc3 <- lapply(1:87, pchair.2023.sc3)
sim.2023.sc3 <- do.call(rbind.data.frame,sim.2023.sc3)



# get the simulation results 

summary.sc1 <- sim.2023.sc1 %>% 
  group_by(party) %>% 
  summarise(pn = sum(win))

summary.sc2 <- sim.2023.sc2 %>% 
  group_by(party) %>% 
  summarise(pn = sum(win))

summary.sc3 <- sim.2023.sc3 %>% 
  group_by(party) %>% 
  summarise(pn = sum(win))

print(summary.sc1)  
print(summary.sc2)  
print(summary.sc3)  

#get the votes in each scenario 


get.votes.2023 <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
    filter(elec.region==prov)
  
  tot.vote <- sum(tdf$tot.votes)
  
  AKP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[1,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[1,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[1,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[1,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[1,5] 
  
  CHP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[2,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[2,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[2,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[2,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[2,5] 
  
  HDP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[3,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[3,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[3,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[3,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[3,5] 
  
  
  IYIP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[4,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[4,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[4,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[4,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[4,5] 
  
  MHP<- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[5,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[5,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[5,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[5,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[5,5] 
  
  MP <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[6,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[6,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[6,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[6,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[6,5] 
  
  OTH <- tdf$tot.votes[tdf$party=="ADALET VE KALKINMA PARTİSİ"]/100*scenario1[7,1] + 
    tdf$tot.votes[tdf$party=="CUMHURİYET HALK PARTİSİ" ]/100*scenario1[7,2] + 
    tdf$tot.votes[tdf$party=="HALKLARIN DEMOKRATİK PARTİSİ"]/100*scenario1[7,3] + 
    tdf$tot.votes[tdf$party=="İYİ PARTİ"]/100*scenario1[7,4] + 
    tdf$tot.votes[tdf$party=="MİLLİYETÇİ HAREKET PARTİSİ"]/100*scenario1[7,5] 
  
  CHP.sc1 <- CHP 
  CHP.sc2 <- CHP + MP + OTH 
  CHP.sc3 <- CHP + MP + IYIP 
  
  tdf3 <- as.data.frame(rbind(AKP, CHP.sc1,CHP.sc2,CHP.sc3, HDP, IYIP, MHP, MP, OTH))

  tdf3$party <- c("AKP","CHP-1","CHP-2","CHP-3","HDP","IYIP","MHP","MP","OTH")
  names(tdf3) <- c("tot.votes","party")
  tdf3$elec.region <- fun.list[x]
  
  return(tdf3)
}

votes.2023 <- lapply(1:87, get.votes.2023)
votes.2023  <- do.call(rbind.data.frame,votes.2023)
votes.2023$flag <- 1 
votes.2023$flag[votes.2023$party=="CHP-2"] <- 0 
votes.2023$flag[votes.2023$party=="CHP-3"] <- 0 

summary.votes <- votes.2023 %>% 
                 group_by(party) %>% 
                 summarise(tot.votes = sum(tot.votes), 
                           flag = mean(flag)) %>% 
                 ungroup() %>% 
                 mutate(vote.share = tot.votes/sum(tot.votes*flag)*100) %>% 
                 mutate(vote.share = round(vote.share,1))
