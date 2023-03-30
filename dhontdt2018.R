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

# place this file to the same folder that you have the R script 
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

fun.list <- unique(df2018$elec.region)

x <- 1

pchair <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
         filter(elec.region==prov & gen.flag==1)
  
  tot.pn <- sum(tdf$pn)
  
  tdf2 <- tdf %>% 
    group_by(coal) %>%
    summarise(votes = sum(tot.votes))
  
  hondt1 <- function(x){
    vt <- tdf2
    vt$rnd <- x 
    vt$votes <- vt$votes/x 
    return(vt)
  }
  
  out1 <- lapply(1:tot.pn, hondt1)
  out1 <- do.call(rbind.data.frame,out1)
  out1$vrank <- rank(-out1$votes)
  out1$win <- as.numeric(out1$vrank<=tot.pn)
  vote.min <- out1$votes[out1$vrank==tot.pn]
  out1$margin <- out1$votes - vote.min
  out1$elec.region <- fun.list[x]
    
return(out1)
}

out1 <- lapply(1:87, pchair)
out1 <- do.call(rbind.data.frame,out1)

#assigned seats by coalitions + HDP 
des.coal <- out1 %>% 
            group_by(coal,elec.region) %>% 
            summarise(pnc=sum(win)) 

out1 %>% group_by(coal) %>% summarise(pn=sum(win))

#re-allocation of the seats within each coalition 

dft1 <- df2018 %>% filter(coal=="cumhur" | coal == "millet")
dft2 <- merge(dft1,des.coal, by = c('coal','elec.region'))

pchair.coal <- function(x){
  prov <- fun.list[x] 
  t1 <- dft2 %>% filter(elec.region==prov, coal == "cumhur")
  t2 <- dft2 %>% filter(elec.region==prov, coal == "millet")
  pn.t1 <- max(t1$pnc)
  pn.t2 <- max(t2$pnc)
  
  hondt1 <- function(t,x){
    vt <- t
    vt$rnd <- x 
    vt$votes <- vt$tot.votes/x 
    return(vt)
  }
  
  out1 <- lapply(1:pn.t1, hondt1, t=t1)
  out1 <- do.call(rbind.data.frame,out1) 
  out1$vrank <- rank(-out1$votes)
  out1$win <- as.numeric(out1$vrank<=pn.t1)
  vote.min <- out1$votes[out1$vrank==pn.t1]
  out1$margin <- out1$votes - vote.min
  out1$elec.region <- fun.list[x]
  
  out2 <- lapply(1:pn.t2, hondt1, t=t2)
  out2 <- do.call(rbind.data.frame,out2) 
  out2$vrank <- rank(-out2$votes)
  out2$win <- as.numeric(out2$vrank<=pn.t2)
  vote.min <- out2$votes[out1$vrank==pn.t2]
  out2$margin <- out2$votes - vote.min
  out2$elec.region <- fun.list[x]
  
  out3 <- as.data.frame(rbind(out1,out2))
  return(out3)
}
  
out2 <- lapply(1:87, pchair.coal)
out2 <- do.call(rbind.data.frame,out2)

des.coal2 <- out2 %>% 
             group_by(party) %>% 
             summarise(pn = sum(win)) %>% 
             filter(party!="SAADET PARTİSİ") 

add.HDP <- as.data.frame(cbind("HALKLARIN DEMOKRATİK PARTİSİ",
                         sum(des.coal$pnc[des.coal$coal=="HALKLARIN DEMOKRATİK PARTİSİ"])))
names(add.HDP) <- names(des.coal2)

#final seat distribution in 2018 
des.coal2 <- rbind(des.coal2,add.HDP)
              

#simulation with the new rule 

pchair.2023 <- function(x){
  prov <- fun.list[x]
  tdf <- df2018 %>% 
    filter(elec.region==prov)
  
  tot.pn <- sum(tdf$pn)
  tot.vote <- sum(tdf$tot.votes)
  
  hondt1 <- function(x){
    vt <- tdf
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

out5 <- lapply(1:87, pchair.2023)
out5 <- do.call(rbind.data.frame,out5)


#seat distribution in 2023 using 2018 election resuts 
des.coal3 <- out5 %>% group_by(party) %>% 
             summarise(pn=sum(win)) %>% 
             filter(pn>0)

#comparison of 2018 vs 2013 
summary.2018 <- merge(des.coal2,des.coal3, by="party")
names(summary.2018) <- c("party", "2018 results, 2018 hondt","2018 results, 2023 hondt") 


#election results 
agg.results.2018 <- df2018 %>% 
               group_by(party) %>% 
               summarise(votes = sum(tot.votes)) %>% 
               ungroup() %>% 
               mutate(vote.share = round(votes/sum(votes)*100,2))


