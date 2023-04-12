# Load the required packages
library(rvest)
library(tidyr)
#library(tidytable)
library(pdftools)
#library(tabulizer)
library(readxl)
library(zoo)
library(stringr)
library(dplyr)

rm(list = ls())
options(scipen=999)

# place this file to the same folder that you have the R script 
df2018 <- readRDS("gen_elections_2018.RDS")
df2018$party <- as.factor(df2018$party)



df2018$coal <- df2018$party
df2018$coal[df2018$party=="ADALET VE KALKINMA PARTİSİ"] <- "cumhur"
df2018$coal[df2018$party=="MİLLİYETÇİ HAREKET PARTİSİ"] <- "cumhur"
df2018$coal[df2018$party=="CUMHURİYET HALK PARTİSİ"] <- "millet"
df2018$coal[df2018$party=="İYİ PARTİ"] <- "millet"



df2018$gen.flag <- as.numeric(df2018$coal=="cumhur" | 
                              df2018$coal=="millet" | 
                              df2018$coal=="HALKLARIN DEMOKRATİK PARTİSİ")


#replicate the allocation 
#general multiplier 
k <- sum(df2018$custom.votes)/sum(df2018$local.votes)

df2018$party <- as.factor(df2018$party)
#party multiplier 
df.party.k <- df2018 %>% 
           group_by(party) %>%
           summarise(custom.votes = sum(custom.votes)) %>% 
           mutate(k = custom.votes/sum(custom.votes)) %>% 
           select(-custom.votes)


df.region.2018 <- df2018 %>% 
          group_by(elec.region) %>% 
          summarise(local.votes=sum(local.votes), 
                    custom.votes.reg=sum(custom.votes), 
                    tot.votes = sum(tot.votes)) %>%
          mutate(add.vote = local.votes*k) %>%
          select(elec.region,custom.votes.reg,add.vote)

df2018 <- merge(df2018, df.region.2018, by="elec.region")
df2018 <- merge(df2018,df.party.k,by="party")
df2018 <- df2018 %>% 
          mutate(tot.votes.sim = round(
            local.votes +  add.vote*k), 
            flag = as.numeric(tot.votes==tot.votes.sim))   

table(df2018$flag)

#replicate the allocation, works except rounding

#fix the common lists
df2018$party[df2018$party=="HÜR DAVA PARTİSİ"] <- "ADALET VE KALKINMA PARTİSİ" 
df2018$party[df2018$party=="SAADET PARTİSİ"] <- "CUMHURİYET HALK PARTİSİ"


df2018 <- df2018 %>%
  group_by(elec.region,party,yr) %>%
  summarise_if(is.numeric, sum)



#re do the party multiplier 
df.party.k <- df2018 %>% 
  group_by(party) %>%
  summarise(custom.votes = sum(custom.votes)) %>% 
  mutate(k = custom.votes/sum(custom.votes)) %>% 
  select(-custom.votes)

chp.hdp.iyip.k <- df.party.k$k[df.party.k$party=="CUMHURİYET HALK PARTİSİ"] + 
                     df.party.k$k[df.party.k$party=="HALKLARIN DEMOKRATİK PARTİSİ"] + 
                     df.party.k$k[df.party.k$party=="İYİ PARTİ"]  

chp.hdp.k  <-        df.party.k$k[df.party.k$party=="CUMHURİYET HALK PARTİSİ"] + 
                     df.party.k$k[df.party.k$party=="HALKLARIN DEMOKRATİK PARTİSİ"] 

chp.iyip.k <-        df.party.k$k[df.party.k$party=="CUMHURİYET HALK PARTİSİ"] + 
                     df.party.k$k[df.party.k$party=="İYİ PARTİ"] 



df2018 <- df2018 %>% 
          group_by(elec.region) %>% 
          mutate(tot.region.votes = sum(tot.votes)) %>% 
          ungroup() %>% 
          mutate(vote.share = tot.votes/tot.region.votes*100) 


df.party.k <- df.party.k %>% 
  mutate(sc1 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ chp.hdp.iyip.k, 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~ 0, 
                         party=="İYİ PARTİ" ~ 0, 
                         TRUE ~ k), 
         sc2 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ 0 , 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~ chp.hdp.iyip.k, 
                         party=="İYİ PARTİ" ~ 0, 
                         TRUE ~ k),
         sc3 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ 0 , 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~ 0 , 
                         party=="İYİ PARTİ" ~ chp.hdp.iyip.k, 
                         TRUE ~ k),
         sc4 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ chp.hdp.k , 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~ 0 ,  TRUE ~ k),
         sc5 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ 0 , 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~ chp.hdp.k, TRUE ~k), 
         sc6 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ chp.iyip.k , 
                         party=="İYİ PARTİ" ~ 0, TRUE ~k), 
         sc7 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ 0 , 
                         party=="İYİ PARTİ" ~ chp.iyip.k, TRUE ~k), 
         sc8 = case_when(party=="CUMHURİYET HALK PARTİSİ" ~ 0 , 
                         party=="İYİ PARTİ" ~ 0, 
                         party=="HALKLARIN DEMOKRATİK PARTİSİ" ~0,
                         TRUE ~k), 
         
  )

       

fun.list <- unique(df2018$elec.region)
klist <- names(df.party.k)

x <- 1
k <- 2

#simulation with the new rule 


pchair.2023 <- function(x,k){
  prov <- fun.list[x]
  tdf1 <- df2018 %>% 
    filter(elec.region==prov) %>%
    select(party,elec.region,local.votes,custom.votes.reg,pn)
  
  tdf2 <- df.party.k %>% 
          select(party,klist[k])
  
  names(tdf2) <- c("party","k")
  
  tdf <- merge(tdf1,tdf2,by="party")
  tdf$tot.votes <- tdf$local.votes + tdf$custom.votes.reg*tdf$k
    
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
  
  out1$party <- as.factor(out1$party)

  out1 <- out1 %>% 
          group_by(party) %>% 
          summarise(win = sum(win))
  
  out1$elec.region <- fun.list[x]
  return(out1)
}

z <- 2

get.sim <- function(z){
  out.sc <- lapply(1:87, pchair.2023, k=z)
  out.sc <- do.call(rbind.data.frame,out.sc)
  t1 <- out.sc %>% 
    group_by(party) %>% 
    summarise(win=sum(win))
  t1$sc <- z-1
  return(t1)
}

out.sim <- lapply(2:10, get.sim)
out.sim2 <- do.call(rbind.data.frame,out.sim)

out.sim2$dem.bloc <- 0 
out.sim2$dem.bloc[out.sim2$party=="CUMHURİYET HALK PARTİSİ"] <- 1 
out.sim2$dem.bloc[out.sim2$party=="HALKLARIN DEMOKRATİK PARTİSİ"] <- 1 
out.sim2$dem.bloc[out.sim2$party=="İYİ PARTİ"] <- 1 

out.sim3 <- out.sim2 %>% 
            group_by(sc,dem.bloc) %>% 
            summarise(win=sum(win))

out.sim2 <- out.sim2 %>% 
            select(-dem.bloc) %>% 
            pivot_wider(names_from = sc, values_from = win)

out.sim3 <- out.sim3 %>% 
  pivot_wider(names_from = sc, values_from = win)

print(out.sim2)

tdf <- df2018 %>% 
       filter(party=="CUMHURİYET HALK PARTİSİ" |
       party=="HALKLARIN DEMOKRATİK PARTİSİ" | 
       party=="İYİ PARTİ") %>%
       group_by(party) %>% 
       summarise(custom.votes = sum(custom.votes)) %>% 
       mutate(custom.votes.sum = sum(custom.votes))

CHP.value <-  (252-240)/600332*100000 
HDP.value <-  (241-240)/600332*100000  
IYIP.value <- (248-240)/600332*100000 

      

