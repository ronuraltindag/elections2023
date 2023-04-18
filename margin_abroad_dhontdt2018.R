# Load the required packages
library(rvest)
library(tidyr)
library(tidytable)
library(pdftools)
library(tabulizer)
library(readxl)
library(zoo)
library(stringr)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(forcats)

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



g1 <- ggplot(out5) +
  geom_density_ridges(aes(x = margin*rnd/1000000, y = fct_reorder(party,  tot.votes, median), 
                          fill=as.factor(party)), stat="binline", bins=100) +
  scale_colour_viridis_c() + 
  geom_vline(xintercept = 0, linetype="dashed", colour="black", linewidth=0.7) + 
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("2018 Milletvekili Seçimleri Kazanma Kaybetme Marjini (Milyon oy)") +
  ylab("")



g2 <- out5 %>% 
  filter(abs(margin*rnd)<1000000) %>%
  ggplot() + 
  geom_density_ridges(aes(x = margin*rnd/1000000, y = fct_reorder(party,  tot.votes, median), 
                          fill=as.factor(party)), stat="binline", bins=100) +
  scale_colour_viridis_c() + 
  geom_vline(xintercept = 0, linetype="dashed", colour="black", linewidth=0.7) + 
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.5, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("2018 Milletvekili Seçimleri Kazanma Kaybetme Marjini (Milyon oy)") +
  ylab("")



g3 <- out5 %>% 
  #filter(abs(margin)<50000) %>%
  ggplot() + 
  geom_density_ridges(aes(x = margin, y = fct_reorder(party,  tot.votes, median)), 
                          fill="black", colour="white", stat="binline", bins=50) +
  
  #geom_density_ridges(aes(x = custom.votes/rnd, y = fct_reorder(party,  tot.votes, median)), 
  #                    fill="black", colour="white", stat="binline", bins=20) +
  #scale_colour_viridis_c() + 
  geom_vline(xintercept = 0, linetype="dashed", colour="black", linewidth=0.7) + 
  xlim(-100000,100000) + 
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(3, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("2018 Milletvekili Seçimleri Kazanma Kaybetme Marjini (Milyon oy)") +
  ylab("")



g4 <- out5 %>% 
      ggplot(aes(x=custom.votes/(rnd), fill=fct_reorder(party,  -tot.votes, median))) + 
      geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins=50) +
  xlab("yurtdisi oylarin etkisi") + 
  ylab("bolge sayisi") + 
  scale_fill_viridis_d() + 
  theme_ipsum() +
  labs(fill="") 