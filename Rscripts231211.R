
# R codes for prediction of calving date ------------------------------------------------------

## author: Shigeki KISHI
## affiliate: National Agriculture and Food Research Organization
## R version: R 4.2.1
## all codes were made on R studio 2023.9.1 B494
## the latest update: 11st, Dec. 2023


#setwd("C:/Users/kishis201/Desktop/ウシ分娩予測/211203分娩予測")

library(forecast)
library(magrittr)
library(tidyverse)
library(patchwork)
library(TTR)

#H057
h057 <- read.csv("dat_H057.csv") %>% na.omit() %>% as_tibble()
#H021
h021 <- read.csv("dat_H021.csv") %>% na.omit() %>% as_tibble()
#H045
h045 <- read.csv("dat_H045.csv") %>% na.omit() %>% as_tibble()
#H055
h055 <- read.csv("dat_H055.csv") %>% na.omit() %>% as_tibble()
#H058
h058 <- read.csv("dat_H058.csv") %>% na.omit() %>% as_tibble()
#H886
h886 <- read.csv("dat_H886.csv") %>% na.omit() %>% as_tibble()

# recorded every 30 minutes

## STL analysis

stl_RR <- function(x){
  x$RR %>% ts(freq = 48) %>% 
    stl(s.window="per") %>% 
    .$time.series %>% as.data.frame() %>% 
    cbind(x %>% select(Time, RR)) %>% 
    mutate(num = 1:nrow(.)) %>% 
    mutate(hour = num*0.5) %>% 
    select(-num)
}

df57ori <- h057 %>% stl_RR()
df21ori <- h021 %>% stl_RR()
df45ori <- h045 %>% stl_RR()
df55ori <- h055 %>% stl_RR()
df58ori <- h058 %>% stl_RR()
df886ori <- h886 %>% stl_RR()

## 上の３つのグラフ
func_figset1 <- function(x){ #x = data
  p <- ggplot(data=x)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=15, color="black"),
          axis.text.x = element_blank(),
          plot.margin = unit(c(2,6,2,2), units="mm")
    )
  
  return(p)
}

##　下のグラフremainder
func_figset2 <- function(x){ #x = data
  p <- ggplot(data=x)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=15, color="black"),
          axis.text.y = element_text(size=15, color="black"),
          axis.text.x = element_text(size=15, color="black"),
          plot.margin = unit(c(2,6,2,2), units="mm")
    )
  
  return(p)
}

#  for 4 figures
func_4figs <- function(x){
  ## observed
  po <- func_figset1(x)+
    geom_line(aes(x=hour, y=RR), color="black", size=0.7)
  ## trend
  pt <- func_figset1(x)+
    geom_line(aes(x=hour, y=trend), color="black", size=0.7)
  ## seasonal
  ps <- func_figset1(x)+
    geom_line(aes(x=hour, y=seasonal), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed")
  ## remainder
  pr <- func_figset2(x)+
    geom_line(aes(x=hour, y=remainder), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed")+
    xlab("Hour")
  
  pa <- po+pt+ps+pr+plot_layout(ncol=1)
  
  return(pa)
}


df_list <- list(df21ori, df45ori, df55ori, df57ori, df58ori, df886ori)

fig_list <- df_list %>% lapply(function(x){
  p <- func_4figs(x)
  return(p)
  }
  )

## ind. h021, h045, h057
g <- fig_list[c(1,2,4)] %>% wrap_plots(nrow=1)
g
ggsave(g, filename="stl_figs.png", height=6, width=9)

g1 <- fig_list[c(1, 2, 4, 3, 5, 6)] %>% wrap_plots(nrow=1)
g1
ggsave(g1, filename="stl_figs_all.png", height=6, width=12)


##
g1 <- fig_list[c(3, 5, 6)] %>% wrap_plots(nrow=1)
g1
ggsave(g1, filename="stl_figs_rest.png", height=6, width=9)


## for Fig. 1

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## for Fig. 2
## ##

## Trend+SMA, MAD, RSI

indic_list <- df_list %>% lapply(
  function(x){
    p <- x %>% select(hour, trend) %>% 
      mutate(SMA12 = SMA(trend, n=12)) %>% 
      mutate(MAD12 = 100*(trend - SMA12)/SMA12) %>% 
      mutate(RSI = RSI(trend)) %>% 
      cbind(MACD(x$trend) %>% as.data.frame()) %>% 
      cbind(stoch(x$trend) %>% as.data.frame()) %>% 
      mutate(DPO = DPO(trend, n=20, shift=-10))
    return(p)
  }
)


### SMA

SMAfig_func <- function(x){
  p <- x %>% select(hour, trend, SMA12) %>% 
    pivot_longer(cols=-hour, names_to="type", values_to="value") %>% 
    func_figset1()+
    theme(
      legend.title=element_blank(),
      legend.text=element_text(size=10),
      legend.position = c(0.05, 0.05),
      legend.justification = c(0,0)
      
    )+
    geom_line(aes(x=hour, y=value, linetype=type), color="black", size=0.7)+
    scale_linetype_manual(values=c(trend = "solid", SMA12="dashed"),labels = c(trend = "TREND", SMA12 ="SMA") )
  return(p)
}

### MAD moving average deviation rate

MADfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=MAD12), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}

### MACD

MACDfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=macd), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}

### DPO: de-trended price oscillator

DPOfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=DPO), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}

### RSI

rsi_func <- function(x){
  p <- x %>% func_figset2()+
    geom_line(aes(x=hour, y=RSI), color="black", size=0.7)+
    geom_hline(yintercept=50, color="black", linetype="dashed", size=0.7)+
    ylim(c(0, 100))+xlab("Hour")
  return(p)
}

## making figures


#  for 4 figures
func_4indic_figs <- function(x){
  ## trend+SMA
  pt <- SMAfig_func(x)
  ## DPO
  pd <- DPOfig_func(x)
  ## MACD
  pm <- MACDfig_func(x)
  ## RSI
  pr <- rsi_func(x)

  pa <- pt+pd+pm+pr+plot_layout(ncol=1)
  
  return(pa)
}

fig_list2 <- indic_list %>% lapply(function(x){
  p <- func_4indic_figs(x)
  return(p)
}
)


## arranging and saving figures

g <- fig_list2[c(1,2,4)] %>% wrap_plots(nrow=1)
g
ggsave(g, filename="indic_figs.png", height=6, width=9)

g1 <- fig_list2[c(1, 2, 4, 3, 5, 6)] %>% wrap_plots(nrow=1)
g1
ggsave(g1, filename="indic_figs_all.png", height=6, width=12)

g2 <- fig_list2[c(3, 5, 6)] %>% wrap_plots(nrow=1)
g2
ggsave(g2, filename="indic_figs_rest.png", height=6, width=9)


