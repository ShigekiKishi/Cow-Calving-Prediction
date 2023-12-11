
# 221128 分娩予測解析　検証など ------------------------------------------------------


setwd("C:/Users/kishis201/Desktop/ウシ分娩予測/211203分娩予測")

library(forecast)
library(magrittr)
library(tidyverse)
library(patchwork)
library(TTR)

load("Rdata211203.RData")

# データは30分おきにとられている
## indivID 21, 45, 55, 57, 58, 886

## MACD

df21 %>% head()
df21macd %>% head()
p <- df21macd %>% select(num, macd, signal) %>% 
  pivot_longer(cols=-num, names_to = "type", values_to="value") %>% 
  ggplot(aes(x=num, y=value, color=type))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_line()
p

## SMA, RSI, MACD, PBand, aroon, CMO(Chande momentum oscill.), DPO, stochastics
df21macd %>% head()
df21i <- df21 %>% mutate(SMA9 = SMA(trend, n=9)) %>% 
  mutate(SMA48 = SMA(trend, n=48)) %>% 
  mutate(RSI = RSI(trend, n=14)) %>%
  mutate(MACD = MACD(trend)) %>%
  mutate(PBands(trend)) %>% 
  mutate(Aroon = aroon(trend)) %>%
  mutate(CMO = CMO(trend)) %>% # chande momentum oscillator
  mutate(DPO = DPO(trend)) %>%
  mutate(stoch= stoch(trend))

df21i %<>% as.matrix() %>% as.data.frame()
df21i %>% colnames()
plot(data=df21i, trend~num, type="l")
plot(data=df21i, DPO~num, type="l")
## DPOはたぶん内部の計算がおかしい


## SMAからやってみるか
df21test <- df21i %>% select(num, trend, SMA24, SMA48) %>% 
  pivot_longer(cols=-c(num, trend), names_to="SMA", values_to="value")

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        strip.background = element_rect(fill="transparent")
        )+
  geom_line(aes(x=num, y=trend), color="black")+
  geom_line(aes(x=num, y=value), color="red")+facet_wrap(.~SMA)
p

## SMA以外を検討する
df21i %>% names()

## RSI
df21test <- df21i %>% select(num, RSI) %>% mutate(signal = SMA(RSI, 9)) %>% 
  pivot_longer(cols=-c(num), names_to="type", values_to="value")
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=value, color=type))
p

## MACD
df21i %>% names()
df21test <- df21i %>% select(num, MACD.macd, MACD.signal) %>% 
  pivot_longer(cols=-c(num), names_to="type", values_to="value")
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=value, color=type))
p

## Stochas
df21i %>% names()
df21test <- df21i %>% select(num, stoch.fastD, stoch.slowD) %>% 
  pivot_longer(cols=-c(num), names_to="type", values_to="value")
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=value, color=type))
p


## MAD
df21i %>% names()
df21test <- df21i %>% select(num, DPO) 
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=DPO))
p

## PBand
df21i %>% names()
df21test <- df21i %>% select(num, trend, `PBands(trend).dn`, `PBands(trend).center`, `PBands(trend).up`) %>% 
  pivot_longer(cols=-num, names_to="type", values_to="value")
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=value, color=type))
p

## 滑らかなカーブにBBは合わない。やってもあまり意味がない

## 
df21i %>% head()

## テスト
data(ttrc)
priceDPO <- DPO(ttrc[, "Close"])
ts.plot(priceDPO[5300:length(priceDPO)])

ts.plot(DPO(sin(1:100)))
ts.plot(DPO(df21i$trend, n=20, shift=-11, percent=T))
## DPOの理解をした。DPOには２つある。
#* 本来のDPOはSMAとの差を取るんだけど、SMAの真ん中との差を取るので、
#* ほんとの移動平均みたいなものになる
#* つまり、テクニカルでいう移動平均乖離率は price - SMA で、これは
#* SMA---- price なんだけど、DPOの場合は、 --price-- って感じ。だから
#* トレンド除去になる。だけど、これだと配置がおかしい。
#* もう一つのDPOが自分の理解していたDPOで、これは SMA----■■price
#* というもの。移動平均の先からの10日かんで動いた値幅を示す。

df21i %<>% mutate(DPO=DPO(trend, n=20, shift=-11))

## moving average deviation rate

df21i %>% head()
df21i %<>% mutate(SMA12 = SMA(trend, n=12)) %>% 
  mutate(MAD12 = 100*(trend - SMA12)/SMA12) %>% 
  mutate(MAD48 = 100*(trend - SMA48)/SMA48)


## MAD24
df21test <- df21i %>% select(num, MAD24) %>% mutate(signal = SMA(MAD24, 24)) %>% 
  pivot_longer(cols=-c(num), names_to="type", values_to="value")
df21test %>% head()

p <- ggplot(data=df21test)+
  theme_bw()+
  theme(panel.grid = element_blank()
        #strip.background = element_rect(fill="transparent")
  )+
  geom_line(aes(x=num, y=value, color=type))+
  geom_hline(yintercept=0, color="gray60", linetype="dashed")
p




### MAD24, MACD, RSI, STOCH の4グラフを横に並べる
df21i %<>% mutate(SMA12 = SMA(trend, n=12))
df21test <- df21i %>% select(num, trend, SMA12, MAD12, MACD.macd, RSI, stoch.fastD)

## 12 SMA
p1 <- df21test %>% select(num, trend, SMA12) %>% 
  pivot_longer(cols=-num, names_to="type", values_to="value") %>% 
  ggplot(aes(x=num, y=value, color=type))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20, color="black"),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.3),
    legend.text = element_text(size=20, color="black"),
    plot.title = element_text(size=20, color="black"),
    plot.margin = unit(c(3,8,3,3), units="mm")
  )+
  geom_line(size=1)+
  ylab("R-R interval(mS)")+
  scale_color_manual(labels = c("trend"="TREND", "SMA12"= "SMA"),
                             values = c("trend"="black", "SMA12"="red"))+
  ggtitle(label = "TREND+12SMA")
p1

## MAD12

p2 <- df21test %>% select(num, MAD12) %>% 
  ggplot(aes(x=num, y=MAD12))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20, color="black"),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size=20, color="black"),
    plot.title = element_text(size=20, color="black"),
    plot.margin = unit(c(3,8,3,3), units="mm")
  )+
  geom_line(size=1, color="blue")+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("Deviation rate(%)")+
  ggtitle(label = "MAD 12")
p2

## MACD

p3 <- df21test %>% select(num, MACD.macd) %>% 
  ggplot(aes(x=num, y=MACD.macd))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20, color="black"),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size=20, color="black"),
    plot.title = element_text(size=20, color="black"),
    plot.margin = unit(c(3,8,3,3), units="mm")
  )+
  geom_line(size=1, color="blue")+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("MACD")+
  ggtitle(label = "MACD")
p3


# RSI

p4 <- df21test %>% select(num, RSI) %>% 
  ggplot(aes(x=num, y=RSI))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20, color="black"),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size=20, color="black"),
    plot.title = element_text(size=20, color="black"),
    plot.margin = unit(c(3,8,3,3), units="mm")
  )+
  geom_line(size=1, color="blue")+
  geom_hline(yintercept=50, color="black", linetype="dashed")+
  ylab("RSI")+
  ggtitle(label = "RSI")
p4

# Stochastic

p5 <- df21test %>% select(num, stoch.fastD) %>% 
  ggplot(aes(x=num, y=100*stoch.fastD))+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size=20, color="black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=20, color="black"),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size=20, color="black"),
    plot.title = element_text(size=20, color="black"),
    plot.margin = unit(c(3,8,3,3), units="mm")
  )+
  geom_line(size=1, color="blue")+
  geom_hline(yintercept=50, color="black", linetype="dashed")+
  ylab("Stochastics")+
  ggtitle(label = "Stochastics")
p5

p1+p2+p3+p4+p5+plot_layout(ncol=1)

## MADとMACDはにているのでどちらか。
## RSIとStochasticsは似ているのでどちらか。示さないものは付表に

pS <- p1+p2+p4+plot_layout(ncol=1)

ggsave(pS, filename="h21indices.png", width=5, height=12)


###
###
save.image("RData221129.RData")
#* 今日はここまで
#* グラフは、２つ作る必要があり、元データのSTL解析のグラフと
#* インジケータのグラフ
#* 


# 221130 ------------------------------------------------------------------



setwd("C:/Users/kishis201/Desktop/ウシ分娩予測/211203分娩予測")

library(forecast)
library(magrittr)
library(tidyverse)
library(patchwork)
library(TTR)

load("Rdata221129.RData")

## df とついているものがstl解析の中身をとりだしたもの
df21 %>% head()


## making figure
## original
h021 %>% head()
df21ori <- df21 %>% cbind(h021 %>% select(Time, RR))

po <- ggplot(data=df21, aes(x=num, y = trend+seasonal+remainder))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size=20, color="black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=20, color="black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(2,4,2,2), units="mm")
  )+
  geom_line(color="black", size=1)+
  #geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("R-R")
po


## trend
pt <- ggplot(data=df21, aes(x=num, y = trend))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size=20, color="black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=20, color="black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(2,4,2,2), units="mm")
  )+
  geom_line(color="black", size=1)+
  #geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("Trend")
pt

## seasonal
ps <- ggplot(data=df21, aes(x=num, y = seasonal))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size=20, color="black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=20, color="black"),
        axis.text.x = element_blank(),
        plot.margin = unit(c(2,4,2,2), units="mm")
        )+
  geom_line(color="black", size=1)+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("Daily")
ps

## reminder
df21 %>% head()
pr <- ggplot(data=df21, aes(x=num, y = remainder))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size=20, color="black"),
        axis.title.x = element_text(size=20, color="black"),
        axis.text.y = element_text(size=20, color="black"),
        axis.text.x = element_text(size=20, color="black"),
        plot.margin = unit(c(2,4,2,2), units="mm")
  )+
  geom_line(color="black", size=1)+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  ylab("Remainder")+xlab("Time")
pr

po+pt+ps+pr+plot_layout(ncol=1)

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


df21ori %>% head()
df21ori %<>% mutate(hour = 0.5*num)


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

func_4figs(df21ori)

# できた。

save.image("RData221129.RData")


# 221201 ------------------------------------------------------------------


setwd("C:/Users/kishis201/Desktop/ウシ分娩予測/211203分娩予測")

library(forecast)
library(magrittr)
library(tidyverse)
library(patchwork)
library(TTR)

load("Rdata221129.RData")

df21 %>% head()
df45 %>% head()
df55 %>% head()
df58 %>% head()
df886 %>% head()

## h057 ? のデータを整備
h057 %>% head()
h057$RR %>% ts(frequency=48) %>% stl(s.window="per") %>% plot()

df57 <- h057$RR %>% ts(frequency=48) %>% stl(s.window="per") %>% .$time.series %>% as.data.frame()
df57 %<>% mutate(SMA24 = SMA(trend, n=24)) %>% mutate(num = 1:nrow(.))
df57 %>% head()


##　Time, RR, hourのデータを追加する

time_RR <- function(x, y){ ## x=df21, y = h021
  xnew <- x %>% cbind(y %>% select(Time, RR)) %>% 
    mutate(hour = num*0.5)
  return(xnew)
}

df21ori <- df21 %>% time_RR(h021)
df45ori <- df45 %>% time_RR(h045) 
df55ori <- df55 %>% time_RR(h055)
df57ori <- df57 %>% time_RR(h057)
df58ori <- df58 %>% time_RR(h058)
df886ori <- df886 %>% time_RR(h886)

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


## Fig. 1 OK

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## Fig. 2
## ##

## Trend+SMA, MAD, RSI
df21ori %>% head()
df21ori$trend %>% stoch() %>% head() # 3,3,14

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

indic_list[[1]] %>% head()

indic_list[[1]] %>% func_figset1()+
  geom_line(aes(x=hour, y=trend), color="black")+
  geom_line(aes(x=hour, y=SMA12), color="red")

indic_list[[1]] %>% func_figset1()+
  geom_line(aes(x=hour, y=MAD12), color="black")+
  geom_hline(yintercept=0, color="black", linetype="dashed")


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

SMAfig_func(indic_list[[1]])

MADfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=MAD12), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}

indic_list[[1]] %>% head()

MACDfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=macd), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}
indic_list[[2]] %>% MACDfig_func()

DPOfig_func <- function(x){
  p <- x %>% func_figset1()+
    geom_line(aes(x=hour, y=DPO), color="black", size=0.7)+
    geom_hline(yintercept=0, color="black", linetype="dashed", size=0.7)
  return(p)
}
indic_list[[2]] %>% DPOfig_func()

rsi_func <- function(x){
  p <- x %>% func_figset2()+
    geom_line(aes(x=hour, y=RSI), color="black", size=0.7)+
    geom_hline(yintercept=50, color="black", linetype="dashed", size=0.7)+
    ylim(c(0, 100))+xlab("Hour")
  return(p)
}

indic_list[[1]] %>% func_figset2()+
  geom_line(aes(x=hour, y=RSI), color="black", size=0.7)+
  geom_hline(yintercept=50, color="black", linetype="dashed", size=0.7)+ylim(c(0, 100))+ylab("Hour")

indic_list[[3]] %>% rsi_func()
indic_list[[3]] %>% head()
indic_list[[3]]$trend

p <- indic_list[[3]] %>% func_figset2()+
  geom_line(aes(x=hour, y=RSI), color="black", size=0.7)+
  geom_hline(yintercept=50, color="black", linetype="dashed", size=0.7)+
  ylim(c(0, 100))+xlab("Hour")


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

func_4indic_figs(indic_list[[1]])
func_4indic_figs(indic_list[[3]])


fig_list2 <- indic_list %>% lapply(function(x){
  p <- func_4indic_figs(x)
  return(p)
}
)


## ind. h021, h045, h057
g <- fig_list2[c(1,2,4)] %>% wrap_plots(nrow=1)
g
ggsave(g, filename="indic_figs.png", height=6, width=9)

g1 <- fig_list2[c(1, 2, 4, 3, 5, 6)] %>% wrap_plots(nrow=1)
g1
ggsave(g1, filename="indic_figs_all.png", height=6, width=12)


##
g2 <- fig_list2[c(3, 5, 6)] %>% wrap_plots(nrow=1)
g2
ggsave(g2, filename="indic_figs_rest.png", height=6, width=9)



save.image("RData221129.RData")

