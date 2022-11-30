library(ggplot2)
library(dplyr)
library(mosaic)
options(scipen = 100, digits=4)

## Champions League
# Preparación datos
players <- read.csv("uefa_champions_league_players_1213.csv", sep=";", header = TRUE)
players <- players %>% mutate(db=as.Date(db),mb=format(db,"%m"))
# Distribución datos 
table(players$mb)
ggplot(players) + geom_bar(aes(mb),color="yellowgreen")
# Calculo del porcentaje de nacidos por semestre
players <- players %>% mutate(sb=ifelse(as.numeric(mb)<=6,"S1","S2"))
(t=table(players$sb))
prop.table(t)

## Test de hipótesis
(p_valor <- 1- pbinom(608,size=977,prob=0.5))
prop.test(608,977,p=0.5) # test de proporciones

## Análisis por posición en el campo
( by_sem_pos<-players %>% group_by(ps) %>% 
    summarise(num=n(),n_s1=length(which(sb=="S1")),p_s1=n_s1/num) )
# p-valores
by_sem_pos %>% summarise(pvalor=1-pbinom(.$n_s1,.$num,prob = 0.5)) %>% 
  cbind(by_sem_pos,.)

## Datos del Mundial de Fútbol 2014
fifawc <- read.csv("wordCup2014_ Alle_Spieler_all players.csv",sep=",",header=TRUE)
# obtención semestre de nacimiento
fifawc <- fifawc %>% mutate(sb=ifelse(Monat<=6,"S1","S2"))
head(fifawc)
#  nacimientos por semestre en función de la posición en el campo
( wc_pos<- fifawc %>% group_by(Position) %>%
    summarise(num=n(),n_s1=length(which(sb=="S1")), p_s1=n_s1/num) )
# p-valores
wc_pos %>% summarise(pvalor=1-pbinom(.$n_s1,size=.$num,prob = 0.5)) %>%
  cbind(wc_pos,.)
# OBSERVACIÓN: En el caso de jugadores del mundial, la proporción solo es significativamente mayor del 50%
# para porteros y centrocampistas