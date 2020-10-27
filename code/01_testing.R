#Clear memory
rm(list = ls())

#Clear the console
cat("\014")

#devtools::install_github("hrbrmstr/firasans")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(designmatch)
library(hrbrthemes)
library(firasans)
library(augsynth)
library(magrittr)

#Turn off scientific notation (turn back on with 0)
set.seed(100)

### Load data (Linea 213 carga los datos que estan limpios que esta en el repositorio)

dir_data = "C:/Users/mc72574/Dropbox/covid/participacion/"


######## Data participacion (bajada del servel)

d2012 = read.csv(paste0(dir_data,"data_original/VW_VOTARON_2012.csv"), sep = ";", header = TRUE)
d2013 = read.csv(paste0(dir_data,"data_original/PARTICIPACION_2013_1V.csv"), sep = ";", header = TRUE, encoding = "UTF-8")
d2016 = read.csv(paste0(dir_data,"data_original/PARTICIPACION_2016.csv"), sep = ";", header = TRUE, encoding = "UTF-8")
d2017 = read.csv(paste0(dir_data,"data_original/VW_VOTARON_2017_1V.csv"), sep = ";", header = TRUE)
d2020_padron = read.csv(paste0(dir_data,"data_original/PADRON2020.csv"), sep = ";", header = TRUE, encoding = "UTF-8")
d2020_comuna = read.csv(paste0(dir_data,"data_original/votacion2020.csv"), header = TRUE)

###########################

####### Data etapas COVID (https://docs.google.com/spreadsheets/d/1WieweYNSPdpmjUIyYcbKp1oaqwlnD61_/edit?usp=drive_web&ouid=117681751153105889471&dls=true)

etapas = read.csv(paste0(dir_data,"data_original/CUT_CUARENTENAS_COVID_24_25_Oct.csv"))

############################

####### Data CASEN 2017

casen = read.csv("https://raw.githubusercontent.com/maibennett/participacion/main/data/casen2017_clean.csv")

############################

# Crear variables para rangos etarios discretizados (joven <35, medio 35-64, mayor 65+)
rango_edad_joven = sort(unique(d2012$RANGO_EDAD))[1:4]
rango_edad_medio = sort(unique(d2012$RANGO_EDAD))[5:10]
rango_edad_mayor = sort(unique(d2012$RANGO_EDAD))[11:14]

####### Arreglar formato datos

# 2012
d2012 = d2012 %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))


d2012_comuna = d2012 %>% group_by(COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))

d2012_comuna$participacion = d2012_comuna$vote/d2012_comuna$n

d2012_comuna = d2012_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = c(n, vote, participacion))

d2012_comuna = d2012_comuna %>% mutate(n = n_1+n_2+n_3,
                                       vote = vote_1+vote_2+vote_3,
                                       participacion = vote/n)

d2012_comuna$year = 2012

# 2013

d2013 = d2013 %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))


d2013_comuna = d2013 %>% group_by(COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))

d2013_comuna$participacion = d2013_comuna$vote/d2013_comuna$n


d2013_comuna = d2013_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = c(n, vote, participacion))

d2013_comuna = d2013_comuna %>% mutate(n = n_1+n_2+n_3,
                                       vote = vote_1+vote_2+vote_3,
                                       participacion = vote/n)
  
d2013_comuna$year = 2013

# 2016

d2016 = d2016 %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))


d2016_comuna = d2016 %>% group_by(COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))

d2016_comuna$participacion = d2016_comuna$vote/d2016_comuna$n

d2016_comuna = d2016_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = c(n, vote, participacion))

d2016_comuna = d2016_comuna %>% mutate(n = n_1+n_2+n_3,
                                       vote = vote_1+vote_2+vote_3,
                                       participacion = vote/n)

d2016_comuna$year = 2016

# 2017

d2017 = d2017 %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))

# Elimino voto en el exterior:
d2017 = d2017[d2017$COMUNA!="",]

d2017_comuna = d2017 %>% group_by(COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(SUFRAGIO)), vote = sum(SUFRAGIO == "sufragó"))

d2017_comuna$participacion = d2017_comuna$vote/d2017_comuna$n

d2017_comuna = d2017_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = c(n, vote, participacion))

d2017_comuna = d2017_comuna %>% mutate(n = n_1+n_2+n_3,
                                       vote = vote_1+vote_2+vote_3,
                                       participacion = vote/n)


d2017_comuna$year = 2017

# 2020

d2020_padron = d2020_padron %>% 
  mutate(rango_edad3 = ifelse(RANGO_EDAD %in% rango_edad_joven,1,
                              ifelse(RANGO_EDAD %in% rango_edad_medio,2,
                                     ifelse(RANGO_EDAD %in% rango_edad_mayor,3,NA))))

d2020_padron = d2020_padron[d2020_padron$COMUNA!="",]

d2020_padron_comuna = d2020_padron %>% group_by(COMUNA,rango_edad3) %>% 
  summarise(n = sum(!is.na(rango_edad3)))


d2020_padron_comuna = d2020_padron_comuna %>% pivot_wider(id_cols = c(COMUNA, rango_edad3),
                                            names_from = rango_edad3,
                                            values_from = n)

names(d2020_padron_comuna) = c("COMUNA","n_1","n_2","n_3")

# Cambiar comunas que tienen distinto nombre:
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Aysen"] = "Aisen"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Cabo De Hornos(Ex-Navarino)"] = "Cabo De Hornos"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Paiguano"] = "Paihuano"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Trehuaco"] = "Treguaco"
d2020_padron_comuna$COMUNA[d2020_padron_comuna$COMUNA=="Llaillay"] = "Llay-Llay"

# Votacion 2020

names(d2020_comuna) = c("COMUNA","n_mesas","n","vote","participacion")
d2020_comuna = d2020_comuna[,c("COMUNA","n","vote","participacion")]

d2020_comuna$year = 2020

d2020_comuna = left_join(d2020_comuna,d2020_padron_comuna, by="COMUNA")

# No tenemos datos de votacion por grupo etario por grupo etario, asi que incluyo 0s

d2020_comuna = d2020_comuna %>% mutate(vote_1 = 0, vote_2 = 0, vote_3 = 0,
                                       participacion_1 = 0, participacion_2 = 0,
                                       participacion_3 = 0) %>%
  relocate(COMUNA,n_1,n_2,n_3,vote_1,vote_2,vote_3,participacion_1,participacion_2,
           participacion_3, year)


# Append all

d = data.frame(rbind(d2012_comuna,
                     d2013_comuna,
                     d2016_comuna,
                     d2017_comuna,
                     d2020_comuna))

# Etapas COVID
etapas$group_etapa_plebiscito = NA
etapas$group_etapa_plebiscito[etapas$X24.Oct<=2] = 1
etapas$group_etapa_plebiscito[etapas$X24.Oct>2] = 0


d = left_join(d, etapas[,c("COMUNA","CUT","X24.Oct","group_etapa_plebiscito")], by = "COMUNA")

d = d %>% rename(cod_comuna = CUT)

d = left_join(d, casen, by="cod_comuna")

save(d,file=paste0(dir_data,"repositorio/participacion/data/test_workspace.Rdata"))
write.csv(d, file = paste0(dir_data,"repositorio/participacion/data/d2012-2020_clean.csv"))

############################################################################
##### Analysis

d = read.csv("https://raw.githubusercontent.com/maibennett/participacion/main/data/d2012-2020_clean.csv")

# Efecto COVID ASCM

d$treated = as.numeric(d$year==2020 & d$X24.Oct<=1)

# Drop comunas that don't have CASEN data:
d = d[!is.na(d$yautcorh),]

syn <- augsynth(participacion ~ treated | participacion_1 + participacion_2
                + participacion_3 + n + yautcorh + hacinamiento_2, COMUNA, year, 
                d[d$X24.Oct==1 | d$X24.Oct==3 | d$X24.Oct==2,],
                progfunc = "Ridge",scm=T,fixedeff = T)


summary(syn)

plot(syn)

# Efecto edad
d$adulto_mayor = d$n_3/d$n
d$treated = as.numeric(d$adulto_mayor>=0.2 & d$year>=2020)

comunas_selected = unique(d$COMUNA[(d$adulto_mayor>0.21 | d$adulto_mayor<=0.17) & d$year==2020])

d_subset = d[d$COMUNA %in% comunas_selected,]

syn <- augsynth(participacion ~ treated | participacion_1 + participacion_2
                + participacion_3 + n + factor(X24.Oct) + decil1 + decil2 + decil9 + decil10 + hacinamiento_2, 
                COMUNA, year, d,
                progfunc = "Ridge",scm=T,fixedeff = T)

summary(syn)

plot(syn)





######################################################################
# Diff-in-diff

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Etapa

d_sum <- summarySE(d, measurevar="participacion", groupvars=c("group_etapa_plebiscito","year"))

pos <- position_dodge(.5)


ggplot(d_sum, aes(x=year, y=participacion, colour=factor(group_etapa_plebiscito),
                  fill=factor(group_etapa_plebiscito)), fill="white") + 
  geom_errorbar(aes(ymin = participacion-se, ymax=participacion+se), width=.1, position = pos) +
  geom_line(position = pos) +
  geom_point(size=4,pch=21,position = pos) +
  geom_vline(aes(xintercept = 2019, color = area), lty = 2, col = "dark grey", lwd = 1) + 
  scale_color_manual(labels = c("Etapa 3-4","Etapa 1-2"),values=c("#900DA4","#FCCE25")) + 
  scale_fill_manual(labels = c("Etapa 3-4","Etapa 1-2"),values=c(alpha("#900DA4",0.5),
                             alpha("#FCCE25",0.5))) +
  xlab("Año") + ylab("Participación")+
  ggtitle("Participación electoral según etapas de comunas el 24/10/2020")+
  
  #Estos son solo arreglos estéticos (si es que no tienes algunos de estos paquetes,
  # puedes correr todo hasta esta linea no mas, borrando el `+` de la linea 100)
  theme_bw()+
  theme_ipsum_fsc(plot_title_face = "bold") + #plain 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  
  theme(axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45,
                                   hjust=1,vjust=1,margin=margin(0.1,0,0,0)),
        axis.title.y = element_text(size=16),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=10),legend.position=c(0.1,0.12),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="white"))


# Edad votantes

comunas_mayores = unique(d$COMUNA[d$adulto_mayor>=0.2 & d$year==2020])

d$comunas_mayores = 0
d$comunas_mayores[d$COMUNA %in% comunas_mayores] = 1 

d_sum <- summarySE(d, measurevar="participacion", groupvars=c("comunas_mayores","year"))

pos <- position_dodge(.2)


ggplot(d_sum, aes(x=year, y=participacion, colour=factor(comunas_mayores),
                  fill=factor(comunas_mayores)), fill="white") + 
  geom_errorbar(aes(ymin = participacion-se, ymax=participacion+se), width=.1, position = pos) +
  geom_line(position = pos) +
  geom_point(size=4,pch=21,position = pos) +
  geom_vline(aes(xintercept = 2019, color = area), lty = 2, col = "dark grey", lwd = 1) + 
  scale_color_manual(labels = c("AM < 0.2","AM > 0.2"),values=c("#900DA4","#FCCE25")) + 
  scale_fill_manual(labels = c("AM < 0.2","AM > 0.2"),values=c(alpha("#900DA4",0.5),
                                                                 alpha("#FCCE25",0.5))) +
  xlab("Año") + ylab("Participación")+
  ggtitle("Participación electoral según etapas de comunas el 24/10/2020")+
  
  #Estos son solo arreglos estéticos (si es que no tienes algunos de estos paquetes,
  # puedes correr todo hasta esta linea no mas, borrando el `+` de la linea 100)
  theme_bw()+
  theme_ipsum_fsc(plot_title_face = "bold") + #plain 
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  
  theme(axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=10, angle = 45,
                                   hjust=1,vjust=1,margin=margin(0.1,0,0,0)),
        axis.title.y = element_text(size=16),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=10),legend.position=c(0.1,0.12),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.background = element_rect(fill="white",colour ="white"))
