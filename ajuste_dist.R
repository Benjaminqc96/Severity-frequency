library(fitdistrplus)
library(actuar)
library(MASS)
library(ggplot2)
library(psych)
library(tidyr)
set.seed(20191103)
#lectura de la base de datos
base<-read.csv("/home/benjamin/Documentos/CASG/res_civil_sin.csv")
#bases de 2017
basena7<-subset(base,(base$MONEDA=="Nacional" & base$Año==2017))
##seleccion y limpieza de los datos
base_nacional<-subset(basena7,basena7$MONTO.DEL.SINIESTRO_PESOS>0)
datos_nacionales<-data.frame(base_nacional$NUMERO.DE.SINIESTROS,
                             base_nacional$MONTO.DEL.SINIESTRO_PESOS)
colnames(datos_nacionales)<-c("Frecuencia","Severidad")
###ajuste de frecuencia
iqr_f1<-IQR(datos_nacionales$Frecuencia)
sup1<-median(datos_nacionales$Frecuencia)+1.5*iqr_f1
inf1<-median(datos_nacionales$Frecuencia)-1.5*iqr_f1
Frecuencia_na<-subset(datos_nacionales$Frecuencia,
                      (datos_nacionales$Frecuencia<sup1 & datos_nacionales$Frecuencia>inf1))
###ajuste de severidad
iqr_nac<-IQR(datos_nacionales$Severidad)
med_nac<-median(datos_nacionales$Severidad)
lsn<-med_nac+1.5*iqr_nac
linfn<-med_nac-1.5*iqr_nac
##
Severidad_na<-datos_nacionales$Severidad[datos_nacionales$Severidad<lsn & 
                                           datos_nacionales$Severidad>linfn]
###
supfn<-median(Frecuencia_na)+1.5*IQR(Frecuencia_na)
inffn<-median(Frecuencia_na)-1.5*IQR(Frecuencia_na)
Frecuencia_na<-subset(Frecuencia_na,(Frecuencia_na<supfn & Frecuencia_na>inffn))
###
Severidad_na<-Severidad_na[Severidad_na>150000]
Frecuencia_na<-sample(Frecuencia_na,length(Severidad_na))
##############################distribuciones de frecuencia elegidas####################################
#poisson
#binomial negativa
#geometrica
ajus_poiss<-fitdistr(Frecuencia_na,"poisson")
ajus_binne<-fitdistr(Frecuencia_na,"negative binomial")
ajus_geome<-fitdistr(Frecuencia_na,"geometric")
###generacion de muestras aleatorias
muestra_poi<-rpois(length(Frecuencia_na),ajus_poiss$estimate[1])
muestra_bnn<-rnbinom(n = length(Frecuencia_na),ajus_binne$estimate[1],mu = ajus_binne$estimate[2])
muestra_geo<-rgeom(length(Frecuencia_na),ajus_geome$estimate)
###bondad de ajuste
chisq.test(muestra_poi,Frecuencia_na)# p-value = 0.484
chisq.test(muestra_bnn,Frecuencia_na)#p-value = 0.7512#mejor distribucion de las 3
chisq.test(muestra_geo,Frecuencia_na)#p-value = 0.01955#distribucion elegida con fines no estadisticos
###############################distribuciones de severidad elegidas####################################
#log-normal
#gamma
#weibull
#pareto
ajus_lnor<-fitdist(Severidad_na,"lnorm","mle")
ajus_gamm<-fitdist(Severidad_na,"gamma","mle",start = list(shape=1,scale=500))
ajus_weib<-fitdist(Severidad_na,"weibull","mle")
ajus_pare<-fitdist(Severidad_na,"pareto","mle",start = list(shape=1,scale=500))
###generacion de muestras aleatorias
muestra_logn<-rlnorm(length(Severidad_na),meanlog = ajus_lnor$estimate[1], sdlog = ajus_lnor$estimate[2])
muestra_gamma<-rgamma(length(Severidad_na),shape = ajus_gamm$estimate[1],scale = ajus_gamm$estimate[2]) 
muestra_weib<-rweibull(length(Severidad_na),shape = ajus_weib$estimate[1], scale = ajus_weib$estimate[2])
muestra_pare<-rpareto(length(Severidad_na),shape = ajus_pare$estimate[1],scale = ajus_pare$estimate[2])
###bondad de ajuste
ks.test(Severidad_na,muestra_logn)#p-value = 0.1931#
ks.test(Severidad_na,muestra_gamma)#p-value = 0.001126
ks.test(Severidad_na,muestra_weib)#p-value = 0.00214
ks.test(Severidad_na,muestra_pare)#p-value < 2.2e-16
########################################estadistica descriptiva######################################
ed_frecuencia_o<-describe(Frecuencia_na)
ed_frecuencia_e<-describe(muestra_geo)
ed_sev_obs<-describe(Severidad_na)
ed_sev_est<-describe(muestra_logn)
estadistica_descriptiva<-as.data.frame(cbind(t(ed_frecuencia_o),t(ed_frecuencia_e),
                                             t(ed_sev_obs),t(ed_sev_est)))
colnames(estadistica_descriptiva)<-c("Frecuencia","Frecuencia estimada","Severidad","Severidad estimada")
estadistica_descriptiva<-estadistica_descriptiva[-1,]
estadistica_descriptiva$Severidad<-as.character(estadistica_descriptiva$Severidad)
estadistica_descriptiva$`Severidad estimada`<-as.character(estadistica_descriptiva$`Severidad estimada`)
###############################################graficos##############################################
datos_fin<-data.frame(Frecuencia_na,Severidad_na,muestra_geo,muestra_logn)
colnames(datos_fin)<-c("Frecuencia","Severidad","Frec_estim","Sev_estim")
###frecuencia observada
graf1<-ggplot(datos_fin,aes(datos_fin$Frecuencia))+geom_histogram(color = "darkblue",
                                                                  fill = "lightblue")+
labs(title = "Frecuencia observada",caption = "Elaboracion propia con datos de la CNSF")+
  xlab("Num de siniestros")+ylab("Frecuencia")+theme_bw()+
  theme(plot.caption = element_text(hjust = -.02,color = "blue", face = "italic"),
        plot.title = element_text(hjust = 0.5) )
###frecuencia estimada
graf2<-ggplot(datos_fin,aes(datos_fin$Frec_estim))+geom_histogram(color = "black",fill = "red")+
  labs(title = "Frecuencia estimada",
       caption = "Elaboracion propia con datos de la CNSF")+
  xlab("Num de siniestros")+ylab("Frecuencia")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02,color = "blue", face = "italic"),
                   plot.title = element_text(hjust = 0.5))

###Severidad observada
graf3<-ggplot(datos_fin,aes(datos_fin$Severidad))+geom_histogram(color = "darkblue",fill = "lightblue")+
  labs(title = "Severidad observada",
       caption = "Elaboracion propia con datos de la CNSF")+
  xlab("Monto")+ylab("Frecuencia")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02,color = "blue", face = "italic"),
                   plot.title = element_text(hjust = 0.5))

###severidad estimada
graf4<-ggplot(datos_fin,aes(datos_fin$Sev_estim))+geom_histogram(color = "black",fill = "red")+
  labs(title = "Severidad estimada",
       caption = "Elaboracion propia con datos de la CNSF")+
  xlab("Monto")+ylab("Frecuencia")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02,color = "blue", face = "italic"),
                   plot.title = element_text(hjust = 0.5))+
  xlim(120000,390000)
###grafico de finciones acumuladas
datos_sev<-data.frame(Severidad_na,muestra_gamma,muestra_logn,muestra_weib,muestra_pare)
colnames(datos_sev)<-c("Severidad observada","Gamma","Log-normal","Weibull","Pareto")
datos_sev<-gather(datos_sev)
colnames(datos_sev)<-c("Distribucion","value")
graf5<-ggplot(datos_sev)+stat_ecdf(aes(datos_sev$value,col=datos_sev$Distribucion))+
  labs(title = "Severidad estimada",
       caption = "Elaboracion propia con datos de la CNSF",col="Distribución")+
  xlab("Importe Severidad")+ylab("Probabilidad acumulada")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02,color = "blue", face = "italic"),
                   plot.title = element_text(hjust = 0.5))+
  xlim(150000,400000)
###graficos en formato bmp
ggsave("/home/benjamin/Imágenes/Frecuencia observada.bmp",plot = graf1,
       width = 5,height = 5,units = "in")

ggsave("/home/benjamin/Imágenes/Frecuencia estimada.bmp",plot = graf2,
       width = 5,height = 5,units = "in")

ggsave("/home/benjamin/Imágenes/Severidad observada.bmp",plot = graf3,
       width = 5,height = 5,units = "in")

ggsave("/home/benjamin/Imágenes/Severidad estimada.bmp",plot = graf4,
       width = 5,height = 5,units = "in")

ggsave("/home/benjamin/Imágenes/Severidad acumulada.bmp",plot = graf5,
       width = 5,height = 5,units = "in")
###supuestos para el calculo de perdidas
#p0 = (1+b)^-1 =0.3238994
#frecuencia teorica
mft<-(1-ajus_geome$estimate)/ajus_geome$estimate
vft<-mft*(1+mft)
#frecuencia estimada
mfe<-mean(datos_fin$Frec_estim)
vfe<-var(datos_fin$Frec_estim)
#severidad teorica
mst<-exp(ajus_lnor$estimate[1]+.5*ajus_lnor$estimate[2]^2)
e2_sev<-exp(2*ajus_lnor$estimate[1]+2*ajus_lnor$estimate[2]^2)
vst<-e2_sev-mst^2
#severidad estimada
mse<-mean(datos_fin$Sev_estim)
vse<-var(datos_fin$Sev_estim)
#############################caso normal#################################
bell_norm<-mft*mst
var_norm<-mft*vst+vft*mst^2
pne_nor<-sqrt(var_norm)*qnorm(.995)
#############################caso cero truncado#########################
#p0 = (1+b)^-1 =0.3238994
med_trun<-mft/(1-0.3238994)
e2_run<-(vft+mft)/(1-0.3238994)
var_trun<-e2_run-med_trun^2
bell_trun<-med_trun*mst
vc_trun<-med_trun*vst+var_trun*mst^2
pne_trun<-sqrt(vc_trun)*qnorm(0.995)
###datos en formato csv
write.csv(datos_fin,"/home/benjamin/Documentos/CASG/datos_est.csv")
############################################  fin del codigo####################################
