#### carga de la base de datos####
library(dplyr)
library(tidyverse)
library(sampling)
library(purrr)
library(ggplot2)
library(sampling)
library(reshape)
library(MASS)

wine <- read.csv2('winequality-white.csv')
wine <- data.frame(apply(wine, 2, as.numeric))
set.seed(123)

###https://drinkriesling.com/tasteprofile/thescale###
wine_sweet <- wine %>% mutate (rieseling_s= residual.sugar/(fixed.acidity+citric.acid+volatile.acidity))
###se definen categorías, se elmina el dulce porque es un outsider (residual.sugar 65)###
wine_sweet <- wine_sweet %>% mutate(sweet_c = 
                                        case_when(rieseling_s < 1 ~ 'dry', 
                                                  rieseling_s < 2.1 ~ 'medium_dry',
                                                  rieseling_s <= 4 ~ 'medium_sweet')) %>% na.omit()
wine_sweet$sweet_c <- as.factor(wine_sweet$sweet_c)



#### gráficas por grupos para la variable CITRIC-ACID ####
data_gr <- wine_sweet %>% group_by(sweet_c) %>%
    summarise(counts = n()) %>% arrange(desc(sweet_c)) %>%
    mutate(prop = round(counts*100/sum(counts), 1),
           lab.ypos = cumsum(prop) - 0.5*prop)

data_mean <- wine_sweet %>% group_by(sweet_c) %>% 
    summarise(grp.mean = mean(citric.acid))

density_plot <- ggplot(wine_sweet, aes(x = citric.acid)) + 
    geom_density(aes(color = sweet_c, fill = sweet_c), alpha = 0.4) + 
    scale_color_manual(values = c("#FC4E07", "#868686FF", "#0073C2FF" ))+
    scale_fill_manual(values = c("#FC4E07", "#868686FF", "#0073C2FF" )) +
    geom_vline(aes(xintercept = grp.mean, 
                   color = sweet_c),
               data = data_mean,
               size = 1,
               linetype = "dashed")

boxplot_wine <- ggplot(wine_sweet, aes(x=factor(sweet_c), 
                                       y = citric.acid, 
                                       fill = sweet_c))+
    geom_boxplot(alpha=0.3) + scale_y_continuous(trans = 'log2') +
    theme(legend.position="none")


propo_gr <- ggplot(data_gr, aes(sweet_c, prop)) +
    geom_linerange(aes(x = sweet_c, ymin = 0, ymax = prop),
                   color = "lightgray", size = 1.5)+
    geom_point(aes(color = sweet_c), size = 2)+
    ggpubr::color_palette("jco")+
    theme_minimal()

#### deteccion de outliners ####
outliers = c()
stats = boxplot.stats(wine_sweet[,3])$stats
bottom_outlier_rows = which(wine_sweet[,3] < stats[1])
top_outlier_rows = which(wine_sweet[,3] > stats[5])
outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )

clean.wine_sweet <- wine_sweet[-outliers, ]

data_gr.clean <- clean.wine_sweet %>% group_by(sweet_c) %>%
    summarise(counts = n()) %>% arrange(desc(sweet_c)) %>%
    mutate(prop = round(counts*100/sum(counts), 1),
           lab.ypos = cumsum(prop) - 0.5*prop)

boxplot_wine.clean <- ggplot(clean.wine_sweet, aes(x=factor(sweet_c), 
                                       y = citric.acid, 
                                       fill = sweet_c))+
    geom_boxplot(alpha=0.3) + scale_y_continuous(trans = 'log2') +
    theme(legend.position="none")

density_plot.clean <- ggplot(clean.wine_sweet, aes(x = citric.acid)) + 
    geom_density(aes(color = sweet_c, fill = sweet_c), alpha = 0.4) + 
    scale_color_manual(values = c("#FC4E07", "#868686FF", "#0073C2FF" ))+
    scale_fill_manual(values = c("#FC4E07", "#868686FF", "#0073C2FF" )) +
    geom_vline(aes(xintercept = grp.mean, 
                   color = sweet_c),
               data = data_mean,
               size = 1,
               linetype = "dashed")

propo_gr.clean <- ggplot(data_gr, aes(sweet_c, prop)) +
    geom_linerange(aes(x = sweet_c, ymin = 0, ymax = prop),
                   color = "lightgray", size = 1.5)+
    geom_point(aes(color = sweet_c), size = 2)+
    ggpubr::color_palette("jco")+
    theme_minimal()


####n de cada grupo####

n_estrato <- clean.wine_sweet %>% group_by(sweet_c) %>% count()

n_ney <- round(0.05*nrow(clean.wine_sweet),0)


####Varibale Citric.Acid
sd_est_n <- clean.wine_sweet %>% 
    group_by(sweet_c) %>% 
    summarise_at(vars(citric.acid),list(Desviacion_std = sd)) %>% 
    merge(n_estrato)

n_total_est <- sd_est_n %>%
    mutate(n_sd = Desviacion_std*n) %>% 
    summarise_at(vars(n_sd), sum) %>% 
    merge(sd_est_n) %>%
    mutate(n_total = (n_ney*n*Desviacion_std)/n_sd) 

Tab_estr <- n_total_est[, 2:5]

clean.wine_sweet <- clean.wine_sweet[order(clean.wine_sweet$sweet_c),]

strata_data <- strata(clean.wine_sweet,stratanames = c("sweet_c"),
                      size = c(Tab_estr[1, 4], Tab_estr[2, 4], Tab_estr[3, 4]),
                      method = "srswor" )

sampling_ac <- getdata(clean.wine_sweet$citric.acid, strata_data)


n_strata <- sampling_ac %>% group_by(sweet_c) %>% count()

mean_est_n <- sampling_ac %>% 
    group_by(sweet_c) %>% 
    summarise_at(vars(data),list(Media = mean, Varianza = var)) %>% 
    merge(n_strata)


#media estimada
mean_est<-(1/nrow(clean.wine_sweet))*(sd_est_n[1, 3]*
                                     mean_est_n[1, 2]+
                                    sd_est_n[2, 3]*
                                     mean_est_n[2, 2]+
                                    sd_est_n[3, 3]*
                                     mean_est_n[3, 2])
Error_est<-0.05*(mean_est)

NC<-0.95

z<-qnorm((1-NC)/2, mean = 0, sd = 1)# valor de Z 

V<-(Error_est/z)^2

n_0<-((sd_est_n[1, 3]/nrow(clean.wine_sweet))*mean_est_n[1, 3]+
          (sd_est_n[2, 3]/nrow(clean.wine_sweet))*mean_est_n[2, 3]+
          (sd_est_n[3, 3]/nrow(clean.wine_sweet))*mean_est_n[3, 3])
n_0 <- n_0*(1/V)

n_inf <- n_0/(1+n_0/nrow(clean.wine_sweet))# final para la muestra

est_pilot <- data.frame(Media_estimada=mean_est, N.confianza = NC, Z_value = z, 
                        Error_estimado = Error_est, n_finita = n_0, n_infinita = n_inf)

##########################################################################
##########################################################################

e_margen <- seq(0,Error_est, by=0.0002)


V_t<-(e_margen/z)^2

n_0_fin<-((sd_est_n[1, 3]/nrow(clean.wine_sweet))*mean_est_n[1, 3]+
          (sd_est_n[2, 3]/nrow(clean.wine_sweet))*mean_est_n[2, 3]+
          (sd_est_n[3, 3]/nrow(clean.wine_sweet))*mean_est_n[3, 3])

n_0_finita <- n_0_fin*(1/V_t)

n_infinita <- n_0_finita/(1+n_0_finita/nrow(clean.wine_sweet))# final para la muestra

tabl_n_pop <- cbind(V_t, n_0_finita, n_infinita)

tabl_n_pop <- as.data.frame(tabl_n_pop)



###inserta una elemento tipo gráfica de los errores###
dat.melt <- melt(tabl_n_pop, id.vars = "V_t")
graph <- ggplot(dat.melt, aes(V_t, value, colour = variable)) + 
    geom_point(size = 1) + 
    labs(title = 
             paste('Muestreo por variable', 'citric.acid'),
         y = 'Tamaño de muestra', 
         x = 'Errores', 
         caption = "Método = muestro aleatorio simple") +
    geom_line(linetype = "dashed")

graph_citric.a  <- graph + 
    theme(plot.caption = element_text(hjust = 1, face = "italic"), 
          legend.title = element_blank(), 
          legend.position="bottom", legend.box = "horizontal") +
    scale_x_continuous(trans = 'log2') + 
    scale_y_continuous(trans = 'log2')+
    annotation_logticks(sides="lb")


##########################################################################
##########################################################################

#### varianza estimada ####

#### tabla con estimadores ####
est_fin <- mean_est_n %>% mutate(varianza_est = Tab_estr[,3]^2*((Tab_estr[,3] - Tab_estr[,4])/Tab_estr[,3])*
                                     (mean_est_n[,3]/Tab_estr[,4]))


var_estima <- (1/nrow(clean.wine_sweet))*(sum(est_fin[,5]))

des_stand <- sqrt(var_estima)

estim_mean <- data.frame(Var_estimada = var_estima, D_std.estimada = des_stand,
                         Int_Inf = mean_est-2*des_stand, Media_est = mean_est,
                         Int_Sup = mean_est+2*des_stand)

#### Estimación total ####
t_media <- nrow(clean.wine_sweet)*mean_est
t_var <- nrow(clean.wine_sweet)*var_estima

total_estim <- data.frame(var_total = t_var, Int_Inf = t_media-2*sqrt(t_var), 
                          Media_total = t_media,  
                          Int_Sup = t_media+2*sqrt(t_media))

##########################################################################
##########################################################################


