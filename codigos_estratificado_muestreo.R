#### carga de la base de datos####
library(dplyr)
library(tidyverse)
library(sampling)
library(purrr)
library(ggplot2)

wine <- read.csv2('winequality-white.csv')
wine <- data.frame(apply(wine, 2, as.numeric))
set.seed(123)
N <- 4897
np <- round(0.05*N,0)
np

n1<-np/3

###https://drinkriesling.com/tasteprofile/thescale###
wine_sweet <- wine %>% mutate (rieseling_s= residual.sugar/(fixed.acidity+citric.acid+volatile.acidity))
###se definen categorías, se elmina el dulce porque es un outsider (residual.sugar 65)###
wine_sweet <- wine_sweet %>% mutate(sweet_c = 
                                        case_when(rieseling_s < 1 ~ 'dry', 
                                                  rieseling_s < 2.1 ~ 'medium_dry',
                                                  rieseling_s <= 4 ~ 'medium_sweet')) %>% na.omit()
wine_sweet$sweet_c <- as.factor(wine_sweet$sweet_c)


x <- wine_sweet %>% group_by(sweet_c) %>%
    sample_n(n1, replace = FALSE)

#xt <- x %>% summarise_at(.vars = names(.)[1:12],.funs = c(mean)) %>%
    #gather(Variable, valname, -sweet_c) %>% spread(sweet_c, valname)

nom_sweet <- c('Seco', 'Semi_seco', 'Semi_dulce')
x_mean <- x %>% summarise_at(.vars = names(.)[1:12],.funs = c(mean))
x_mean <- as.data.frame(t(x_mean[, -1]))
colnames(x_mean) <- nom_sweet

x_sd <- x %>% summarise_at(.vars = names(.)[1:12],.funs = c(sd))
x_sd <- as.data.frame(t(x_sd[, -1]))
colnames(x_sd) <- nom_sweet

coef_var <- x_sd/x_mean

table_est <- cbind(x_mean, x_sd, coef_var)### tabla para gráficar

##########################medias ponderadas##################################
med_pond <- wine_sweet %>% group_by(sweet_c) %>% count() %>% 
    mutate(N = N, media_ponderada = n/N) %>% as.data.frame() ###tabla medias ponderadas
med_pond <- wine_sweet %>% group_by(sweet_c) %>% count() %>% 
    mutate(N = N, media_ponderada = n/N) %>% as.data.frame() ###tabla medias ponderadas

med_pond_1 <- med_pond[,-1]
row.names(med_pond_1) <- nom_sweet

#############################################################################
y_1 <-med_pond %>% 
    select(sweet_c, media_ponderada) %>% 
    spread(sweet_c, media_ponderada) ### organizacion de tabla para merge

medp_str<- x_mean %>% merge(y_1) %>% 
    mutate(dry = dry*Seco, 
           medium_dry = medium_dry*Semi_seco, 
           medium_sweet = medium_sweet*Semi_dulce) %>%
    mutate(media_estratificada = dry+medium_dry+medium_sweet) %>%
    mutate(error = media_estratificada*0.05)


nom_sweet_medp <- c('Seco', 'Semi_seco', 
                    'Semi_dulce', 'Seco', 
                    'Semi_seco', 'Semi_dulce', 
                    'Media estratificada',
                    'Error')
rownames(medp_str) <- rownames(x_mean)
colnames(medp_str) <- nom_sweet_medp

#######################################################################
########sd_ponderada#########
sd_ponder_val<- x_sd %>% merge(y_1) %>% cbind(medp_str)

#### función para muestreo población finita e infinita ####

inf_finite_estr <- function(N, sd_ponder_data, conf) {
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    V <- (sd_ponder_data[,14]/Z)^2
    P_infinite <- (sd_ponder_data[,4]*sd_ponder_data[,1]^2+
                       sd_ponder_data[,5]*sd_ponder_data[,2]^2+
                       sd_ponder_data[,6]*sd_ponder_data[,3]^2)/V
    P_finite <- P_infinite/(1+(P_infinite/N))
    Seco <- P_finite*sd_ponder_data[,4]
    Semi_seco <- P_finite*sd_ponder_data[,5]
    Semi_dulce <- P_finite*sd_ponder_data[,6]
    n_final <- Seco+Semi_seco+Semi_dulce
    Error <- sd_ponder_data[,14]
    tabl_e_s <- data.frame(Error, P_infinite, P_finite, 
                           Seco, Semi_seco, Semi_dulce, n_final)
    rownames(tabl_e_s) <- rownames(sd_ponder_data)
    print(tabl_e_s)
}



var_all <- inf_finite_estr(4897,  sd_ponder_val, 0.95)

###########################################################################
###########################################################################

error_lst_estra <- function(data_est) {
    mar_1 <- seq(0,data_est[1, 8]+0.01,by=0.01)
    mar_2 <- seq(0,data_est[2, 8]+0.01, by=0.00065)
    mar_3 <- seq(0,data_est[3, 8], by=0.0005)
    mar_4 <- seq(0,data_est[4, 8]+0.01, by=0.0075)
    mar_5 <- seq(0,data_est[5, 8]+0.01, by=0.00035)
    mar_6 <- seq(0,data_est[6, 8]+0.001, by=0.05)
    mar_7 <- seq(0,data_est[7, 8], by=0.2)
    mar_8 <- seq(0,data_est[8, 8]+0.01, by=0.0015)
    mar_9 <- seq(0,data_est[9, 8], by=0.004)
    mar_10 <- seq(0,data_est[10, 8], by=0.0007)
    mar_11 <- seq(0,data_est[11, 8], by=0.015)
    mar_12 <- seq(0,data_est[12, 8], by=0.0075)
    f_list <- list(mar_1, mar_2, mar_3, mar_4, mar_5, mar_6, 
                   mar_7, mar_8, mar_9, mar_10, mar_11, mar_12)
    n.obs <- sapply(f_list, length)
    seq.max <- seq_len(max(n.obs))
    meta <- data.frame(sapply(f_list, "[", i = seq.max))
    colnames(meta) <- rownames(data_est)
    print(meta)
}

e_estra<- error_lst_estra(medp_str)

inf_fin_extra<- function(N, sd_ponder_data, data_error, varnum, conf) {
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    Error <- na.omit((data_error[,varnum]/Z)^2)
    P_infinite <- (sd_ponder_data[,4]*sd_ponder_data[,1]^2+
                       sd_ponder_data[,5]*sd_ponder_data[,2]^2+
                       sd_ponder_data[,6]*sd_ponder_data[,3]^2)/Error
    P_finite <- P_infinite/(1+(P_infinite/N))
    tabl_e_s <- data.frame(Error, P_infinite, P_finite)
    print(tabl_e_s)
}

inf_fin_extra(4897, sd_ponder_val, e_estra, 1, 0.95)


