install.packages('corrplot')
library(corrplot)
install.packages("dplyr")
library(dplyr)
install.packages("HSAUR2")
library(HSAUR2)
install.packages("sparklyr")
library(sparklyr)
install.packages("ggplot")
library(ggplot2)
#con esta versi髇 si pude hacer la conecci髇
spark_install("2.3.1")

sc<-spark_connect(master="local", version="2.3.1")

data <- read.csv("healt_data.csv")

data <- data[!(data$gender=="Other"),]
data$gender <- as.numeric(data$gender)

#correlacion de variables
select_var <- data.frame(data$gender,data$age,data$hypertension,data$ever_married,data$work_type, data$Residence_type,data$avg_glucose_level, data$bmi,data$smoking_status,data$heart_disease)
corrplot(cor(select_var), method = 'square',addCoef.col = 'black', number.cex = 0.5, tl.col = 'black', tl.srt = 90, tl.cex = 0.5 )

#pca
score <- which(colnames(select_var) == "data.heart_disease")
score
data_pca <- prcomp(select_var[, -score],scale = TRUE)
data_pca


# Se obtiene un resumen para analizar m谩s detalles
summary(data_pca)
#Se obtienen los pesos de la 1a. componente principal
a1 <- data_pca$rotation[,1]
a1
# Se requiere reescalar apropiadamente los datos para calcular la primera  componente principal
#Se obtiene el centro y la escala disponible en la funci贸n prcomp
center <- data_pca$center
center
scale <- data_pca$scale
scale
#Para calcular los valores de 1a. componente principal para cada atleta
#Se aplica la funcion scale y se multiplican los datos de la 1a.  componente
hm <- as.matrix(select_var[,-score])
drop(scale(hm, center = center, scale =scale) 
     %*%data_pca$rotation[,1])
#Se pueden extraer las coordenadas de los individuos respecto al primer  eje de la matriz que contiene todas las coordenadas de los individuos
predict(data_pca)[,1]
#Diagrama de barras de la varianza explicada por las componentes  principales
plot(data_pca)
# Se grafican los 24 atletas sobre los 2 primeros ejes principales con  Biplot
biplot(data_pca, col = c("red", "blue"))
#La correlaci贸n entre la puntuaci贸n final de cada atleta y la primera  componente principal se calcula como
cor(select_var$data.heart_disease, data_pca$x[,1])
#Finalmente, el diagrama de dispersi贸n de la puntuaci贸n oficial dada a los  atletas y la 1a. Componente principal
plot(select_var$data.heart_disease, data_pca$x[,1])

pca_variables <- data.frame(data_pca$x[,1],data_pca$x[,2],data_pca$x[,3],data_pca$x[,4],data_pca$x[,5],data_pca$x[,6],data_pca$x[,7],data_pca$x[,8],data_pca$x[,9])

#LOGISTIC REGRESSION
data_spark <- sdf_copy_to(sc = sc, x=health_data, overwrite= T)

start_time <- Sys.time()
regression_model <- ml_logistic_regression(x=data_spark, response = "heart_disease", features = c("age", "avg_glucose_level", "hypertension"))
end_time <- Sys.time()
execution_time <- end_time - start_time

roc <- validation_summary$roc() %>%
  collect()
ggplot(roc, aes(x = FPR, y = TPR)) + geom_line() + geom_abline(lty = "dashed")

#RANDOM FOREST
select_var <- sdf_copy_to(sc, health_data, overwrite = TRUE)
select_var

start_time <- Sys.time()
rf_model <- ml_random_forest(select_var, response = "heart_disease", features = c("age", "avg_glucose_level", "hypertension"), type = "classification")
end_time <- Sys.time()
execution_time <- end_time - start_time

rf_predict <- ml_predict(rf_model, select_var) %>% ft_string_indexer("heart_disease") %>% collect
table(rf_predict$heart_disease, rf_predict$prediction)
