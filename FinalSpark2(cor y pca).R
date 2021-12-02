install.packages('corrplot')
library(corrplot)
install.packages("dplyr")
library(dplyr)
install.packages("HSAUR2")
library(HSAUR2)


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


# Se obtiene un resumen para analizar más detalles
summary(data_pca)
#Se obtienen los pesos de la 1a. componente principal
a1 <- data_pca$rotation[,1]
a1
# Se requiere reescalar apropiadamente los datos para calcular la primera  componente principal
#Se obtiene el centro y la escala disponible en la función prcomp
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
#La correlación entre la puntuación final de cada atleta y la primera  componente principal se calcula como
cor(select_var$heart_disease, data_pca$x[,1])
#Finalmente, el diagrama de dispersión de la puntuación oficial dada a los  atletas y la 1a. Componente principal
plot(select_var$heart_disease, data_pca$x[,1])

pca_variables <- data.frame(data_pca$x[,1],data_pca$x[,2],data_pca$x[,3],data_pca$x[,4],data_pca$x[,5],data_pca$x[,6],data_pca$x[,7],data_pca$x[,8],data_pca$x[,9])
