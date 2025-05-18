install.packages('FactoMineR')
install.packages('factoextra')
install.packages('ggplot2')
library(ggplot2)
library(FactoMineR)
```{r setup, include=FALSE}
#librerias
library(readxl)
library(FactoMineR)
#base de datos
ciudades <- ciudades_original_filtrado_con_etiquetas
summary(ciudades)
str(ciudades)
View(ciudades)
```


```{r setup, include=FALSE}
#base de datos RH+INFRA
ciudadest<-ciudades[,c(2:15,21:30)]
str(ciudadest)
View(ciudadest)

?PCA

#ACP todas las variables que le corresponden
acp1<-PCA(ciudadest)

#contiene los auto valores correspondientes a los componentes principales
s1<-acp1$eig
s1

#al sumar la columna de los eigenvalue da 24
sum(s1[,1])

#la coluna de porcentages de varianza acumulados
#nos muestra que tan importantes son los primeros componentes
# y es notable como con los primeros 7 ya se acumula
# el 83 porciento de la varianza
s1[,3]

#correlaciones variable factor
acp1$var$cor
#RH13 a Dimension 4
#no se como interpretar esto




#ACP variables con ilustrativas RH####
acp2<-PCA(ciudadest,quanti.sup = c(1:14))
#Solo las variables INFRA_25 a INFRA_38
#(columnas 15 a 24) serán activas.
#Las variables RH_* (1 a 14) no se usan para
#construir los ejes principales, pero se proyectan
#en el plano factorial para ser interpretadas.
s2<-acp2$eig
sum(s2[,1])

#ACP RH con ilustrativas variables####
acp3<-PCA(ciudadest,quanti.sup = c(15:24))
#Esto hace lo opuesto: trata las variables de
#infraestructura (15 a 24) como suplementarias y
#las variables RH (1 a 14) como activas.
acp3$eig





# Cargar librerías
library(FactoMineR)
library(factoextra)


#  Gráfico combinado: activas + suplementarias
fviz_pca_var(acp3,
             col.var = "darkgreen", 
             alpha.var = 0.5,
             select.var = list(active = 1:10, sup = 1:14),
             repel = TRUE
)

#  Gráfico de individuos 
fviz_pca_ind(acp3,
             col.ind = "cos2",  # calidad de representación
             gradient.cols = c("darkgreen", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# 6. Biplot (variables + individuos)
Biplot2<-fviz_pca_biplot(acp3,
                col.ind = "red",
                col.var = "darkgreen",
                repel = TRUE
)



G21<-fviz_pca_ind(acp2,
                  col.ind = "cos2",  # calidad de representación
                  gradient.cols = c("darkgreen", "#E7B800", "#FC4E07"),
                  repel = TRUE
)
G21

G22<-fviz_pca_var(acp2,
                  col.var = 'red',
                  alpha.var = 0.5,
                  select.var = list(active = 1:10, sup = 1:14),
                  repel = TRUE,
                  col.quanti.sup = 'darkgrey'
)
G22

G23<-fviz_pca_biplot(acp2,
                     col.ind = "red",
                     col.var = "darkblue",
                     repel = T,
                     col.quanti.sup = 'darkgrey'
)
G23


