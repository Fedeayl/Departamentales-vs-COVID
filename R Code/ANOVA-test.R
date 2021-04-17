###### Test ANOVA ######



### Importo datos de circuito 

DataDep <- rio::import(here::here("Data","Subnacionales 2020 - Total GENERAL por circuito.xlsx"))
DataNac <- rio::import(here::here("Data", "Nacional 2019 - Total GENERAL por circuito.xlsx"))
EdadMed <- rio::import(here::here("Data", "Edad promedio circuitos 2019.xlsx"))


############### Algunos retoques a las bases para poder trabajar con ellas ######################

#names(DataDep) 
names(DataNac) <- names(DataDep) # Compatibilizar los nombres de las variables
names(EdadMed) <- c("Departamento", "CRV", "SERIES", "DESDE", "HASTA", "Edad")


## La idea es crear un código único por circuito

## Reemplaza los nombres de los departamentos en EdadMed por las 
## referencias que usan las otras bases (mismo orden)

for (i in 1:19){ EdadMed[EdadMed$Departamento == unique(EdadMed$Departamento)[i],1] <-   unique(DataNac$Departamento)[i] } 

# Crea los códigos
DataDep$ID <- paste0(DataDep$Departamento, DataDep$CRV)
DataNac$ID <- paste0(DataNac$Departamento, DataNac$CRV)
EdadMed$ID <- paste0(EdadMed$Departamento, EdadMed$CRV)

# Hay diferencias entre las bases. Opto por eliminar las entradas que no están en todas ellas
x <- setdiff(unique(DataNac$ID), unique(EdadMed$ID))
DataNac <- DataNac[!DataNac$ID %in% x, ]

x <- setdiff(unique(DataDep$ID), unique(EdadMed$ID))
DataDep <- DataDep[!DataDep$ID %in% x, ]

x <- setdiff(unique(DataDep$ID), unique(DataNac$ID))
DataDep <- DataDep[!DataDep$ID %in% x, ]
DataNac <- DataNac[!DataNac$ID %in% x, ]

x <- setdiff(unique(DataNac$ID), unique(DataDep$ID))
DataDep <- DataDep[!DataDep$ID %in% x, ]
DataNac <- DataNac[!DataNac$ID %in% x, ]

##########################################################################

# Variables de participación

DataDep$ParticipDep <- with(DataDep, (Total_Votos_Emitidos-Total_Votos_Observados)/Total_Habilitados)
DataNac$ParticipNac <- with(DataNac, (Total_Votos_Emitidos-Total_Votos_Observados)/Total_Habilitados)

# Armo una base nueva con los datos que me interesan

Base <- dplyr::full_join(DataDep[, c(1,10,11)], DataNac[, c(1, 10,11)], by= c("ID", "Departamento"))
Base <- dplyr::full_join(Base, EdadMed[, c(1, 6,7)], by= c("ID", "Departamento"))
Base$Diferencia <- Base$ParticipNac - Base$ParticipDep # Diferencia de participación entre elecciones
Base <- na.omit(Base)



# Voy a eliminar valores extremos, el 1% superior y el 1 % inferior.
# Hay observaciones muy raras, que pueden deberse a circuitos muy pequeños, o errores en asignación de datos

Base <- Base[Base$Diferencia < quantile(Base$Diferencia, .99), ]
Base <- Base[Base$Diferencia > quantile(Base$Diferencia, .01), ]

############################# ANOVA #####################################

# Factorización de la variable edad en tres categorías
Base$Edad_fact <- Base$Edad
Base$Edad_fact[Base$Edad >= 60] <- 2
Base$Edad_fact[Base$Edad < 60 & Base$Edad > 40] <- 1
Base$Edad_fact[Base$Edad <= 40] <- 0
Base$Edad_fact <- as.factor(Base$Edad_fact)


nrow(Base[Base$Edad < 60 & Base$Edad > 40, ])
nrow(Base[Base$Edad >= 60, ])
nrow(Base[Base$Edad <= 40, ])

Base$Edad_fact <- factor(Base$Edad_fact, levels = c(2, 1, 0), labels  = c("60 y más", "Entre 40 y 60", "40 y menos"))

min(DataDep$Total_Habilitados)

# Gráfico de violín para comparar las distribuciones 
library(ggplot2)
jpeg("Figures/Dist_ParticipaciónvsEdad.jpeg", width = 1200, height = 800, res = 150)
DistPlot <- ggplot(data = Base, aes(x= Edad_fact, y=Diferencia, fill=Edad_fact)) + 
                geom_violin() +
                scale_fill_manual(values=c("gray25", "gray50", "gray70"))+
                labs(x = "Edad promedio", y = "Diferencia de participación",
                     title = "Distribución de la diferencia de participación (nacional-departamental)", 
                     subtitle = "Según edad promedio del circuito",
                     caption = "Elaboración propia en base a datos de la Corte Electoral")+
                theme_minimal() +
                theme(text=element_text(family="Times"))+
                theme(legend.position="none")
               
DistPlot
dev.off()


# Homocedasticidad test
# Test de Levene  y barlett para comparar las varianzas de los grupos
car::leveneTest(y = Base$Diferencia , group = Base$Edad_fact, center = "median")
bartlett.test(Diferencia ~ Edad_fact, data = Base)

# Los datos tienen problemas de homogeneidad entre grupos y de heterocedasticidad (se rechazan las H0)

# Procediendo de todas formas con un ANOVA estandar

Mod2 <- aov(Diferencia ~ Edad_fact, data=Base)
summary(Mod2) # Hay diferencias significativas entre grupos
pairwise.t.test(Base$Diferencia,  Base$Edad_fact,  p.adj = "bonferroni") # Permite ver entre que grupos lo son


# El test de Kruskal-Wallis es una alternativa no paramétrica al test ANOVA
# https://rpubs.com/Joaquin_AR/219504
# Ojo, también es necesario que la varianza sea igual entre grupos

kruskal.test(Diferencia ~ Edad_fact, data = Base)
pairwise.wilcox.test(Base$Diferencia, Base$Edad_fact, p.adjust.method = "bonferroni")


# Tal vez una alternativa puede ser transformar los datos
# Transformación de Johnson buscando normalidad en la distribución
# No parece solucionar mucho

Dif_J <- Johnson::RE.Johnson(Base$Diferencia)
Base$Dif_J <- Dif_J$transformed
rm(Dif_J)

hist(Base$Dif_J)
normtest::jb.norm.test(Base$Dif_J) # No bueno pero mejor
car::leveneTest(y = Base$Dif_J , group = Base$Edad_fact, center = "median") #Sigue sin servir


# Repito lo mismo con la variable transformada

Mod2 <- aov(Dif_J ~ Edad_fact, data=Base)
summary(Mod2)

kruskal.test(Dif_J ~ Edad_fact, data = Base)
pairwise.wilcox.test(Base$Dif_J, Base$Edad_fact, p.adjust.method = "bonferroni")



#### Buscando que los estimadores sean robustos a la heterocedasticidad 
## http://ritsokiguess.site/docs/2017/05/19/welch-analysis-of-variance/
## https://www.researchgate.net/profile/Antonio-Monleon-Getino/publication/304283596_Diseno_de_experimentos_su_analisis_y_diagnostico/links/576b8cea08aefcf135bd5977/Diseno-de-experimentos-su-analisis-y-diagnostico.pdf


Mod <- lm (Diferencia ~ Edad_fact, data = Base)
car::Anova (Mod, white.adjust =TRUE) # white adjust: obtener un p-valor considerando la heterodasticidad

summary(Mod)
nrow(Base)
# devtools::install_github("matherion/userfriendlyscience")

# Welch ANOVA, no asume igualdad de varianzas entre gurpos
# Si sería necesario preocuparse por la normalidad de los datos

oneway.test(Diferencia~Edad_fact,data=Base)
oneway.test(Dif_J~Edad_fact,data=Base) 


# Games-Howell - método pos-hoc no paramétrico que no asume la igualdad de varianza
userfriendlyscience::oneway(y=Base$Diferencia, x=Base$Edad_fact, posthoc="games-howell")


### Resumen: los resultados son similares independiente del método utilizado. Quedandonos con este último,
### que estimo más conveniente dado la robustez a la heterocedasticidad y la no asunción de homogeneidad
### de varianzas, se tiene que:
        
        # El Welch ANOVA muestra diferencias estadísticamente significativas entre grupos
        # El test pos-hoc de Games_Howell entre la participación del grupo 2 y el 1, y el 2 y el 0
        # No así entre el grupo 1 y el 0. (Algo de esto se intuye de los gráficos de violín)


