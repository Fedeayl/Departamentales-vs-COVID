###### Test ANOVA ######


Resultados <- read.csv2(here::here("Data","ResultadosDepartamentales.csv"))

Resultados$Dif_Turnout <- Resultados$Turnout_Nac - Resultados$Turnout_Dep # Variable diferencia de votos en el ciclo

Resultados$COVID <- 0
Resultados[Resultados$Año==2020,]$COVID <- 1 # Variable Covid

Resultados$Comp_fact <- ifelse(Resultados$Competitividad <= mean(Resultados$Competitividad), yes= 0, no=1)

Mod <- aov(lm(Dif_Turnout ~ COVID + Alternancia_Nac + Incumbente + Comp_fact, data=Resultados))
summary(Mod)



### Datos de circuito 

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

# Variables de participación

DataDep$ParticipDep <- with(DataDep, (Total_Votos_Emitidos-Total_Votos_Observados)/Total_Habilitados)
DataNac$ParticipNac <- with(DataNac, (Total_Votos_Emitidos-Total_Votos_Observados)/Total_Habilitados)


# Armo una base nueva con los datos que me interesan

Base <- dplyr::full_join(DataDep[, c(1,10,11)], DataNac[, c(1, 10,11)], by= c("ID", "Departamento"))
Base <- dplyr::full_join(Base, EdadMed[, c(1, 6,7)], by= c("ID", "Departamento"))
Base$Diferencia <- Base$ParticipNac - Base$ParticipDep # Diferencia de participación entre elecciones
Base <- na.omit(Base)

##########################################################################




############################# ANOVA #####################################

# Factorización de la variable edad en tres categorías
Base$Edad_fact <- Base$Edad
Base$Edad_fact[Base$Edad >= 60] <- 2
Base$Edad_fact[Base$Edad < 60 & Base$Edad > 40] <- 1
Base$Edad_fact[Base$Edad <= 40] <- 0
Base$Edad_fact <- as.factor(Base$Edad_fact)

# Base$Edad_fact <- factor(Base$Edad_fact, levels = c(2, 1, 0), labels  = c("60 y más", "Entre 40 y 60", "40 y menos"))

library(ggplot2)

DistPlot <- ggplot(data = Base, aes(x= Edad_fact, y=Diferencia, fill=Edad_fact)) + 
                geom_violin() +
                theme_minimal() +
                ggtitle ("Distribución de la diferencia de votación por circuito por categoría de edad promedio")




# Prueba de normalidad 
shapiro.test(Base$Diferencia[Base$Edad_fact == "0"])
shapiro.test(Base$Diferencia[Base$Edad_fact == "1"])
shapiro.test(Base$Diferencia[Base$Edad_fact == "2"])

# Homocedasticidad test
# Test de Levene  y barlett para comparar las varianzas de los grupos
car::leveneTest(y = Base$Diferencia , group = Base$Edad_fact, center = "median")
bartlett.test(Diferencia ~ Edad_fact, data = Base)

# Los datos tienen problemas de normalidad y de heterocedasticidad (se rechazan las H0)





# Los datos son una cagada en tanto los supuestos de la técnica. Sin embargo es un N bastante grande
# Ignoremos el problema por un momento

Mod2 <- aov(Diferencia ~ Edad_fact, data=Base)
summary(Mod2) # Hay diferencias significativas entre grupos


# Permite ver entre que grupos lo son
pairwise.t.test(Base$Diferencia,  Base$Edad_fact,  p.adj = "bonferroni")



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


# Repito lo mismo con la variable transformada

Mod2 <- aov(Dif_J ~ Edad_fact, data=Base)
summary(Mod2)


pairwise.t.test( Base$Dif_J, 
                 Base$Edad_fact, 
                 p.adj = "bonferroni")



kruskal.test(Dif_J ~ Edad_fact, data = Base)
pairwise.wilcox.test(Base$Dif_J, Base$Edad_fact)
