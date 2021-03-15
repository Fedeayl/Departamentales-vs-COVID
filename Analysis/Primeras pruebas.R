
library(ggplot2)
# Importación de las bases

DataDep <- rio::import(here::here("Data","Subnacionales 2020 - Total GENERAL por circuito.xlsx"))
DataNac <- rio::import(here::here("Data", "Nacional 2019 - Total GENERAL por circuito.xlsx"))
EdadMed <- rio::import(here::here("Data", "Edad promedio circuitos 2019.xlsx"))


# Algunos retoques a las bases para poder trabajar con ellas

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


# H: Circuitos más añosos, más pérdida de participación (espero signo positivo)
# El covid afecta más a los ancianos, ergo su percepción del riesgo es mayor
# y evitan concurrir a votar.

# Función que resume modelos de regresión para cada departamento (Diferencia de participación entre elecciones vs Edad)
FUN <- function(Data, vector){
        
        v <- unique(vector)
        L <- list()
       
         for(i in v[1:length(v)]) {
                Mod <- summary(lm(formula=Diferencia ~ Edad, 
                                  data=Data[Data$Departamento == i, ], 
                                  na.action = na.exclude))
                
                L[[i]] <- c(Mod$coefficients[2,1], Mod$coefficients[2,4]) }
       
        L <- rlist::list.rbind(L)
        L <- as.data.frame(L)
        names(L) <- c("coef", "p-value")
        L <- round(L, 4)
        L$Sig <- ifelse(L$`p-value` < 0.05, "***", "")
        return(L)
}


ModDepto <- FUN(Base, unique(as.character(Base$Departamento)))
ModDepto

ModDepto <- FUN(Base[Base$Edad >= 60,], Base$Departamento)
ModDepto


# Modelos para los circuitos con un promedio de edad mayor a 60 años
CasosCOVID <- rio::import(here::here("Data", "Covid.csv")) # Agregué una columna con el cálculo del índice de Harvard

Base60 <- Base[Base$Edad >= 60,]
Base60 <- doBy::summary_by(data = Base60, formula = Diferencia ~ Departamento, FUN = mean, order = FALSE)
Base60 <- dplyr::full_join(Base60, CasosCOVID, by= c("Departamento"))

Mod60 <- lm(Diferencia ~ Hindex, data=Base60)
summary(Mod60)



# Con datos de circuito
BaseCir <- merge(Base, CasosCOVID)
ModCir <- lm(Diferencia ~ as.numeric(Hindex) , data = BaseCir)
summary(ModCir)



# Gráfico de densidad
Baseplot <- with(Base, cbind.data.frame(ParticipDep, ParticipNac))
Baseplot <- reshape::melt(Baseplot)

p <- ggplot(data=Baseplot, aes(x=value, fill=variable ))+
        geom_density(alpha=0.6) +
        scale_fill_manual(values=c("coral1", "cadetblue3"))+
        geom_vline(xintercept = mean(Baseplot$value[Baseplot$variable == "ParticipDep"]), colour="coral3")+
        geom_vline(xintercept = mean(Baseplot$value[Baseplot$variable == "ParticipNac"]), colour="cadetblue")+
        xlim(0,1)+
        labs(title = "Distribución de participación por circuito",
             subtitle = "Elecciones nacionales 2019 y departamentales 2020")+
        xlab("Participación") +
        ylab("Densidad") +
        theme_light()
p

names(DataNac)
doBy::summary_by(data=DataNac, Total_Habilitados ~ Departamento, FUN = sum)


# Participacion histórica

Participacion <- rio::import(here::here("Data", "Participación (2009-2020).xlsx"))

Resumen <- doBy::summary_by(Participacion, Part ~ Año + Nivel, FUN = c(mean, sd))

ggplot(Participacion[!Participacion$Año == 2020,], aes(x=Nivel, y=Part)) + 
        geom_boxplot() +
        geom_point(data=Participacion[Participacion$Año == 2020,], aes(x=Nivel, y=Part, color="red"))+
        labs(title = "Distribución de participación elecciones nacionales y departamentales (2000-2020)",
             subtitle = "En rojo los datos de la elección 2020")+
        xlab("Nivel") +
        ylab("Participación") +
        theme_light() +
        theme(legend.position = "none")

