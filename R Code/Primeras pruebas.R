
library(ggplot2)
# Importación de las bases

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

#######################################################################################################



# H: Circuitos más añosos, más pérdida de participación (espero signo positivo)
# El covid afecta más a los ancianos, ergo su percepción del riesgo es mayor
# y evitan concurrir a votar.

# Función que resume modelos de regresión para cada departamento 
# (Diferencia de participación entre elecciones vs Edad)
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



# Base con los casos
CasosCOVID <- rio::import(here::here("Data", "Covid.csv")) # Agregué una columna con el cálculo del índice de Harvard


# Con datos de circuito
BaseCir <- merge(Base, CasosCOVID)
ModCir <- lm(Diferencia ~ as.numeric(Hindex) , data = BaseCir)
summary(ModCir)


# Para mayores de 60
ModCir60 <- lm(Diferencia ~ as.numeric(Hindex) , data = BaseCir[BaseCir$Edad >= 60, ])
summary(ModCir60)



# Gráfico de densidad
Baseplot <- with(Base, cbind.data.frame(ParticipDep, ParticipNac))
Baseplot <- reshape::melt(Baseplot)

#jpeg("Figures/Participacion 2020vs2019.jpeg", width = 1200, height = 1200, res = 150)

p <- ggplot(data=Baseplot, aes(x=value, fill=variable))+
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
#dev.off()



# Participacion histórica

Participacion <- rio::import(here::here("Data", "Participación (2009-2020).xlsx"))



jpeg("Figures/Boxplot.jpeg", width = 1200, height = 1200, res = 150)

ggplot(Participacion[!Participacion$Año == 2020,], aes(x=Nivel, y=Part)) + 
        geom_boxplot() +
        geom_point(data=Participacion[Participacion$Año == 2020,], aes(x=Nivel, y=Part), color="gray60")+
        labs(title = "Distribución de participación elecciones nacionales y departamentales (2000-2020)",
             subtitle = "En gris los valores correspondientes a la elección 2020")+
        xlab("Nivel") +
        ylab("Participación") +
        theme_light() +
        theme(legend.position = "none")

dev.off()

# Gráfico participación Nacional vs Subnacional

Resumen <- doBy::summary_by(Participacion, Part ~ Año + Nivel, FUN = c(mean, sd))
Resumen$Nivel[Resumen$Nivel == "Nac"] <-  "Nacional"
Resumen$Nivel[Resumen$Nivel == "Dep"] <-  "Departamental"

jpeg("Figures/Barplot_Participacion.jpeg", width = 1200, height = 800, res = 150)
ggplot(data = Resumen, aes(x = Año, y = Part.mean, fill=Nivel)) +
       geom_bar(stat="identity", position="stack") + 
       ylim(0,1) + scale_fill_manual(values=c("gray80", "gray50"))+
       geom_text(aes(label= paste0(100*round(Part.mean,2), "%")), vjust = -1,
            color = "gray9", size = 2.5, family="Cambria") + 
       labs(x = "", y = "% Participación",
            title = "Participación promedio por departamento", 
            subtitle = "Elecciones nacionales y subnacionales",
            caption = "Elaboración en base a datos de la Corte Electoral") +
       theme_minimal()+
       theme(plot.margin = margin(30,30,30,30), 
              axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0),
              text=element_text(size=12, family="Cambria"))
       
dev.off()


