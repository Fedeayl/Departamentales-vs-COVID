
######## Tasa de ruralidad ########

# La idea es resumir el % de población rural por departamento
FUN <- function(Data, vector){
        v <- unique(vector)
        L <- list()
        Data_Subset <- data.frame()
        
        for(i in v[1:length(v)]) {
                Data_Subset <- Data[Data$nomdpto == i, ]
                L[[i]] <- (sum(Data_Subset$Ruralidad) / sum(as.numeric(Data_Subset$pesoano))) *100
        }
        
        L <- rlist::list.rbind(L)
        L <- as.data.frame(L)
        return(L)
}

#Carga de bases

ECH2019 <- rio::import(here::here("Data", "ECH_2019_Personas.sav"))
ECH2015 <- rio::import(here::here("Data", "ECH_2015_Personas.sav"))
ECH2010 <- rio::import(here::here("Data", "ECH_2010_Personas.sav"))

# Creación de la variable (ponderada por pesoano)

ECH2019$Ruralidad <- as.numeric(car::recode(ECH2019$region_4, "4=1; else = 0"))
ECH2019$Ruralidad  <- ECH2019$Ruralidad*ECH2019$pesoano

ECH2015$Ruralidad <- as.numeric(car::recode(ECH2015$region_4, "4=1; else = 0"))
ECH2015$Ruralidad  <- ECH2015$Ruralidad*ECH2015$pesoano

ECH2010$Ruralidad <- as.numeric(car::recode(ECH2010$region_4, "4=1; else = 0"))
ECH2010$Ruralidad  <- ECH2010$Ruralidad*ECH2010$pesoano


# Creación de las tablas de % de población rural por departamento para cada año
Ruralidad_2019 <- FUN(ECH2019, as.character(ECH2019$nomdpto))
Ruralidad_2015 <- FUN(ECH2015, as.character(ECH2015$nomdpto))
Ruralidad_2010 <- FUN(ECH2010, as.character(ECH2010$nomdpto))

Ruralidad_2019
Ruralidad_2015
Ruralidad_2010
