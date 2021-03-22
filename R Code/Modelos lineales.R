# Pruebas - Modelos lineales

Resultados <- read.csv2(here::here("Data","ResultadosDepartamentales.csv"))

Resultados$Dif_Turnout <- Resultados$Turnout_Nac - Resultados$Turnout_Dep # Variable diferencia de votos en el ciclo

Resultados$COVID <- 0
Resultados[Resultados$Año==2020,]$COVID <- 1 # Variable Covid

summary(ModP2)

ModP1 <- lm(data=Resultados, Dif_Turnout ~ COVID)

ModP2 <- lm(data=Resultados, Dif_Turnout ~ COVID + Ruralidad + IDH + Secundaria + Ingreso )

ModP3 <- lm(data=Resultados, Dif_Turnout ~ COVID + Incumbente + Competitividad + Funcionarios)

ModP4 <-  lm(data=Resultados, Dif_Turnout ~ COVID + Incumbente + Funcionarios + Competitividad 
                                                  + Ruralidad + IDH + Secundaria + Ingreso)

jtools::export_summs(ModP1, ModP2, ModP3, ModP4)


