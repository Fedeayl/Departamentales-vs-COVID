# Pruebas - Modelos lineales

Resultados <- read.csv2(here::here("Data","ResultadosDepartamentales.csv"))

Resultados$Dif_Turnout <- Resultados$Turnout_Nac - Resultados$Turnout_Dep # Variable diferencia de votos en el ciclo

Resultados$COVID <- 0
Resultados[Resultados$Año==2020,]$COVID <- 1 # Variable Covid


# Modelos

ModP1 <- lm(data=Resultados, Dif_Turnout^(1/2) ~ COVID)
ModP2 <- lm(data=Resultados, Dif_Turnout^(1/2) ~ COVID + Ruralidad + IDH + Secundaria + Ingreso + Funcionarios)
ModP3 <- lm(data=Resultados, Dif_Turnout^(1/2) ~ COVID + Incumbente + Competitividad + Alternancia_Nac + N_Candidatos)
ModP4 <- lm(data=Resultados, Dif_Turnout^(1/2) ~ COVID + Incumbente + Funcionarios + Competitividad +
                                                         Ruralidad + IDH + Secundaria + Ingreso)


### Normalidad de los residuos 

tseries::jarque.bera.test(ModP1$residuals)
tseries::jarque.bera.test(ModP2$residuals)
tseries::jarque.bera.test(ModP3$residuals)
tseries::jarque.bera.test(ModP4$residuals)
# Con la transformación raíz cuadrada no hay problemas con la distribución de los residuos de los modelos


### Homocedasticidad

lmtest::bptest(ModP1)
lmtest::bptest(ModP2)
lmtest::bptest(ModP3)
lmtest::bptest(ModP4)
# No se rechaza la H0 en ningún caso, se acepta el supuesto de homocedasticidad


### Test de especificación Reset de Ramsey 

lmtest::resettest(ModP1)
lmtest::resettest(ModP2)
lmtest::resettest(ModP3)
lmtest::resettest(ModP4)
# No se rechaza la H0 en ningún caso. Asumimos que no hay problemas de linealidad


### Multicolinealidad

car::vif(ModP2)
car::vif(ModP3)
car::vif(ModP4)
# Sin dramas en cuanto a la multicolinealidad


### Salida de los modelos
jtools::export_summs(ModP1, ModP2, ModP3, ModP4)


