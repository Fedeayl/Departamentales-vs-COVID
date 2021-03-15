# abro base "paperUNR.xlsx"
deptos <- rio::import(file.choose())
View(deptos)

attach(deptos)

particip <- emitidos/habilitados

# media por eleccion
tapply(particip, año, mean)

barplot(particip[año==2020]~dpto[año==2020], ylim=c(0,1))

# bivariadas con casos nuevos y activos
cor.test(particip, nuevos_t)
cor.test(particip, activos_t)



# casos del dia
modelo1 <- lm(particip ~ nuevos_t + activos_t)
summary(modelo1)

# t-1
modelo2 <- lm(particip ~ nuevos_t1 + activos_t1)
summary(modelo2)

# t-2
modelo3 <- lm(particip ~ nuevos_t2 + activos_t2)
summary(modelo3)

# t-3
modelo4 <- lm(particip ~ nuevos_t3 + activos_t3)
summary(modelo4)

# t-4
modelo5 <- lm(particip ~ nuevos_t4 + activos_t4)
summary(modelo5)

# t-5
modelo6 <- lm(particip ~ nuevos_t5 + activos_t5)
summary(modelo6)

# t-6
modelo7 <- lm(particip ~ nuevos_t6 + activos_t6)
summary(modelo7)

# t-7
modelo8 <- lm(particip ~ nuevos_t7 + activos_t7)
summary(modelo8)

### todos los modelos siempre dan signo negativo y casi siempre significativo
### a la variable de casos nuevos, no así a los activos. 