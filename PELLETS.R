# Importamos la información
X = DATOS$Gramos 
Y = DATOS$Costo



# Generamos modelo lineal simple para comparación
Modelosimple = lm(Y ~ X)  

#Resumen del modelo lineal simple
summary(Modelosimple)

# Gráfico de dispersión con la línea del modelo lineal simple
plot(X, Y, col="red", main="Relación entre X e Y con modelo lineal")
abline(Modelosimple, col="blue")  #Añadimos una linea


cor(X, Y)  #Coeficiente de correlación

plot(Modelosimple)#evidenciamos que no es el mejor ajuste (el lineal)


################################################################################
# Ajustar el modelo polinómico 
Modelopol <- lm(Y ~ X + I(X^2))  #incluye X y X^2 

# Resumen del modelo para ver los coeficientes
summary(Modelopol)

# Gráfico de dispersión con la curva del modelo ajustado
plot(X, Y, col="blue", main="Relación entre X e Y con modelo polinómico")
lines(X, predict(Modelopol), col="red")  # Añadimos la línea de ajuste del modelo polinómico

plot(Modelopol) # Solamente analizamos los primeros dos gráficos, ya está mejor ajustado


# Comprobación de normalidad de los residuos
hist(residuals(Modelopol), main="Histograma de los residuos", col="lightblue")


#Normalidad de los residuos Kolmogorov
install.packages("nortest")
library(nortest)
lillie.test(Error)  # Prueba de Kolmogorov para normalidad

#Homocedasticidad de los residuos Breusch Pagan
install.packages("lmtest")
library(lmtest)
bptest(Modelosimple, studentize=FALSE)  # Prueba de Breusch Pagan para homocedasticidad
