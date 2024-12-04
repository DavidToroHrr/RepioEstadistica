#para modelos míltiples
#importamos información

#modelo múltiple

Y= DATOS$Price
X1= DATOS$Mileage
X6= DATOS$Cylinder
X7= DATOS$Liter
X8= DATOS$Doors
X9=DATOS$Cruise
X10=DATOS$Sound
X11=DATOS$Leather
#Generar modelo###
# Crear un data.frame con las variables como vectores, no como fórmula
Tabla = data.frame(Y, X1, X6, X7, X8, X9, X10, X11)


cor(Tabla) #matriz de correlaciones 
plot(Tabla) #arroja diagrama de dispersion variable a variable o múltiple

ModeloMulti=lm(Y~X1+X6+X7+X8+X9+X10+X11)
summary(ModeloMulti) #me arroja los coeficientes
plot(ModeloMulti) #me hace la gráfica



#validación de supuestos (normalidad)###


Error2=residuals(ModeloMulti) #residual o error
hist(Error2) #esperamos que los residuales se comportan de forma normal, miramos si se asemeja a una CAMPANA DE GAUSS en el doc


#Kolmogorov n>=50
install.packages("nortest")
library("nortest")
lillie.test(Error2) #miramos p valor contra alfa y determinamos si de rechaza o no la normalidad

#validación de (homocedasticidad)
install.packages("lmtest")
library("lmtest")
bptest(ModeloMulti, studentize=FALSE)# 

# ahora hacemos la parte de estadística descriptiva

#Primera variable a utilizar (Price)
Precio= DATOS$Price

summary(Precio) # se calcula el mínimo 1st Quart, mediana,Media, 3rd Quart y el máximo.  
var(Precio) #varianza 
sd(Precio) #deviación estándar

# Crear el boxplot
boxplot(Precio, 
        col = "lightgreen", 
        main = "Diagrama de Caja: Precio", 
        ylab = "Precio del Vehículo", 
        horizontal = TRUE)

# Agregar línea con la media al boxplot
abline(v = mean(Precio), col = "red", lty = 2)

#Segunda variable a utilizar (Mileage)
Millas= DATOS$Mileage

summary(Millas) # se calcula el mínimo 1st Quart, mediana,Media, 3rd Quart y el máximo.  
var(Millas) #varianza 
sd(Millas) #deviación estándar

# Crear el boxplot
boxplot(Millas, 
        col = "lightgreen", 
        main = "Diagrama de Caja: Kilometraje",  
        xlab = "Kilometraje (millas)",         
        horizontal = TRUE)

# Agregar línea con la media al boxplot
abline(v = mean(Millas), col = "red", lty = 2)


