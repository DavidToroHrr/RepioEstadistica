#mirar quién es la dependiente y la independiente

# múltiple regresión lineal
X1=DATOS$`Circunferencia de la cintura (cm)`
X2=DATOS$`Nivel Socioeconómico`
X3=DATOS$Peso
X4=DATOS$Edad
Y2=DATOS$`Tejido adiposo abdominal`


#modelo simple correlación, múltiple es matriz de correlación

#Generar el modelo
Tablam=data.frame(Y2,X1,X2,X3,X4)
cor(Tablam)#me arroja la matriz de correlación.Queremos ver la fuerza de cada una de las variables
plot(Tablam)#es el diagrama de dispersión variable a variablle. Debemos indentificar entre las y y las x que sean lineales
#y entre independientes que no tengan NINGUNA FORMA


#con el diagrama de dispersión variable a variable y con la matriz de correlación nos podemos dar cuenta de que la única variable  que 
#nos sirve para hacer nustro análisis, ya que en la gráfica Y2 y X1 muestran una línea recta y en la matriz vemos que la fuerza de 
#correlación es fuerte


#PROCEDEMOS A REALIZAR UNA REGRESIÓN SIMPLE

Modelosimple=lm(Y2~X1)#corresponde a los coeficientes del modelo de la línea recta

#nos arroja el intercepto 
#nos arroja un X1

plot(X1,Y2,col="red")#diagrama de dispersión
abline(Modelosimple)#arroja la línea

cor(X1,Y2)#coef Correlación con el signo negativo o positivo. Nega-> si son inversamente proporcipnales
#fuerte,medio,débil->en este caso es fuerte

summary(Modelosimple)#nos saca los residuales y no los interpretamos
#nos arroja los coeficientes
#r cuadrado
#p-valor...para la prueba de hipótesis de linealidad

plot(Modelosimple)#heterrosedástico porque se observa un embudo


#Q-Q
#RESIDUALS VS FITTED 


#VAMOS A hacer validación de supuestos
#validación de normalidad
#la puedo validad con Q-Q o con dos pruebas, kolmo y shapiro (obtenemos un p valor)
#me arroja un p valor, si es menor que alfa rechazo Ho, sino no rechazo Ho
#n<50 shapiro
#n>=50 kolmo

#Shapiro n<50

#el error es la diferencia entre el valor observaado y el obtenido ES EL RESIDUAL
Error=residuals(Modelosimple)
hist(Error)#vamos a analizar el histograma del comportamiento de los residuales
#esperamos que se comporten dem manera normal, ver si se asemeja a campana de GAUSS o NO
shapiro.test(Error)#W= VALOR CALCULADO p-valor=0,05239   alpha=0,05   p-valor>alfa (NO RECHAZO Ho) los residuos sí se distribuyen normalente
#El test es más confiable

#kol n>=50

install.packages("nortest")
library("nortest")
lillie.test(Error)#en este caso no aplica para nuestra base de datos por el número de datos. HACEMOS LA COMPARACIÓN CON EL P-VALOR DE NUEVO



#Validación de homosedasticidad
install.packages("lmtest")
library("lmtest")
bptest(Modelosimple,studentize = FALSE)#P-valor<alfa rechazho Ho, las varianzas son heterosedásticas...
#validamos con los gráficos y analizamos los indicadores como lo hemos hecho en otras clases




summary(X1)

# Crear el boxplot
boxplot(X1, 
        col = "lightgreen", 
        main = "Diagrama de Caja: Circunferencia de la Cintura", 
        ylab = "Circunferencia (cm)", 
        horizontal = TRUE)

# Agregar línea con la media (opcional)
abline(v = mean(X1), col = "red", lty = 2)







Modelomultiple=lm(Y2~X1+X2)#corresponde a los coeficientes del modelo de la línea recta

#nos arroja el intercepto 
#nos arroja un X

plot(X,Y,col="red")#diagrama de dispersión
abline(Modelomultiple)#arroja la línea

cor(X,Y)#coef Correlación con el signo negativo o positivo. Nega-> si son inversamente proporcipnales
#fuerte,medio,débil->en este caso es fuerte

summary(Modelomultiple)#nos saca los residuales y no los interpretamos
#nos arroja los coeficientes
#r cuadrado
#p-valor...para la prueba de hipótesis de linealidad

plot(Modelomultiple)#heterrosedástico porque se observa un embudo
#Q-Q
#RESIDUALS VS FITTED 


#VAMOS A hacer validación de supuestos
#validación de normalidad
#la puedo validad con Q-Q o con dos pruebas, kolmo y shapiro (obtenemos un p valor)
#me arroja un p valor, si es menor que alfa rechazo Ho, sino no rechazo Ho
#n<50 shapiro
#n>=50 kolmo

#Shapiro n<50

#el error es la diferencia entre el valor observaado y el obtenido ES EL RESIDUAL
Errorm=residuals(Modelomultiple)
hist(Errorm)#vamos a analizar el histograma del comportamiento de los residuales
#esperamos que se comporten dem manera normal, ver si se asemeja a campana de GAUSS o NO
shapiro.test(Errorm)#W= VALOR CALCULADO p-valor=0,05239   alpha=0,05   p-valor>alfa (NO RECHAZO Ho) los residuos sí se distribuyen normalente
#El test es más confiable

#kol n>=50

install.packages("nortest")
library("nortest")
lillie.test(Error)#en este caso no aplica para nuestra base de datos por el número de datos. HACEMOS LA COMPARACIÓN CON EL P-VALOR DE NUEVO


#Validación de homosedasticidad
install.packages("lmtest")
library("lmtest")
bptest(Modelomultiple,studentize = FALSE)#P-valor<alfa rechazho Ho, las varianzas son heterosedásticas...
#validamos con los gráficos y analizamos los indicadores como lo hemos hecho en otras clases
