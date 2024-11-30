X1C=DATOS$Time
X2C=DATOS$Diet
X3C=DATOS$Chick
Y2C=DATOS$weight

#Generar el modelo
TablamC=data.frame(Y2C,X1C,X2C,X3C)
cor(TablamC)#me arroja la matriz de correlación.Queremos ver la fuerza de cada una de las variables
plot(TablamC)#es el diagrama de dispersión variable a variablle. Debemos indentificar entre las y y las x que sean lineales
#y entre independientes que no tengan NINGUNA FORMA


#con el diagrama de dispersión variable a variable y con la matriz de correlación nos podemos dar cuenta de que la única variable  que 
#nos sirve para hacer nustro análisis, ya que en la gráfica Y2 y X1C muestran una línea recta y en la matriz vemos que la fuerza de 
#correlación es fuerte


#PROCEDEMOS A REALIZAR UNA REGRESIÓN MULTIPLE

summary(Modelomultiple)#nos saca los residuales y no los interpretamos
#nos arroja los coeficientes
#r cuadrado
#p-valor...para la prueba de hipótesis de linealidad

plot(Modelomultiple)#heterrosedástico porque se observa un embudo
abline(Modelomultiple)#arroja la línea
#Q-Q
#RESIDUALS VS FITTED 


#VAMOS A hacer validación de supuestos
#validación de normalidad
#la puedo validad con Q-Q o con dos pruebas, kolmo y shapiro (obtenemos un p valor)
#me arroja un p valor, si es menor que alfa rechazo Ho, sino no rechazo Ho
#n<50 shapiro
#n>=50 kolmo


#kol n>=50
Errorm=residuals(Modelomultiple)

install.packages("nortest")
library("nortest")
lillie.test(Errorm)#en este caso no aplica para nuestra base de datos por el número de datos. HACEMOS LA COMPARACIÓN CON EL P-VALOR DE NUEVO


#Validación de homosedasticidad
install.packages("lmtest")
library("lmtest")
bptest(Modelomultiple,studentize = FALSE)#P-valor<alfa rechazho Ho, las varianzas son heterosedásticas...
#validamos con los gráficos y analizamos los indicadores como lo hemos hecho en otras clases

