Horner = function (funcion, g, x0){
  resultado=funcion[1]
  n=0
  for(i in 2:(g+1)){
    resultado= resultado*x0 + funcion[i]
    n=n+2
  }
  cat("El resultado del polinomio es: ", resultado, " en ",n,"sumas.")
}
funcion<-c(2,0,-3,3,-4)
Horner(funcion,4,-2)