#Algoritmo punto fijo 
PuntoFijo = function(f, x0, tol, numIteraciones){
  k=1
  repeat{
    x1=f(x0)
    dx=abs(x1-x0)
    x0=x1
    #Imprimir estado
    cat("x ", k, "= ", x1,"\n")
    k=k+1
    #Hasta
    if(dx< tol || k>numIteraciones) break;
  }
  #Mensaje salida 
  if(dx>tol){
    cat("No hubo convergencia  ")
    #return(NULL)
  }else{
    cat("x* es aproximadamente ", x1, "con error menor que ",tol)
  }
}

f= function(x) (x+1)*sin(x^2)
PuntoFijo(f,0.5,1e-9,100)

