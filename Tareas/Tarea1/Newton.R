Newton = function (f,fp,x0,tol,maxiter){
  k=0
  repeat{
    correcion = f(x0)/fp(x0)
    x1 = x0 - correcion
    dx = abs(x1-x0)
    x0 = x1
    k = k+1
    if(dx<=tol || k >maxiter)
      break
    
  }
  if(k > maxiter){
    cat("Numero máximo de Iteraciones alcanzado.")
    cat("Iteraciones=", k, " X: ",x1," f(x):",f(x1)," Error Estimado: ", correcion)
  }
  else{
    cat("Iteraciones=", k, " X: ",x1," f(x):",f(x1)," Error Estimado: ", correcion)
    
  }
}
f= function(x) exp(x)-pi*x
fp= function(x) exp(x)-pi
Newton(f,fp,1.6,1e-8,1000)