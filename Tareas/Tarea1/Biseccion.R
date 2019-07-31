install.packages(pracma)
require(pracma)

biseccion = function(f, a1, b1, tol)
{
  if( sign(f(a1)) == sign(f(b1)) )
  {
    stop("f(a1) y f(b1) tienen el mismo signo")
  }
  a = a1;
  b = b1;
  i = 0;
  errores = c()
  iteraciones = c()
  cat(formatC(c("a","b","m","Error est."), width = -15, format = "f", flag = " "),"\n")
  
  repeat
  {
    
    m = a + 0.5 * ( b - a )
    if(f(m) == 0)
    {
        cat("Cero de la función en [",a1,",",b1,"] es: ",m)
    }
    if(sign(f(a)) != sign(f(m)))
    {
        b = m
    }
    else
    {
        a = m
    }
    #Calcular el error generado
    estError = ( b - a ) / 2
    errores = c(errores,estError)
    iteraciones = c(iteraciones,i)
    #Imprimir resultado de algoritmo de bisección
    cat(formatC( c(a,b,m,estError), digits = 7, width = -15, format = "f", flag = " "), "\n")
    # Hacer update de Index (Iteraciones)
    i = i + 1
    #Condición del ciclo (Tolerancia de Error)
    if( estError < tol || i>100)
    {
      cat("Cero de función en [",a1,",",b1,"] aproximadamente es: ", m, " con error <=", estError, "Iteraciones: ", i,"\n Predicción: ",log((b1 - a1)/tol)/log(2))
      break;
    }
  }
  plot(iteraciones,errores, type = "l")
}

f = function(x) x^3-x-1
curve(f, -2,2); abline(h=, v=0);
biseccion(f, 1,2, 0.000000001)
#TEST
#rootToFind = 4
#f = function(x) x^2-rootToFind
#curve(f, -2,2); abline(h=, v=0);
#biseccion(f, 0, rootToFind/2, 0.000000000000000001)
