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
  cat(formatC(c("a","b","m1","m2","Error est."), width = -15, format = "f", flag = " "),"\n")
  
  repeat
  {
    
    m1 = a + ( b - a )/3
    m2 = a + 2*( b - a )/3
    if(f(m1) == 0)
    {
        cat("Cero de la función en [",a1,",",b1,"] es: ",m1)
    }
    if(f(m2) == 0)
    {
        cat("Cero de la funci?n en [",a1,",",b1,"] es: ",m2)  
    }
    if(sign(f(a)) != sign(f(m1)))
    {
        b = m1
    }
    else if(sign(f(m1)) != sign(f(m2)))
    {
        a=m1
        b=m2
    }
    else 
    {
      a=m2
    }
    #Calcular el error generado
    estError = ( b - a ) / 2
    errores = c(errores,estError)
    iteraciones = c(iteraciones,i)
    #Imprimir resultado de algoritmo de bisección
    cat(formatC( c(a,b,m1,m2,estError), digits = 7, width = -15, format = "f", flag = " "), "\n")
    # Hacer update de Index (Iteraciones)
    i = i + 1
    #Condición del ciclo (Tolerancia de Error)
    if( estError < tol || i>1000)
    {
      m1 = a + ( b - a )/3
      m2 = a + 2*( b - a )/3
      cat("Cero de función en [",a1,",",b1,"] aproximadamente es: ", (m2+m1)/2, " con error <=", estError, "Iteraciones: ", i,"\n Predicci?n bisecci?n: ",log((b1 - a1)/tol)/log(2))
      break;
    }
  }
  plot(iteraciones,errores, type = "l")
}

f = function(x) x^3-x-1
curve(f, -2,2); abline(h=, v=0);
biseccion(f, 1,2, 0.000000001)


