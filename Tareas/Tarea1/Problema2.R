squareRoot = function(n, Error)
{
  x = 100;
  y = 0.5*( x + n / x);
  repeat
  {
    if( abs(x - y) < Error)
    {
      cat("Raíz aproximada de ",n, " es: ", y , " Con error de " ,Error)
      break;
    }
    x = y
    y = 0.5 * (x+n/x)
  }
  
}



squareRoot(7,0.0000000000000001)