#n representa el n�mero de polinomios de taylor 
#x representa el exponente de la expresi�n
 Aproximacion=function (n,x){
  suma=1
  i=n-1
  while(i>0){
    suma=1+x*suma/i
    i=i-1
  }
  cat(signif(suma,digits=5)) 
}
Aproximacion(5,0.5)