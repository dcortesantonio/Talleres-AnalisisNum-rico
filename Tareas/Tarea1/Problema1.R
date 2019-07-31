ErrorRedondeo = function (numero, NDecimales){
  n = 0
  numero= abs(numero)
 
  while(numero>1){
    numero=numero/10
    n=n+1
  }
  R=numero-trunc(numero*10^4)/10^4
  izquierda=-1*10^(n-NDecimales)
  derecha=1*10^(n-NDecimales)
  cat("Error de redondeo R está acotado por: "
        ,izquierda,"<",R,"<"
        ,derecha)
}
ErrorRedondeo(536.78,4)
