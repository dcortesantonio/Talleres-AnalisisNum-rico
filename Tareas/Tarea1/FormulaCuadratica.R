Cuadratica = function (a,b,c){
  #La variaci�n de la f�rmula se utilizar� en la resta para evitar el inconveniente.
  #--------------------------
  #M�todo normal (suma)
  x1=-(b+sqrt(b^2-4*a*c))/(2*a)
  #F�rmula racionalizada por el conjugado
  x2=-(2*c)/(b+sqrt(b^2-4*a*c))
  cat("Con 8 decimales.\n")
  options(digits=8)
  cat("Ra�z 1: ",x1, " Ra�z 2: ",x2,"\n")
  options(digits=16)
  cat("Con 16 decimales.\n")
  cat("Ra�z 1: ",x1, " Ra�z 2: ",x2)
}
Cuadratica(3,9^12,-3)