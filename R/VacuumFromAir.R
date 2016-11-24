VacuumFromAir = function(air){
  
  vacLookup <- 0:7999 + 3500
  convertLookup <- (1.0 + 2.735182E-4 + 131.4182 / vacLookup^2 + 2.76249E8 / vacLookup^4)
  airLookup <- vacLookup / convertLookup
  convert <- approx(x = airLookup, y = convertLookup, xout = air, method = "linear",rule=2)$y
  vac <- air * convert
  
  return = vac
}
