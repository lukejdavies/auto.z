AirFromVacuum = function(vac){
  air <- vac / (1.0 + 2.735182E-4 + 131.4182 / vac^2 + 2.76249E8 / vac^4)
  return = air
}
