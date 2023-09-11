
degrees_2_radians <- function( degrees ){
  radians <-  (2*pi)*degrees/360 
  return( radians )
}

Radians2Degrees <- function( radians ){
  degrees <-  360*radians/(2*pi) 
  return( degrees )
}