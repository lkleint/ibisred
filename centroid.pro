FUNCTION Centroid, array
   s = Size(array, /Dimensions)
   totalMass = Total(array)
   xcm = Total( Total(array, 2) * Indgen(s[0]) ) / totalMass
   ycm = Total( Total(array, 1) * Indgen(s[1]) ) / totalMass                                                                        
   RETURN, [xcm, ycm]
   END
