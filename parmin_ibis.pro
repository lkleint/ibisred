FUNCTION parmin_ibis, y, npoints
;lk: added > and < to avoid crashes

  s = size(y)
  yy = y ;profile

  pmin = min(yy,xxpos) ;line core
  yn = yy[xxpos-npoints>0:xxpos+npoints<s[1]-1] ;part of line profile
  xn = findgen(n_elements(yn))
  coef = poly_fit(xn,yn,2,/double) ;parabolic fit to profile
  
  if coef(2) ne 0 then zent = -1.*coef(1)/(2.*coef(2)) ;-b/2a to find x pos of min
  xmin = zent + (xxpos-npoints) ;add part of profile that was excluded in fit

  return, xmin ;x position of line core
END
