;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Removing trends in sub-field shifts.
;; 
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION detrendshifts, sft, r, par
;r = reference point coordinates
;sft =  shifts (array [x,y,lambda,pol])
;par = parameter set by template file, for poly_fit order of polynomial

     d1 = SIZE(sft, /dimensions)
     d2 = SIZE(r, /dimensions)

     IF ((d1[0] NE d2[0]) OR  (d1[1] NE d2[1]) OR (d1[2] NE d2[2])) THEN $
     MESSAGE, 'Parameters do not match.'
  
     FOR j = 0, d1[2]-1 DO BEGIN

         FOR i = 0, d1[1]-1 DO BEGIN

             xst = REFORM(sft[0, i, j, *]-r[0, i, j])
             yst = REFORM(sft[1, i, j, *]-r[1, i, j])
             x   = FINDGEN(d1[3])
             px  = poly_fit(x, xst, par)
             py  = poly_fit(x, yst, par)
             plot, xst + r[0, i, j], /ynozero

             ;
             ; subtract fit
             ;

;LK: detrend seems to work only with par=3, otherwise the next line will create dimension error
             sft[0, i, j, *] = xst - (px[0] + px[1]*x + px[2]*x^2. + px[3]*x^3.) + r[0, i, j]
             sft[1, i, j, *] = yst - (py[0] + py[1]*x + py[2]*x^2. + py[3]*x^3.) + r[1, i, j]
             oplot, sft[0, i, j, *]

         ENDFOR

     ENDFOR

;;***************************************************************
;; Done
;;***************************************************************

  RETURN, sft

END
