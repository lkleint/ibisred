;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Determine the IBIS aperture in frame.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_get_aperture, frame

;;***************************************************************
;; Get dimensions
;;***************************************************************

     aperture = 0*FIX(frame)

     dim = SIZE(aperture)
     Nx  = dim[1]
     Ny  = dim[2]

;;***************************************************************
;; Show input and determine mask parameters
;;***************************************************************

     tvscl,frame
     inc = 50
     thres = mean(frame)

     PRINT,'---------------------------------------------------------'
     PRINT,'type "a" or "d" to increase/decrease the threshold value '
     PRINT,'type "w" or "s" to increase/decrease the increment value '
     PRINT,'type "q" when done                                       '
     PRINT,'---------------------------------------------------------'

     k = GET_KBRD(1)

     WHILE (k NE 'q') DO BEGIN

           IF (k EQ 'd') THEN thres += inc ELSE $
           IF (k EQ 'a') THEN thres -= inc ELSE $
           IF (k EQ 'w') THEN inc += 50 ELSE $
           IF (k EQ 's') THEN inc -= 50

           aperture *= 0
           aperture[where(frame GT thres)] = 1
           tvscl, (aperture-erode(aperture,[[0,1,0],[1,1,1],[0,1,0]]))
           WRITEU,-1, 'Threshold: '+strtrim(thres,2), '     Increase: '+strtrim(inc,2)+string(13B)
           k = GET_KBRD(1)

     ENDWHILE

;;***************************************************************
;; Done
;;***************************************************************

     RETURN, label_region(aperture)

END
