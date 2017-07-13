;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;  Procedure: ibis_scan_calibrate_xt
;
;  Purpose: Calibrate an IBIS scan (detsretched and aligned) for
;           telescope and instrument polarization effects.
;
;
;  Input:  stksin -- 5 dimensional data cube
;                      dimensions = (x,y,lambda,4,beams)
;
;           xmatl -- X matrix for left beam, dimension (4 x 4)
;
;           xmatr -- X matrix for right beam, dimension  (4 x 4)
;
;            tmat -- T matrix during observation, dimension  (4 x 4)
;                    we assume that the T matrix does not change during 
;                    the acquisition time of one scan. 
;
;  Output: stksin -- 4 dimensional data cube 
;                      dimensions = (x,y,lambda,4)
;
;
;  ali@nso.edu February 2007
; mod Nov 2014 LK for y-dependent X-matrix, range is y-range in pixels
; where X-matrix was determined. wlobs=wavelength points where polcal
; was done. wlscan=observed wavelengths
; ### todo: add interpolation for wavelengths
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_scan_calibrate_xt, stksin, xmatl, xmatr, tmat, stksout, range, wlobs, wlscan

;-------------------------------------------------
;; Create Arrays
;-------------------------------------------------

      s = SIZE(stksin)

      stksinl = REFORM( stksin[*,*,*,*,0] )
      stksinr = REFORM( stksin[*,*,*,*,1] )

      stksoutl = stksinl
      stksoutr = stksinr

      stksout = fltarr( s[1], s[2], s[3], 4)

;-------------------------------------------------
;; Invert matrices
;-------------------------------------------------

;     Xinv_right = INVERT(xmatr)
;     Xinv_left  = INVERT(xmatl)

;mod LK Nov 2014 for FOV dependent X-matrix
     ny = range[1]-range[0]+1
     xtmp1 = smooth(xmatr,[1,1,5,1])  ;smooth X-matrix spatially
     xtmp2 = smooth(xmatl,[1,1,5,1])  ;smooth X-matrix
     nwl = (size(xmatr))[4]
     nscan = n_elements(wlscan)

     ;interpolate for observed wavelength
     xtmp1b = fltarr(4,4,ny,nscan) ;right
     xtmp2b = fltarr(4,4,ny,nscan) ;left
     Xinv_right = xtmp1b*0.
     Xinv_left = xtmp1b*0.

       for i=0,3 do begin
        for j=0,3 do begin           
           for yy=0,ny-1 do begin
             xtmp1b[i,j,yy,*] = interpol(xtmp1[i,j,yy,*],wlobs,wlscan)
             xtmp2b[i,j,yy,*] = interpol(xtmp2[i,j,yy,*],wlobs,wlscan)
           endfor
        endfor
       endfor


     ;invert matrices
     for j=0,nscan-1 do begin
       for i=0,ny-1 do begin
       Xinv_right[*,*,i,j] = INVERT(xtmp1b[*,*,i,j])
       Xinv_left[*,*,i,j]  = INVERT(xtmp2b[*,*,i,j])
       endfor
     endfor

     Tinv = INVERT(Tmat) 
     Tinv = Tinv/Tinv[0,0]


;----------------------------------------------------
;; 5. Loop over data ---> Do Calibration for X and T
;----------------------------------------------------

stop

     s = SIZE(stksinr)

     FOR i = 0, s[1] - 1 DO BEGIN  ;x direction
        if i mod 50 eq 0 then print,'x=',i

         FOR j = 0, s[2] - 1 DO BEGIN ;ydirection

     ;            print,(j-range[0])>0<(range[1]-range[0])

             FOR l = 0, s[3]-1 DO BEGIN ;lambda

                 ar = REFORM( stksinr[i,j,l,*] )

;mod LK Nov 2014, for y-dependent X-matrix
                 xinv_r_tmp = xinv_right[*,*,(j-range[0])>0<(range[1]-range[0]),l]
 ;                br = Xinv_right ## ar
                 br = Xinv_r_tmp ## ar
                 cr = Tinv ## br
                 al = REFORM( stksinl[i,j,l,*] )
;                 bl = Xinv_left ## al
                 xinv_l_tmp = xinv_left[*,*,(j-range[0])>0<(range[1]-range[0]),l]
                 bl = Xinv_l_tmp ## al
                 cl = Tinv ## bl
                 stksoutl[i,j,l,*] = cl
                 stksoutr[i,j,l,*] = cr

             ENDFOR

         ENDFOR

     ENDFOR

stop

;-------------------------------------------------
;; Combine calibrated left and right beam
;-------------------------------------------------

;includes correction for beam imbalance (Tom's version)
stksout[*,*,*,0] = REFORM( 0.5 * ( stksoutl[*,*,*,0] + stksoutr[*,*,*,0] ) )
stksout[*,*,*,1] = REFORM( 0.5 * ((stksoutl[*,*,*,1]/stksoutl[*,*,*,0]) + stksoutr[*,*,*,1]/stksoutr[*,*,*,0])) * stksout[*,*,*,0]
stksout[*,*,*,2] = REFORM( 0.5 * ((stksoutl[*,*,*,2]/stksoutl[*,*,*,0]) + stksoutr[*,*,*,2]/stksoutr[*,*,*,0])) * stksout[*,*,*,0]
stksout[*,*,*,3] = REFORM( 0.5 * ((stksoutl[*,*,*,3]/stksoutl[*,*,*,0]) + stksoutr[*,*,*,3]/stksoutr[*,*,*,0])) * stksout[*,*,*,0]


;old version
;     stksout[*,*,*,0] = REFORM( 0.5 * ( stksoutl[*,*,*,0] + stksoutr[*,*,*,0] ) )
;     stksout[*,*,*,1] = REFORM( 0.5 * ( stksoutl[*,*,*,1] + stksoutr[*,*,*,1] ) )
;     stksout[*,*,*,2] = REFORM( 0.5 * ( stksoutl[*,*,*,2] + stksoutr[*,*,*,2] ) )
;     stksout[*,*,*,3] = REFORM( 0.5 * ( stksoutl[*,*,*,3] + stksoutr[*,*,*,3] ) )

;-------------------------------------------------
;; Done
;-------------------------------------------------

END
