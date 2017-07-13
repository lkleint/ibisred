;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;  Procedure: ibis_scan_calibrate_xt
;
;  Purpose: Calibrate an IBIS scan (detsretched and aligned) for
;           telescope and instrument polarization effects.
;
;
;  Input:  stksin -- 5 dimensional data cube
;                      dimensions = (x,y,6,lambda,beams)
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
; mod v2: use response matrix and svd inverse. some order of indices
; changed (stokes and lambda)
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_scan_calibrate_xt_v2, stksin, xmatl, xmatr, tmat, stksout, range, wlobs, wlscan,avg=avg
;keyword avg to average over whole x matrix input

;-------------------------------------------------
;; Create Arrays
;-------------------------------------------------

      s = SIZE(stksin)

      stksinl = REFORM( stksin[*,*,*,*,0] )
      stksinr = REFORM( stksin[*,*,*,*,1] )

      stksoutl = fltarr( s[1], s[2], s[4], 4)
      stksoutr = fltarr( s[1], s[2], s[4], 4)

      stksout = fltarr( s[1], s[2], s[4], 4)


;-------------------------------------------
; Smooth matrices, normalize data?
;--------------------------------------------

;normalize to first element
;no, do not do this, it creates fringes!
;   for ll=0,s[4]-1 do begin
;      for st=0,5 do begin
;          stksinl[*,*,st,ll] = stksinl[*,*,st,ll]/stksinl[*,*,0,ll]
;          stksinr[*,*,st,ll] = stksinr[*,*,st,ll]/stksinr[*,*,0,ll]
;       endfor
;   endfor

;ignore first point of xmatrix, it seems to be bad, at least in 6302
xmatl = xmatl[*,*,*,1:*]
xmatr = xmatr[*,*,*,1:*]
wlobs = wlobs[1:*]




if keyword_set(avg) then begin
   print,'Averaging X-matrix over FOV'
   xmatl_fit = avg(avg(xmatl,2),2)
   xmatr_fit = avg(avg(xmatr,2),2)
endif else begin
   print,'Smoothing X-matrix over FOV by 25 pixels'
   xmatl_fit = smooth(xmatl,[1,1,25,1],/edge_truncate)
   xmatr_fit = smooth(xmatr,[1,1,25,1],/edge_truncate)
endelse
 
;-------------------------------------------------
;; Invert matrices
;-------------------------------------------------

;mod LK Nov 2014 for FOV dependent X-matrix
     ny = range[1]-range[0]+1
     nwl = (size(xmatr))[4]
     nscan = n_elements(wlscan)


;get pseudo-inverse with SVD
      Xinv_right =fltarr(6,4,ny,nwl)
      Xinv_left = fltarr(6,4,ny,nwl)

if ~keyword_set(avg) then begin
print,'inverting X'
     ;invert matrices
     for j=0,nwl-1 do begin
       for i=0,ny-1 do begin
       Xinv_right[*,*,i,j] = INVERT_svd(xmatr_fit[*,*,i,j])
       Xinv_left[*,*,i,j]  = INVERT_svd(xmatl_fit[*,*,i,j])
       endfor
     endfor
  endif else begin
    tmp1 = INVERT_svd(xmatr_fit)
    tmp2 =  INVERT_svd(xmatl_fit)
    for j=0,nwl-1 do begin
       for i=0,ny-1 do begin
       Xinv_right[*,*,i,j] = tmp1
       Xinv_left[*,*,i,j]  = tmp2
       endfor
     endfor
 endelse





     ;interpolate for observed wavelengths
     xtmpL = fltarr(6,4,ny,nscan) ;right
     xtmpR = fltarr(6,4,ny,nscan) ;left

       for i=0,5 do begin
        for j=0,3 do begin           
           for yy=0,ny-1 do begin
             xtmpL[i,j,yy,*] = interpol(xinv_left[i,j,yy,*],wlobs,wlscan)
             xtmpR[i,j,yy,*] = interpol(xinv_right[i,j,yy,*],wlobs,wlscan)
           endfor
        endfor
       endfor



     Tinv = INVERT(Tmat) 
     Tinv = Tinv/Tinv[0,0]


;----------------------------------------------------
;; 5. Loop over data ---> Do Calibration for X and T
;----------------------------------------------------



     s = SIZE(stksinr)

     FOR i = 0, s[1] - 1 DO BEGIN  ;x direction
        if i mod 50 eq 0 then print,'x=',i

         FOR j = 0, s[2] - 1 DO BEGIN ;ydirection

     ;            print,(j-range[0])>0<(range[1]-range[0])

             FOR l = 0, s[4]-1 DO BEGIN ;lambda

                 ar = REFORM( stksinr[i,j,*,l] )  ;order of indices changed...
;mod LK Nov 2014, for y-dependent X-matrix
                 xinv_r_tmp = xtmpR[*,*,(j-range[0])>0<(range[1]-range[0]),l]
                 br = Xinv_r_tmp ## ar
                 cr = Tinv ## br
                 al = REFORM( stksinl[i,j,*,l] )
                 xinv_l_tmp = xtmpL[*,*,(j-range[0])>0<(range[1]-range[0]),l]
                 bl = Xinv_l_tmp ## al
                 cl = Tinv ## bl
                 stksoutl[i,j,l,*] = cl
                 stksoutr[i,j,l,*] = cr

             ENDFOR

         ENDFOR

     ENDFOR


;stop
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
