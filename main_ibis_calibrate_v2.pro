;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;  Procedure: MainIbisCalibrate
;
;  Purpose: Calibrate an IBIS scan (detsretched and aligned) for
;           telescope and instrument polarization effects.
;           also included is removal of crosstalk I->Q,U,V
;           and residual crosstalk from V->Q,U and Q,U->V.
;
;
;  ali@nso.edu February 2007
; mod Nov 2014 for response matrix LK
; mod June 2015: added wl_obs to save variables
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO main_ibis_calibrate_v2, lambda, date, cregion, wregion, $
                         thresh, fovr, dfile, tfile, xfile1, xfile2, opath, $
                         MASK = mask, REGION = region

;-------------------------------------------------
;; Load X-Matrices
;-------------------------------------------------

     RESTORE, xfile1, /verb
     xmtxl = xmat
     RESTORE, xfile2, /verb
     xmtxr = xmat

;-------------------------------------------------
;; Load T-Matrix
;-------------------------------------------------

     RESTORE, tfile, /verb
     
;-------------------------------------------------
;;Load data cube
;-------------------------------------------------

     RESTORE, dfile, /verb
     s = SIZE(nbdwc)

;-------------------------------
;; Calculate T-Matrix
;-------------------------------

     az  = mean( tp_azim  )
     el  = mean( tp_elev  )
     tbl = mean( tp_table )
     solarpee = mean( tp_pee )

     VTTangles = [az,el,tbl]

   ;t matrix is sometimes called tt and sometimes ttnew...
      year = fix(strmid(date,0,4))
      month = fix(strmid(date,4,2))
       if year le 2009 then begin
           if month le 5 then constructtmatrix,ttnew,vttangles,lambda,tmtx $ ;old t matrix format
                 else constructtmatrix_new,tt,vttangles,lambda,tmtx
        endif else begin
         constructtmatrix_new,tt,vttangles,lambda,tmtx
        endelse
    
 ;    CONSTRUCTTMATRIX, ttt, VTTangles, lambda, tmtx, /nonorm
 ;   ttt = 0.

;-------------------------------
;; Demodulate
;-------------------------------

;do not demodulate!!!
   ;  ibis_demodulate_scan, nbdwc, modulation, stokesin
   ;  s = SIZE(stokesin)
    ;output is [x,y,lambda,4,{left,right}]

;-------------------------------
;; Do calibration 
;-------------------------------

     print,'Calibration starts: ', dfile

     ;wlobs are wl steps of calibration
     ;wlscan are wl steps of scans
     wlscan = diffelement(nb_expos.grid,/names,/double)
     range=[ya,yb]
     if lambda eq 8542 then ibis_scan_calibrate_xt_v2, nbdwc, xmtxl, xmtxr, tmtx, stokesout,range,wlobs,wlscan,/avg else $
        ibis_scan_calibrate_xt_v2, nbdwc, xmtxl, xmtxr, tmtx, stokesout,range,wlobs,wlscan,/avg
;/avg should work for all lambda. The above line was used for testing of 6302
;(where nothing works for March 29, 2014)


    ;output is [x,y,lambda,4]

     stokesin = stokesout


     IF KEYWORD_SET(MASK) THEN BEGIN

        tpol = total(sqrt((reform(stokesin[*,*,*,1])/reform(stokesin[*,*,*,0]))^2. + $ 
                          (reform(stokesin[*,*,*,2])/reform(stokesin[*,*,*,0]))^2. + $ 
                          (reform(stokesin[*,*,*,3])/reform(stokesin[*,*,*,0]))^2.),3) 

 ;;       mask = (tpol LT 0.25) * (tpol ne tpol[10,10]) ;; 6302
        mask = (tpol LT 0.3) * (tpol ne tpol[10,10]) ;; 8542

        window,2, xs = 2*256, ys = 512, title='Left: total polarization (logarithmic); Right: mask for I->QUV xtalk'
        tvscl, alog10(tpol), 0,0
        tvscl, mask, 256,0

        ibis_scan_xtalk_i2quv, stokesin, cregion, stokesout, polfac, $ 
                               MASK = mask

     ENDIF

     IF KEYWORD_SET(REGION) THEN BEGIN
;uses quiet sun to correct for I-> Q, U, V crosstalk
        print,"yes, we chose region (if this contains zero, it's probably bad...):",cregion
        ibis_scan_xtalk_i2quv, stokesin, cregion, stokesout, polfac, $ 
                               REGION = region

     ENDIF

;;   stokesin = stokesout

;;   ibis_scan_xtalk, stokesin, thresh, wregion, fovr, stokesout, xtalk

;-------------------------------
;; Rotate into correct RF on Sun
;-------------------------------

     DST_earth_latitude = 32.786 * !dtor
     r_frame_sphtri, DST_earth_latitude , az * !dtor, el * !dtor, $
                     siteha, sitedc, neg_parallactic
     parallactic = -neg_parallactic

     rotn = !pi + parallactic - solarpee * !dtor
     crot = cos(-2.*rotn) 
     srot = sin(-2.*rotn)

     temp0 =  crot*reform(stokesout[*,*,*,1]) + srot*reform(stokesout[*,*,*,2])
     temp1 = -srot*reform(stokesout[*,*,*,1]) + crot*reform(stokesout[*,*,*,2])

     stokesout[*,*,*,1] = temp0
     stokesout[*,*,*,2] = temp1

;-------------------------------
;; Show result
;-------------------------------

;modified lk, highly annoying to block the screen every 20 s
;     window, 0, xs = 3*s[1], ys = s[2], title='Blue Wing'
;     tvscl, reform(stokesout[*,*,15,1]),0,0
;     tvscl, reform(stokesout[*,*,15,2]),s[1],0
;     tvscl, reform(stokesout[*,*,15,3]),2*s[1],0

;-------------------------------
;; Store output
;-------------------------------

     file = (STRMID(dfile, (STRSPLIT(dfile, '/'))))[N_ELEMENTS(STRSPLIT(dfile, '/'))-1]

     outfilename = opath + '/' + (strmid(file, 0, (STRSPLIT(file, '.')))) $ 
                                             [N_ELEMENTS(STRSPLIT(file, '.'))-1] + 'pc.tatb.sav'

     SAVE, /VERB, /COMP, FILENAME = outfilename, $ 
           mwld, simage, stokesout, xtalk, polfac, nb_expos, mask, wl_obs, $
           tp_azim, tp_elev, tp_table, tp_pee, tp_llevel, tp_see, tp_slat, tp_slng, modulation
    
     print,'Done: ', outfilename

;-------------------------------------------------
;; Done
;-------------------------------------------------

end
