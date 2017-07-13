;+
; NAME:      IBIS_GET_POLCALCURVE
;
; PURPOSE:   To process IBIS polcal measurements by performing dark 
;            correction and demodulation for the polarimeter applied
;            modulation sequence.
;
; NOTES:     This routines marks a deviation from the previous IBIS 
;            polcal curve determination.  In this process we dark
;            correct and demodulate each modulation sequence and then
;            coadded the relative polarization in the different
;            wavelength points for a given spectral channel.  
;  
;            This makes the procedure a bit more resilent to light
;            level variations during the polcal sequence.  The general
;            assumption is that the light level remains mostly
;            constant during one modulation sequence (i.e. 6 frames at
;            one setting of the calibration unit and one wavelength
;            point).  As this measurement usually occurs at rates
;            faster than the light level of the DST is updated, I
;            would argue that this procedure does as well (and
;            probably better) than using the lightlevel to correct for
;            transmission variations.  
; 
;            The one caveat that must be made is that in fitting for
;            the polarized response matrix, one cannot fit for the
;            transmission of the polarimeter.  Thus, only the
;            normalized Mueller matrix of the instrument can be
;            calculated.  Although the old ASP code can be difficult
;            to read, this seems to be the same restriction.  In that
;            code the output stokes vector is taken as [1., Q/I, U/I,
;            V/I], which requires the mueller matrix transmission to
;            be set to 1.  (i.e. normalized)
; 
; MODIFICATION HISTORY:  Written by Tom Schad - UA/NSO - 11 April 2012
;                        Extended from code from Lucia Kleint and Ali Tritschler
;   mod Jan 2014: not all 4 table positions are required
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
PRO ibis_get_polcalcurveb, lambda, path, tpfile, region, dfilen, sfile, path1, $
                          SAVEFILE = savefile,flagncam=flagncam

     IF NOT KEYWORD_SET(flagncam) THEN flagncam=0

;;***************************************************************
;; Path to polcals at different table angles
;;***************************************************************

     spath2 = 'polcal_15/'
     spath3 = 'polcal_60/'
     spath4 = 'polcal_105/'
     spath1 = 'polcal_330/'

;;***************************************************************
;; Parameters
;;***************************************************************

     xa = region[0] & xb = region[2]
     ya = region[1] & yb = region[3]
     dxy = region[4]

;;***************************************************************
;; Load data base file
;;***************************************************************
     
     PRINT,'----------------------------------------'
     PRINT, 'Reading: ', tpfile
     PRINT,'----------------------------------------'
     
     if file_test(tpfile) then RESTORE, /VERB, tpfile

;;***************************************************************
;; Load dark calibration file
;;***************************************************************

     RESTORE,/VERB, dfilen
     ; make one mean dark
   ;;;  darkmean = TOTAL(nb_dark,3)/((SIZE(nb_dark,/DIM))[2])
     darkmean = dark

;;***************************************************************
;; Locate the calibration measurements
;;***************************************************************

     fn1 = file_search(path + spath1, lambda + '*.sav',count=f1)
     fn2 = file_search(path + spath2, lambda + '*.sav',count=f2)
     fn3 = file_search(path + spath3, lambda + '*.sav',count=f3)
     fn4 = file_search(path + spath4, lambda + '*.sav',count=f4)

     npos = round(f1+f2+f3+f4)/28.  ;number of table pos

     fn = [fn1, fn2, fn3, fn4]
     ;find empty entries
     index = where(fn ne '')
     fn = fn[index]
 
     num = 28                         
     ntable = npos
     
     ;this should never be executed now
     IF (N_ELEMENTS(fn) NE num*ntable) THEN BEGIN
         PRINT,' '
         PRINT,' Number of Files Found: ',N_ELEMENTS(fn)
         PRINT,' ' 
         PRINT,' Number does not match 4 table angles with 28 calibration'
         PRINT,' unit settings per table angle.  The procedure will try to '
         PRINT,' fit the files that are found. '
         PRINT,' '
         PRINT,' DO YOU WANT TO CONTINUE WITH CALCUATION ANYWAY (y or n) ?', ans
         IF ans EQ 'y' THEN PRINT,' Ok. continuing...' ELSE MESSAGE,' Exiting...'
     ENDIF
     
;;***************************************************************
;; Polarimetry Parameters
;;***************************************************************

     ;; restore dummy file
     RESTORE, fn[0]
     npol = N_ELEMENTS(unique_elements((nb_expos.stokes)[where(nb_expos.stokes NE '')]))
     if npol gt 6 then npol=6 ;for log file errors which sometimes create V instead of I+V ->
     ;;should not happen anymore. problem was log file that was read incorrectly
     modulation = (nb_expos.stokes)[0:npol-1]
	;; calling this nmeas instead of nwave just in case a wavelength got repeated
     nmeas = (SIZE(nb_data))[3] / npol  

     PRINT,'---------------------------------------'
     PRINT,'Number of modulation states : ', npol
     PRINT,'Modulation scheme : ', modulation
     PRINT,'Number of table pos: ', npos
     PRINT,'---------------------------------------'
     
;;***************************************************************
;; Define output arrays
;;***************************************************************

     ;; nbpolcal will hold normalized stokes vector for each setting
     ;; of the Port 4 Calibration Unit (CU)
     ;;
     ;; Dimensions:  < CU Settings, 4 stokes states, 2 beams, Number
     ;; of wavelengths and/or modulation measurements>
     
     nbpolcal  = FLTARR(N_ELEMENTS(fn), 4, 2, nmeas)   ; I,Q,U,V (two beams)
     ;mod LK, Nov 2014; field dependent X
     nbpolcal  = FLTARR(N_ELEMENTS(fn), 4, 2, nmeas, yb-ya+1)   ; I,Q,U,V (two beams)
     ;nmeas is number of wavelengths of polcal

     pc_azim     = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_elev     = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_table    = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_llevel   = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_pt4_pstg = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_pt4_pol  = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_pt4_rstg = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_pt4_ret  = FLTARR(N_ELEMENTS(fn),nmeas)
     pc_pt4_dstg = FLTARR(N_ELEMENTS(fn),nmeas)

;;***************************************************************
;; Loop through the calibration files and combine with
;; telescope geometry information.
;; also average over wavelength and FOV.
;;***************************************************************


     FOR i = 0, N_ELEMENTS(fn) - 1 DO BEGIN
         
         PRINT,' Reading File: ',i,' of ',N_ELEMENTS(fn)-1
         PRINT,' Filename: ', fn[i]
         RESTORE, fn[i]

         FOR k = 0, nmeas - 1 DO BEGIN ;5 wl points for 6302
             
             IF (npol eq 6) THEN BEGIN 

                 mod_k = (nb_expos.stokes)[k*npol:(k*npol)+npol-1]
                 
                 wipq = WHERE( mod_k EQ 'I+Q' )
                 wimq = WHERE( mod_k EQ 'I-Q' )
                 wipv = WHERE( mod_k EQ 'I+V' )
                 wimv = WHERE( mod_k EQ 'I-V' )
                 wipu = WHERE( mod_k EQ 'I+U' )
                 wimu = WHERE( mod_k EQ 'I-U' )
                 
                 IF (wipq[0] EQ -1) THEN BEGIN 
                     PRINT,'ipq index not correct! have to improvise ...'
                     wipq[0] = 0
                 ENDIF 
                 
                 IF (wimq[0] EQ -1) THEN BEGIN 
                     PRINT,'imq index not correct! have to improvise ...'
                     wimq[0] = 2
                 ENDIF
                 
                 IF (wipv[0] EQ -1) THEN BEGIN 
                     PRINT,'ipv index not correct! have to improvise ...'
                     wipv[0] = 1
                 ENDIF
                 
                 IF (wimv[0] EQ -1) THEN BEGIN 
                     PRINT,'imv index not correct! have to improvise ...'
                     wimv[0] = 3
                 ENDIF
                 
                 IF (wipu[0] EQ -1) THEN BEGIN
                     PRINT,'ipu index not correct! have to improvise ...'
                     wipu[0] = 5
                 ENDIF
                 
                 IF (wimu[0] EQ -1) THEN BEGIN 
                     PRINT,'imu index not correct! have to improvise ...'
                     wimu[0] = 4
                 ENDIF
                 
                 LEFT = 0 & RIGHT = 1

                 ; FIRST FOR THE LEFT BEAM  
                 
                                ;dark correction, flats should not be
                                ;necessary because S/I is used
                 ipq = nb_data[xa:xb, ya:yb,k*npol + wipq] - darkmean[xa:xb, ya:yb]
                 imq = nb_data[xa:xb, ya:yb,k*npol + wimq] - darkmean[xa:xb, ya:yb]
                 ipv = nb_data[xa:xb, ya:yb,k*npol + wipv] - darkmean[xa:xb, ya:yb]
                 imv = nb_data[xa:xb, ya:yb,k*npol + wimv] - darkmean[xa:xb, ya:yb]
                 ipu = nb_data[xa:xb, ya:yb,k*npol + wipu] - darkmean[xa:xb, ya:yb]
                 imu = nb_data[xa:xb, ya:yb,k*npol + wimu] - darkmean[xa:xb, ya:yb]

                 sil = (ipq + imq + ipv + imv + ipu + imu)/6. ;avg I for mod cycle
                 sql = 0.5*(ipq - imq)
                 sul = 0.5*(ipu - imu)
                 svl = 0.5*(ipv - imv)

                  ;averaging over FOV (k counts the wavelengths)
;                 nbpolcal[i, 0, LEFT, k] += 1.
;                 nbpolcal[i, 1, LEFT, k] += MEAN(sql/sil)
;                 nbpolcal[i, 2, LEFT, k] += MEAN(sul/sil)
;                 nbpolcal[i, 3, LEFT, k] += MEAN(svl/sil)


FOR yy=0,(yb-ya) do begin
;mod Nov 2014 lk, only average over x, not y
                  ;averaging over FOV (k counts the wavelengths)
                 nbpolcal[i, 0, LEFT, k, yy] = 1.
                 nbpolcal[i, 1, LEFT, k, yy] = MEAN(sql[*,yy]/sil[*,yy])
                 nbpolcal[i, 2, LEFT, k, yy] = MEAN(sul[*,yy]/sil[*,yy])
                 nbpolcal[i, 3, LEFT, k, yy] = MEAN(svl[*,yy]/sil[*,yy])
ENDFOR

                 ; NOW FOR RIGHT BEAM
                 
                 ipq = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wipq] - darkmean[xa+dxy:xb+dxy, ya:yb]
                 imq = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wimq] - darkmean[xa+dxy:xb+dxy, ya:yb]
                 ipv = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wipv] - darkmean[xa+dxy:xb+dxy, ya:yb]
                 imv = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wimv] - darkmean[xa+dxy:xb+dxy, ya:yb]
                 ipu = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wipu] - darkmean[xa+dxy:xb+dxy, ya:yb]
                 imu = nb_data[xa+dxy:xb+dxy, ya:yb,k*npol + wimu] - darkmean[xa+dxy:xb+dxy, ya:yb]

                 sir = (ipq + imq + ipv + imv + ipu + imu)/6.
                 sqr = -(0.5*(ipq - imq))
                 sur = -(0.5*(ipu - imu))
                 svr = -(0.5*(ipv - imv))

;                 nbpolcal[i, 0, RIGHT, k] += 1.
;                 nbpolcal[i, 1, RIGHT, k] += MEAN(sqr/sir)
;                 nbpolcal[i, 2, RIGHT, k] += MEAN(sur/sir)
;                 nbpolcal[i, 3, RIGHT, k] += MEAN(svr/sir)
 
;6302: sqr/sir looks flat, sur/sir too, svr/sir has gradient in y


FOR yy=0,(yb-ya) do begin
;mod Nov 2014 lk, only average over x, not y
                  ;averaging over FOV (k counts the wavelengths)
                 nbpolcal[i, 0, RIGHT, k, yy] = 1.
                 nbpolcal[i, 1, RIGHT, k, yy] = MEAN(sqr[*,yy]/sir[*,yy])
                 nbpolcal[i, 2, RIGHT, k, yy] = MEAN(sur[*,yy]/sir[*,yy])
                 nbpolcal[i, 3, RIGHT, k, yy] = MEAN(svr[*,yy]/sir[*,yy])
ENDFOR

                
                 ;; here is a temporary identification
                 ;; of polcal darks...a better way would
                 ;; be to use the pt4 info
                 IF MEAN([sil,sir] LT 50.) THEN pc_pt4_dstg(i,k) = 1.
                 

           
                     azim_k     = info_polcal.dst_az[k*npol:(k*npol)+npol-1]
                     elev_k     = info_polcal.dst_el[k*npol:(k*npol)+npol-1]
                     table_k    = info_polcal.dst_tbl[k*npol:(k*npol)+npol-1]
                     llevel_k   = info_polcal.dst_llevel[k*npol:(k*npol)+npol-1]
                     pt4_pstg_k = info_polcal.pt4_pstg[k*npol:(k*npol)+npol-1]
                     pt4_pol_k  = info_polcal.pt4_pol[k*npol:(k*npol)+npol-1]
                     pt4_rstg_k = info_polcal.pt4_rstg[k*npol:(k*npol)+npol-1]
                     pt4_ret_k  = info_polcal.pt4_ret[k*npol:(k*npol)+npol-1]

                     pt4_pstg_k_fl = FLTARR(6)
                     FOR l=0,5 DO IF STRTRIM(pt4_pstg_k[l],2) EQ 'IN' THEN pt4_pstg_k_fl[l] = 1.
                     pt4_rstg_k_fl = FLTARR(6)
                     FOR l=0,5 DO IF STRTRIM(pt4_rstg_k[l],2) EQ 'IN' THEN pt4_rstg_k_fl[l] = 1.
                     
                     test_unique = [N_ELEMENTS(UNIQUE_ELEMENTS(pt4_pstg_k_fl)),$
                                    N_ELEMENTS(UNIQUE_ELEMENTS(pt4_pol_k)),$
                                    N_ELEMENTS(UNIQUE_ELEMENTS(pt4_rstg_k_fl)),$
                                    N_ELEMENTS(UNIQUE_ELEMENTS(pt4_ret_k))]

                     IF (MAX(test_unique) GT 1) THEN BEGIN
                         PRINT,' '
                         PRINT,' PORT 4 HEADER VALUES NOT CONSISTENT FOR THIS SINGLE MODULATION SEQUENCE'
                         PRINT,' ENSURE QUALITY OF DATA HEADERS AND/OR SELECTED DATA '
                         PRINT,' '
                         MESSAGE,' Exiting...'                        
                     ENDIF
                     
                     pc_azim[i,k]     = MEAN(azim_k)
                     pc_elev[i,k]     = MEAN(elev_k)
                     pc_table[i,k]    = MEAN(table_k)
                     pc_llevel[i,k]   = MEAN(llevel_k)                    
                     pc_pt4_pstg[i,k] = MEAN(pt4_pstg_k_fl)
                     pc_pt4_pol[i,k]  = MEAN(pt4_pol_k)
                     pc_pt4_rstg[i,k] = MEAN(pt4_rstg_k_fl)
                     pc_pt4_ret[i,k]  = MEAN(pt4_ret_k)
                 
             ENDIF ELSE BEGIN ;npol ne 6
                 PRINT,' '
                 PRINT,' Only measurements with 6 modulation states currently supported'
                 MESSAGE,' Exiting...'
              ENDELSE

             
          ENDFOR ;k
         
      ENDFOR ;i


;;***************************************************************
;; AVERAGE ALL THE VALUES OVER THE NUMBER OF WAVELENGTHS, ETC
;;***************************************************************


;     nbpolcal       = TOTAL(nbpolcal,4)/nmeas ;do not average wavelengths
     pc_azim        = TOTAL(pc_azim,2)/nmeas
     pc_elev        = TOTAL(pc_elev,2)/nmeas
     pc_table       = TOTAL(pc_table,2)/nmeas
     pc_llevel_med  = MEDIAN(pc_llevel,DIM = 2)
     pc_llevel_max  = MAX(pc_llevel,DIM = 2 )
     pc_llevel_min  = MIN(pc_llevel,DIM = 2 )

     ;LK, mod Nov 2014, wavelength offsets for polcal
     wlobs = (nb_expos.wavelength_offset)[0:*:npol]

;----- write log ------
sampleimg = nb_data[*,*,0]
log_add_pccurve,path1,lambda,region,nbpolcal,savefile,sampleimg,pc_llevel_med

     test_unique = FLTARR(N_ELEMENTS(fn))
     FOR i = 0, N_ELEMENTS(fn) - 1 DO test_unique[i] = N_ELEMENTS(UNIQUE_ELEMENTS(pc_pt4_pstg[i,*]))
     IF (MAX(test_unique) GT 1) THEN MESSAGE,' PT4 PSTG values not the same for all measured measured wavelengths'
     
     FOR i = 0, N_ELEMENTS(fn) - 1 DO test_unique[i] = N_ELEMENTS(UNIQUE_ELEMENTS(pc_pt4_pol[i,*]))
     IF (MAX(test_unique) GT 1) THEN MESSAGE,' PT4 POL values not the same for all measured measured wavelengths'
   
     FOR i = 0, N_ELEMENTS(fn) - 1 DO test_unique[i] = N_ELEMENTS(UNIQUE_ELEMENTS(pc_pt4_rstg[i,*]))
     IF (MAX(test_unique) GT 1) THEN MESSAGE,' PT4 RSTG values not the same for all measured measured wavelengths'
   
     FOR i = 0, N_ELEMENTS(fn) - 1 DO test_unique[i] = N_ELEMENTS(UNIQUE_ELEMENTS(pc_pt4_ret[i,*]))
     IF (MAX(test_unique) GT 1) THEN MESSAGE,' PT4 RET values not the same for all measured measured wavelengths'

     FOR i = 0, N_ELEMENTS(fn) - 1 DO test_unique[i] = N_ELEMENTS(UNIQUE_ELEMENTS(pc_pt4_dstg[i,*]))
     IF (MAX(test_unique) GT 1) THEN MESSAGE,' Dark Stage values not the same for all measured measured wavelengths'
 
     pc_pt4_pstg = MEDIAN(pc_pt4_pstg, DIM = 2)
     pc_pt4_pol  = MEDIAN(pc_pt4_pol, DIM = 2)
     pc_pt4_rstg = MEDIAN(pc_pt4_rstg, DIM = 2)
     pc_pt4_ret  = MEDIAN(pc_pt4_ret, DIM = 2)
     pc_pt4_dstg = MEDIAN(pc_pt4_dstg, DIM = 2)
     
     PRINT,' '
     PRINT,' Here is some information on the DST light level during polcal acquistion...'
     PRINT,' '
     PRINT,' Each modulation sequence at a specific wavelength is demodulated and normalized to '
     PRINT,' its own average intensity.  This should minimize and/or get rid of problems due to '
     PRINT,' light level variations on time scales longer than the modulation sequence for one wavelength'
     PRINT,' '
     PRINT,' Light level variations on time scales shorter than modulation cannot be corrected'
     PRINT,' '
     PRINT,' The plot shows the light levels reported for all measurements at a specific setting of the '
     PRINT,' Port 4 calibration unit. The solid line is the median value, and the dotted lines '
     PRINT,' give min/max range values'
     PRINT,' '
  
     WINDOW,0,RET = 3,TITLE = ' DST LIGHT LEVEL VARIATIONS '

     PLOT,pc_llevel_med,$
       XSTY =1, $
       YSTY = 1,YRANGE = [MIN(pc_llevel_min),MAX(pc_llevel_max)],$
       XTITLE = ' Polcal Setting Number ',$
       YTITLE = ' DST Light Level'
     OPLOT,pc_llevel_max,LINESTY = 3
     OPLOT,pc_llevel_min,LINESTY = 3
        
;;***************************************************************
;; Save file
;;***************************************************************
  
     IF KEYWORD_SET(SAVEFILE) THEN BEGIN
         
        PRINT,'saving: ',savefile
        SAVE, /VERB, /COMP, FILENAME = savefile, $
          modulation, nbpolcal, pc_azim, pc_elev, pc_table, pc_llevel,$
          pc_pt4_pstg, pc_pt4_pol, pc_pt4_rstg, pc_pt4_ret, pc_pt4_dstg, yb, ya, xb, xa, wlobs
        
     ENDIF ELSE BEGIN

           PRINT,'No output saved!'

     ENDELSE

END
