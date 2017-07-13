;+
; NAME:      IBIS_POLCAL_XCALC
;
; PURPOSE:   
;
; NOTES:     
;
; 
; MODIFICATION HISTORY:  Written by Tom Schad - UA/NSO - 11 April 2012
;                        logging added, may 2012, lk (x-matrices in .tex)
;    mod: Jan 2014 for fewer than 4 table positions - nothing to be done?                   
;    mod: Nov 2014 for y-dependent and wavelength-dependent X matrix
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

FUNCTION ibis_polcal_initialize

    PRINT,'----------------------------------------------------'
    PRINT,' SETTING UP VARIABLES AND INITIALIZING VALUES       '
    PRINT,' FOR THE DEFAULT IBIS POLCAL CASE  '
    PRINT,' '
 
    ;**********************************************************
    ; SET TOTAL NUMBER OF FREE PARAMETERS
    ;**********************************************************

    ; -->16 values for instrumental Mueller matrix
    ; -->Calibration WP retardance, offset at zero, dichroism
    ; -->Assume ZERO extinction ratio of calibration LP
    ; -->Assume LP completely polarizes the incoming beam such that 

    num_para = 16. + 3. + 1.

    PRINT,' TOTAL NUMBER OF PARAMETERS IN FIT: ', FIX(num_para)
    PRINT,' '

    ;**********************************************************
    ; CREATE PARINFO STRUCTURE FOR MPFIT PROCEDURE
    ;**********************************************************
    
    parinfo = replicate({value:0.D, fixed:0, limited:[0,0], step:0.D, $
                     limits:[0.D,0],  mpside:0.D, parname:' ', mpmaxstep:0.D}, num_para)

    ;**********************************************************
    ; GIVE A STRING NAME TO EACH VARIABLE 
    ;**********************************************************

    parinfo[0].parname   = 'M_00        '
    parinfo[1].parname   = 'M_01        '
    parinfo[2].parname   = 'M_02        '
    parinfo[3].parname   = 'M_03        '
    parinfo[4].parname   = 'M_10        '
    parinfo[5].parname   = 'M_11        '
    parinfo[6].parname   = 'M_12        '
    parinfo[7].parname   = 'M_13        '
    parinfo[8].parname   = 'M_20        '
    parinfo[9].parname   = 'M_21        '
    parinfo[10].parname  = 'M_22        '
    parinfo[11].parname  = 'M_23        '
    parinfo[12].parname  = 'M_30        '
    parinfo[13].parname  = 'M_31        '
    parinfo[14].parname  = 'M_32        '
    parinfo[15].parname  = 'M_33        '
    parinfo[16].parname  = 'WP aoffset  '
    parinfo[17].parname  = 'WP retard   '
    parinfo[18].parname  = 'WP dichro   '
    parinfo[19].parname  = 'LP extinct  '

    ;**********************************************************
    ;
    ; GIVE INITIAL VALUE/RANGE FOR EACH VARIALBE
    ; --> ALSO FIX THE VARIABLES AS NECESSARY
    ; 
    ;**********************************************************
    
    ;----------------------------------------------------
    ; INSERT IDENTITY MATRIX IN FOR INITIAL MUELLER MATRIX
   
    mm0 = double(fltarr(4,4))
    FOR i = 0,3 DO mm0(i,i)=1.D
    mm0 = reform(mm0,16)
    parinfo[0:15].value = mm0

    ;----------------------------------------------------
    ; FIX M_00 TO 1 --> NORMALIZED MUELLER MATRIX

    parinfo[0].value        = 1.
    parinfo[0].fixed        = 1.

    ;----------------------------------------------------
    ; LIMIT ALL OTHER MATRIX ELEMENTS TO -1.1 TO 1.1 RANGE TO 
    ; OBTAIN AT LEAST A NEARLY PHYSICAL MUELLER MATRIX
    ; --> NOT LIMITED TO -1,1 FOR FIT FLEXIBILITY
    
    parinfo[1:15].limited   = [1,1] 
    parinfo[1:15].limits    = [-2.1,2.1]

    ;----------------------------------------------------
    ; LIMIT THE ANGLE OFFSET OF THE WP TO 40 DEGREE RANGE
    ; Additionally, set derivative step size to less than
    ; a degree (avoid wrap of retardance)
    
    parinfo[16].value     = 0.0 ; units are degrees
    parinfo[16].fixed     = 0.
    parinfo[16].limited   = [1,1]
    parinfo[16].limits    = [-20.0,20.0]
    parinfo[16].step      = [0.05]

    ;----------------------------------------------------
    ; LIMIT THE RETARDANCE OF THE WP TO A 40 DEGREE RANGE    
    ; Additionally, set derivative step size to less than
    ; a degree (avoid wrap of retardance)   

    parinfo[17].value     = 90.0 ; units are degrees
    parinfo[17].fixed     = 0.0
    parinfo[17].limited   = [1,1]
    parinfo[17].limits    = [45.0,135.0]
    parinfo[17].mpmaxstep = 0.0
    parinfo[17].step      = [0.05]

    ;----------------------------------------------------
    ; LIMIT THE DICHROISM OF THE WP 
        
    parinfo[18].value     = 0.0  ; dimensionless parameter
    parinfo[18].fixed     = 1.0
    parinfo[18].limited   = [1,1]
    parinfo[18].limits    = [-1.0,1.0]

    ;----------------------------------------------------
    ; FIX EXTINCTION RATIO OF THE LINEAR POLARIZER
    
    parinfo[19].value     = 0.0  ; dimensionless parameter
    parinfo[19].fixed     = 1.0
       
    PRINT,' HERE ARE THE SET UP PARAMETERS: '
    PRINT,' '
    PRINT,' PARAMETER NAME     VALUE   FIXED   LIMITS'
    FOR p = 0,num_para-1 DO print,' ',parinfo[p].parname,FIX(parinfo[p].value),parinfo[p].fixed,parinfo[p].limits
    PRINT,'-------------------------------------------------'

    ;**********************************************************
    ; RETURN INITIALIZED PARAMETERS
    ;**********************************************************
    
    RETURN,parinfo

END

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

FUNCTION ibis_polcal_function, angle_values, input_values
;function routine for mpfit

    COMMON tmatrix_vals, tfile_xcalc, flag_oldtcal_xcalc, lambda_xcalc
 
;angle_values is [80,7] for 4 table pos
    lp_stage0  = angle_values[*,0]
    wp_stage0  = angle_values[*,1]
    lp_angle0  = angle_values[*,2]
    wp_angle0  = angle_values[*,3]
    dst_az0 = angle_values[*,4]    
    dst_el0 = angle_values[*,5]
    dst_ta0 = angle_values[*,6]

;input_values is vector [20] with instrument matrix and wp/lp params, passed by parinfo?
    wp_aoff    = input_values[16]
    wp_ret     = input_values[17]
    wp_dicr    = input_values[18]
    lp_klp     = input_values[19]
    num_files  = N_ELEMENTS(lp_stage0)

    X_MM = REFORM([input_values[0:15]],4,4)

    S_PCAL_CALC=double(FLTARR(4,num_files))

    IF (MIN(lp_stage0) EQ 0) THEN RESTORE,tfile_xcalc

;num_files is 80 for 4 table positions
    FOR p=0,num_files-1. DO BEGIN  

        dqwp = DOUBLE((wp_ret)*!pi/180.d)             ; WP retardance
        aqwp = DOUBLE(wp_angle0[p]+wp_aoff)*!pi/180.d ; WP angle (w.r.t. zero pos)
        alp  = DOUBLE(lp_angle0[p])*!pi/180.d         ; LP angle (w.r.t. zero pos)

        ; FOR LINEAR POLARIZER ROTATED AT SOME ANGLE

        s2 = sin(2.d*alp)
        c2 = cos(2.d*alp)
         
        rot_mat_pos = DOUBLE(1.0d*[[1.,   0.,   0.,    0.],$
                                   [0.,   c2,   s2,    0.],$
                                   [0.,  -s2,   c2,    0.],$
                                   [0.,   0.,   0.,    1.]])
        
        mm_lp_nrot  = DOUBLE(0.5d*[[1.+lp_klp, 1-lp_klp,            0.,              0.],$
                                   [1.-lp_klp, 1+lp_klp,            0.,              0.],$
                                   [    0.,     0., 2.*sqrt(lp_klp),              0.],$
                                   [    0.,     0.,           0.,    2.*sqrt(lp_klp)]])
                    
        s2 = sin(-2.d*alp)
        c2 = cos(-2.d*alp)
        
        rot_mat_neg = DOUBLE(1.0d*[[1.,   0.,   0.,    0.],$
                                   [0.,   c2,   s2,    0.],$
                                   [0.,  -s2,   c2,    0.],$
                                   [0.,   0.,   0.,    1.]])   
   
        mm_lp = rot_mat_neg##mm_lp_nrot##rot_mat_pos ;muller matrix for polarizer

        ; FOR RETARDING WAVE PLATE ROTATED AT SOME ANGLE
        
        s2 = sin(2.d*aqwp)
        c2 = cos(2.d*aqwp)
        
        rot_mat_pos = double(1.0d*[[1.,   0.,   0.,    0.],$
                                   [0.,   c2,   s2,    0.],$
                                   [0.,  -s2,   c2,    0.],$
                                   [0.,   0.,   0.,    1.]])
        
        sin_phi = sin(dqwp)
        cos_phi = cos(dqwp)

        wp_mat_nrot = double(1.0d*[[       1., wp_dicr,            0.,             0.],$
                                   [ wp_dicr,        1.,            0.,             0.],$
                                   [       0.,       0.,  (1.-wp_dicr^2.)*cos_phi, (1.-wp_dicr^2.)*(-sin_phi)],$
                                   [       0.,       0.,  (1.-wp_dicr^2.)*sin_phi, (1.-wp_dicr^2.)*cos_phi]])
                 
        s2 = sin(-2.d*aqwp)
        c2 = cos(-2.d*aqwp)
        
        rot_mat_neg = DOUBLE(1.0d*[[1.,   0.,   0.,    0.],$
                                   [0.,   c2,   s2,    0.],$
                                   [0.,  -s2,   c2,    0.],$
                                   [0.,   0.,   0.,    1.]])   

        wp_mat =  rot_mat_neg##wp_mat_nrot##rot_mat_pos

        ; USE THE STOKES VECTOR MODULATED BY TELESCOPE

        IF (MIN(lp_stage0) EQ 0) THEN BEGIN 
            ;get T matrix if polarizer was not always in the beam
            az = dst_az0[p]
            el = dst_el0[p]
            ta = dst_ta0[p]
            
            RESTORE,tfile_xcalc ; DST calibration file from time of observation
            VTTangles = [az,el,ta]
            
            ttt = tt
            
            IF (FLAG_OLDTCAL_XCALC EQ 0)  THEN BEGIN 
                CONSTRUCTTMATRIX_NEW, ttt, VTTangles, lambda_xcalc, tmtx, /nonorm
            ENDIF ELSE BEGIN 	
                CONSTRUCTTMATRIX, ttt, VTTangles, lambda_xcalc, tmtx, /nonorm
            ENDELSE	
            
            T_MM = tmtx/tmtx[0,0]
            
        ENDIF ELSE BEGIN 
           ;if all measurements are with the
           ;polarizer in the beam, then use unity
           ;matrix for telescope matrix
            T_MM      = FLTARR(4,4)
            T_MM[0,0] = 1.
            T_MM[1,1] = 1.
            T_MM[2,2] = 1.
            T_MM[3,3] = 1.
        ENDELSE
        
        ; MODULATE THE UNPOLARIZED SOLAR LIGHT
   
        I_INPUT=[[1.d],[0],[0],[0]]
        
        IF (lp_stage0[p] eq 1.) AND (wp_stage0[p] eq 1.) THEN S_CAL_INPUT=double(X_MM ## wp_mat ## mm_lp ## T_MM ## I_INPUT)
        IF (lp_stage0[p] eq 1.) AND (wp_stage0[p] eq 0.) THEN S_CAL_INPUT=double(X_MM ##           mm_lp ## T_MM ## I_INPUT)
        IF (lp_stage0[p] eq 0.) AND (wp_stage0[p] eq 1.) THEN S_CAL_INPUT=double(X_MM ## wp_mat          ## T_MM ## I_INPUT)
        IF (lp_stage0[p] eq 0.) AND (wp_stage0[p] eq 0.) THEN S_CAL_INPUT=double(X_MM ##                    T_MM ## I_INPUT)
;     s_cal_input is the input stokes vector, for example .5,.5,0,0 for stokes Q        

  ;p counts the number of known input states (number of calibration measurements)
  ;=80 in case of ibis because of 4 table positions
        S_PCAL_CALC[0,p]=S_CAL_INPUT[0] ;s_pcal_calc = calculated stokes vector [4,80]
        S_PCAL_CALC[1,p]=S_CAL_INPUT[1]
        S_PCAL_CALC[2,p]=S_CAL_INPUT[2]
        S_PCAL_CALC[3,p]=S_CAL_INPUT[3]       
    
    ENDFOR

    S_PCAL = double(FLTARR(4,num_files))
    FOR j = 0,3 DO FOR n = 0,num_files-1 DO S_PCAL[j,n] = S_PCAL_CALC[j,n]
    S_PCAL_NO_NORM = S_PCAL
    FOR j = 1,3 DO S_PCAL[j,*]=S_PCAL[j,*]/S_PCAL[0,*] ; normalize by I
    FOR j = 0,0 DO S_PCAL[j,*]=S_PCAL[j,*]/S_PCAL[0,*] ; normalize by I

    calc_values = DOUBLE([reform(S_PCAL[0,*]),reform(S_PCAL[1,*]),reform(S_PCAL[2,*]),reform(S_PCAL[3,*])])

    RETURN, calc_values
;calc_values: [4,80] vector made into [320] vector with theoretical input states for each measurement

END

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PRO ibis_polcal_xcalc_v2, lambda, polcal_datafile, path1, SAVEFILE1 = savefile1, SAVEFILE2 = savefile2, $
	TFILENAME = tfile, FLAG_OLDTCAL = flag_oldtcal,plotting=plotting
        
     COMMON tmatrix_vals, tfile_xcalc, flag_oldtcal_xcalc, lambda_xcalc
     
     tfile_xcalc        = tfile	
     flag_oldtcal_xcalc = flag_oldtcal
     lambda_xcalc       = lambda
     
     ;;================================================
     ;; RESTORE POLCAL DATA AND CALIBRATION PARAMETERS
     ;;================================================
     
     RESTORE,polcal_datafile,/VERB
     
;mod [0] to [3], LK, nov 2014
     IF (SIZE(nbpolcal))[3] EQ 1 THEN BEGIN	
         n_beams = 1	
         PRINT,' polcal data contains measurements from only one beam'
         PRINT,' ASSUMING SINGLE BEAM POLARIMETRY...' 
     ENDIF ELSE BEGIN 	
         n_beams = 2		
         PRINT,' polcal data contains measurements from two beams'
         PRINT,' ASSUMING DUAL BEAM POLARIMETRY...'
     ENDELSE

    
     ;;================================================
     ;; RESTRICT THE USE OF THE MEASUREMENTS HERE.
     ;; 
     ;; -> To decouple telescope from instrument, only 
     ;; use the measurements where the port 4 linear 
     ;; polarizer is in the beam (and no darks)
     ;; 
     ;; -> Otherwise, the DST T matrix calibration is 
     ;; necessary
     ;;
     ;; -> Though, a comparison of the two methods yields 
     ;; very similar results (according to one time test)
     ;;================================================
     
     PRINT,' '
     PRINT,' restricting the polcal to only the measurements '
     PRINT,' when the calibration linear polarizer is in the '
     PRINT,' beam'

     w_2use = WHERE(pc_pt4_dstg EQ 0 and pc_pt4_pstg EQ 1.,nmeas)

     angle_values = FLTARR(nmeas,7)
     
     angle_values[*,0] = pc_pt4_pstg(w_2use)
     angle_values[*,1] = pc_pt4_rstg(w_2use)
     angle_values[*,2] = pc_pt4_pol(w_2use)
     angle_values[*,3] = pc_pt4_ret(w_2use)
     angle_values[*,4] = pc_azim(w_2use)
     angle_values[*,5] = pc_elev(w_2use)
     angle_values[*,6] = pc_table(w_2use)
     
     parinfo = ibis_polcal_initialize()
 
;----------------------- single beam ------------------------------------------    
     IF n_beams EQ 1 THEN BEGIN 
         
         PRINT,' CALCULATING X MATRIX FOR SINGLE BEAM POLARIMETRY'
         PRINT,' '
         PRINT,' THE POLCAL DATA IS ASSUMED TO BE ALREADY NORMALIZED'
         
         stks_i = nbpolcal[*,0]
         stks_q = nbpolcal[*,1]
         stks_u = nbpolcal[*,2]
         stks_v = nbpolcal[*,3]
         
stop  ;probably need to modify things for single beam and field dependent X, not done yet...
         meas_values  = DOUBLE([stks_i(w_2use),stks_q(w_2use),stks_u(w_2use),stks_v(w_2use)])
         
         mp_result    = MPFITFUN('ibis_polcal_function', angle_values, meas_values,$
                                 PARINFO=PARINFO, NITER=num_iter, MAXITER=50,XTOL=(1e-35),/QUIET)
         
         xmatrix_calc   = REFORM([mp_result(0:15)],4,4)
         input_values = mp_result
         calc_values  = ibis_polcal_function(angle_values,input_values)
         
         PRINT,'-----------------------------------------------'
         PRINT,' X MATRIX FOR SINGLE BEAM IS: '
         PRINT,' '
         PRINT,xmatrix_calc
         PRINT,' '
         PRINT,' Wave Plate Offset [degrees]:     ',mp_result[16]
         PRINT,' Wave Plate Retardance [degrees]: ',mp_result[17]
         PRINT,' '

         ;add in log
        log_add_xsingle,path1,xmatrix_calc,mp_result[16],mp_result[17],savefile1,lambda

         if keyword_set(plotting) then begin
         WINDOW,0,TITLE ='SINGLE BEAM POLCAL FIT'
         PLOT,meas_values[0:nmeas-1],psym=5,$
           ysty = 1, xsty = 1, $
           XTITLE = 'POLCAL MEASUREMENT NUM',$
           YTITLE = 'RELATIVE POLARIZATION',$
           YRANGE = [-1.1,1.1]
         OPLOT,meas_values[(nmeas*1.):(nmeas*2.)-1],psym=5
         OPLOT,meas_values[(nmeas*2.):(nmeas*3.)-1],psym=5
         OPLOT,meas_values[(nmeas*3.):(nmeas*4.)-1],psym=5
         
         OPLOT,calc_values[(nmeas*0.):(nmeas*1.)-1],linesty=3
         OPLOT,calc_values[(nmeas*1.):(nmeas*2.)-1],linesty=3
         OPLOT,calc_values[(nmeas*2.):(nmeas*3.)-1],linesty=3
         OPLOT,calc_values[(nmeas*3.):(nmeas*4.)-1],linesty=3
         endif
	
         IF KEYWORD_SET(SAVEFILE1) THEN BEGIN
             
             Xmat = xmatrix
             wp_aoffset = mp_result[16]
             wp_ret = mp_result[17]
             PRINT,'saving: ',savefile1
             SAVE, /VERB, /COMP, FILENAME = savefile1, $
               Xmat, meas_values,calc_values, wp_aoffset,wp_ret
             
         ENDIF ELSE PRINT,' NO RESULTS SAVED FOR SINGLE BEAM'
         
     ENDIF ELSE BEGIN 
;----------------------------- dual beam ----------------------------------------

        PRINT,' CALCULATING X MATRIX FOR DUAL BEAM POLARIMETRY'
        PRINT,' '
        PRINT,' THE POLCAL DATA IS ASSUMED TO BE ALREADY NORMALIZED'
        
        ;; LEFT BEAM 
;only use certain table pos
;        pos1 = findgen(28)
;        stks_i = nbpolcal[pos1,0,0]
;        stks_q = nbpolcal[pos1,1,0]
;        stks_u = nbpolcal[pos1,2,0]
;        stks_v = nbpolcal[pos1,3,0]
;        w_2use = w_2use[0:19]

;all table pos

        nwl = (size(nbpolcal))[4]
  FOR wl=0,nwl-1 do begin  ;all wavelengths
          print,'*** wl = ',wl,' of ',nwl-1,' ***'
   
         FOR yy=0,(size(nbpolcal))[5]-1 do begin  ;all y-pixels
            print,'*** yy = ',yy,' of ',(size(nbpolcal))[5]-1,' ***'

            stks_i = nbpolcal[*,0,0,wl,yy]
            stks_q = nbpolcal[*,1,0,wl,yy]
            stks_u = nbpolcal[*,2,0,wl,yy]
            stks_v = nbpolcal[*,3,0,wl,yy]
        
        ;measured intensities for all calibrations at 
        ;array with 320 entries  [80xI,80xQ,80xU,80xV]
            meas_values  = DOUBLE([stks_i(w_2use),stks_q(w_2use),stks_u(w_2use),stks_v(w_2use)])
        ;q,u,v is the same for all 4 table positions, diff max 0.006
        if (yy eq 0 and wl eq 0) then meas_values_left  = dblarr(n_elements(meas_values),yb-ya+1,nwl)
        if (yy eq 0 and wl eq 0) then calc_values_left = dblarr(n_elements(meas_values),yb-ya+1,nwl)

;angle_values is [80,7] with polarizer/retarder in/out (indices 0,1), their angles in deg (indices 2,3)
;turret azim, elev, and table position (?) (indices 4,5,6)
;indices 4,5,6 are currently not used because we assume the T matrix
;to be irrelevant when the linear polarizer is in.

;for first table position
;angle_values = angle_values[0:19,*]

;ibis_polcal_function returns vector with 320 entries, which is fitted to meas_values
            mp_result_left    = MPFITFUN('ibis_polcal_function', angle_values, meas_values,$
                                PARINFO=PARINFO, NITER=num_iter, MAXITER=50,XTOL=(1e-35),/QUIET)

            if (yy eq 0 and wl eq 0) then mp_res_left_tmp = fltarr(n_elements(mp_result_left),yb-ya+1,nwl)
            mp_res_left_tmp[*,yy,wl] = mp_result_left
        
            if (yy eq 0 and wl eq 0) then xmatrix_left = fltarr(4,4,yb-ya+1,nwl)
            xmatrix_left[*,*,yy,wl]   = REFORM([mp_result_left(0:15)],4,4)
            input_values = mp_result_left
            calc_values  = ibis_polcal_function(angle_values,input_values)
        
        PRINT,'-----------------------------------------------'
        PRINT,' X MATRIX FOR LEFT BEAM IS: '
        PRINT,' '
        PRINT,xmatrix_left[*,*,yy,wl]
        PRINT,' '
        PRINT,' Wave Plate Offset [degrees]:     ',mp_result_left[16]
        PRINT,' Wave Plate Retardance [degrees]: ',mp_result_left[17]
        PRINT,' '


        if keyword_set(plotting) then begin
        WINDOW,0,TITLE = 'LEFT BEAM POLCAL FIT'
        PLOT,meas_values[0:nmeas-1],psym=5,$
          ysty = 1, xsty = 1, $
          XTITLE = 'POLCAL MEASUREMENT NUM',$
          YTITLE = 'RELATIVE POLARIZATION',$
          YRANGE = [-1.1,1.1]
        OPLOT,meas_values[(nmeas*1.):(nmeas*2.)-1],psym=5
        OPLOT,meas_values[(nmeas*2.):(nmeas*3.)-1],psym=5
        OPLOT,meas_values[(nmeas*3.):(nmeas*4.)-1],psym=5
        
        OPLOT,calc_values[(nmeas*0.):(nmeas*1.)-1],linesty=3
        OPLOT,calc_values[(nmeas*1.):(nmeas*2.)-1],linesty=3
        OPLOT,calc_values[(nmeas*2.):(nmeas*3.)-1],linesty=3
        OPLOT,calc_values[(nmeas*3.):(nmeas*4.)-1],linesty=3
        endif

        meas_values_left[*,yy,wl] = meas_values
        calc_values_left[*,yy,wl] = calc_values
 
   
  
  
        ;; RIGHT BEAM 

;only use certain table pos
 ;       pos1 = findgen(28)
 ;       stks_i = nbpolcal[pos1,0,1]
 ;       stks_q = nbpolcal[pos1,1,1]
 ;       stks_u = nbpolcal[pos1,2,1]
 ;       stks_v = nbpolcal[pos1,3,1]

;all pos
     ;   stks_i = nbpolcal[*,0,1]
     ;   stks_q = nbpolcal[*,1,1]
     ;   stks_u = nbpolcal[*,2,1]
     ;   stks_v = nbpolcal[*,3,1]
  
;mod nov 2014 lk for y-dependence
        stks_i = nbpolcal[*,0,1,wl,yy] ;[pos, 4 stokes, 2 beams, wl point, y-pixel]
        stks_q = nbpolcal[*,1,1,wl,yy]
        stks_u = nbpolcal[*,2,1,wl,yy]
        stks_v = nbpolcal[*,3,1,wl,yy]
            
        meas_values  = DOUBLE([stks_i(w_2use),stks_q(w_2use),stks_u(w_2use),stks_v(w_2use)])
        if (yy eq 0 and wl eq 0) then meas_values_right  = dblarr(n_elements(meas_values),yb-ya+1,nwl)
        if (yy eq 0 and wl eq 0) then calc_values_right = dblarr(n_elements(meas_values),yb-ya+1,nwl)

        mp_result_right   = MPFITFUN('ibis_polcal_function', angle_values, meas_values,$
                                PARINFO=PARINFO, NITER=num_iter, MAXITER=50,XTOL=(1e-35),/QUIET)
        if (yy eq 0 and wl eq 0) then mp_res_right_tmp = fltarr(n_elements(mp_result_right),yb-ya+1,nwl)
        mp_res_right_tmp[*,yy,wl] = mp_result_right
    
        if (yy eq 0 and wl eq 0) then xmatrix_right = fltarr(4,4,yb-ya+1,nwl)
        xmatrix_right[*,*,yy,wl]   = REFORM([mp_result_right(0:15)],4,4)
        input_values = mp_result_right
        calc_values  = ibis_polcal_function(angle_values,input_values)
        
        PRINT,'-----------------------------------------------'
        PRINT,' X MATRIX FOR RIGHT BEAM IS: '
        PRINT,' '
        PRINT,xmatrix_right[*,*,yy,wl]
        PRINT,' '
        PRINT,' Wave Plate Offset [degrees]:     ',mp_result_right[16]
        PRINT,' Wave Plate Retardance [degrees]: ',mp_result_right[17]
        PRINT,' '


        if keyword_set(plotting) then begin
        WINDOW,1,TITLE = ' RIGHT BEAM POLCAL FIT'
        PLOT,meas_values[0:nmeas-1],psym=5,$
          ysty = 1, xsty = 1, $
          XTITLE = 'POLCAL MEASUREMENT NUM',$
          YTITLE = 'RELATIVE POLARIZATION',$
          YRANGE = [-1.1,1.1]
        OPLOT,meas_values[(nmeas*1.):(nmeas*2.)-1],psym=5
        OPLOT,meas_values[(nmeas*2.):(nmeas*3.)-1],psym=5
        OPLOT,meas_values[(nmeas*3.):(nmeas*4.)-1],psym=5
        
        OPLOT,calc_values[(nmeas*0.):(nmeas*1.)-1],linesty=3
        OPLOT,calc_values[(nmeas*1.):(nmeas*2.)-1],linesty=3
        OPLOT,calc_values[(nmeas*2.):(nmeas*3.)-1],linesty=3
        OPLOT,calc_values[(nmeas*3.):(nmeas*4.)-1],linesty=3
        endif  

        meas_values_right[*,yy,wl] = meas_values
        calc_values_right[*,yy,wl] = calc_values

 
     ENDFOR                           ;end y-dependence
      ENDFOR ;end wavelengths


set_plot,'ps'
device,filename='xmat_left_'+string(lambda,format='(I4)')+'.eps',/encaps,/color,xsize=20,ysize=15
loadct,0
plot,findgen(yb-ya+1)+ya,xmatrix_left[1,0,*],/xs,/ys,yrange=[-1,1],xtitle='y pixel',title='Left X-matrix',thick=2,xthick=2,ythick=2,charthick=2
for i=2,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_left[i,0,*,*],lines=i,thick=2
tek_color
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_left[i,1,*,*],lines=i,color=2,thick=2
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_left[i,2,*,*],lines=i,color=4,thick=2
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_left[i,3,*,*],lines=i,color=9,thick=2
al_legend,['[1,0]','[2,0]','[3,0]','[0,1]','[1,1]','[2,1]','[3,1]','[0,2]','[1,2]','[2,2]','[3,2]','[0,3]','[1,3]','[2,3]','[3,3]'],$
 color=[0,0,0,2,2,2,2,4,4,4,4,9,9,9,9],lines=[0,2,3,0,1,2,3,0,1,2,3,0,1,2,3],thick=fltarr(15)+2,pspacing=1.3,position=[400,1.]
device,/close
set_plot,'x'


set_plot,'ps'
device,filename='xmat_right_'+string(lambda,format='(I4)')+'.eps',/encaps,/color,xsize=20,ysize=15
loadct,0
plot,findgen(yb-ya+1)+ya,xmatrix_right[1,0,*,*],/xs,/ys,yrange=[-1,1],xtitle='y pixel',title='Right X-matrix',thick=2,xthick=2,ythick=2,charthick=2
for i=2,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_right[i,0,*,*],lines=i,thick=2
tek_color
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_right[i,1,*,*],lines=i,color=2,thick=2
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_right[i,2,*,*],lines=i,color=4,thick=2
for i=0,3 do oplot,findgen(yb-ya+1)+ya,xmatrix_right[i,3,*,*],lines=i,color=9,thick=2
al_legend,['[1,0]','[2,0]','[3,0]','[0,1]','[1,1]','[2,1]','[3,1]','[0,2]','[1,2]','[2,2]','[3,2]','[0,3]','[1,3]','[2,3]','[3,3]'],$
 color=[0,0,0,2,2,2,2,4,4,4,4,9,9,9,9],lines=[0,2,3,0,1,2,3,0,1,2,3,0,1,2,3],thick=fltarr(15)+2,pspacing=1.3,position=[400,1.]
device,/close
set_plot,'x'


    ;add to log (use last calculated X matrix)
        log_add_xdouble,path1,xmatrix_left[*,*,yy-1],mp_result_left[16],mp_result_left[17],savefile1,$
                 xmatrix_right[*,*,yy-1],mp_result_right[16],mp_result_right[17],savefile2,lambda
 
      
        IF KEYWORD_SET(SAVEFILE1) THEN BEGIN
         
            Xmat = xmatrix_left
            wp_aoffset = mp_res_left_tmp[16,*]
  ;          wp_ret = mp_result_left[17]
            wp_ret = mp_res_left_tmp[17,*]
            PRINT,'saving: ',savefile1
            SAVE, /VERB, /COMP, FILENAME = savefile1, $
              Xmat, meas_values_left,calc_values_left, wp_aoffset,wp_ret,ya,yb,wlobs
            
        ENDIF ELSE PRINT,' NO RESULTS SAVED FOR LEFT BEAM'
        
        IF KEYWORD_SET(SAVEFILE2) THEN BEGIN
            
            Xmat = xmatrix_right
    ;        wp_aoffset = mp_result_right[16]
    ;        wp_ret = mp_result_right[17]
           wp_aoffset = mp_res_right_tmp[16,*]
            wp_ret = mp_res_right_tmp[17,*]
    
            PRINT,'saving: ',savefile2
            SAVE, /VERB, /COMP, FILENAME = savefile2, $
              Xmat, meas_values_right,calc_values_right, wp_aoffset,wp_ret,ya,yb,wlobs
            
        ENDIF ELSE PRINT,' NO RESULTS SAVED FOR RIGHT BEAM'

     ENDELSE 

END
