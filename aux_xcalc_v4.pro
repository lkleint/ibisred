;aux program for parallelization
;created Nov 2014
;mod 23.11.14 to average 2 points in y direction to make it faster
;### this works for 6 polstates (and nothing else)

;which_bridge is the index of the for loop

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
    wp_aoff    = input_values[24]
    wp_ret     = input_values[25]
    wp_dicr    = input_values[26]
    lp_klp     = input_values[27]
    num_files  = N_ELEMENTS(lp_stage0)

    X_MM = REFORM([input_values[0:23]],4,6)

    S_PCAL_CALC=double(FLTARR(6,num_files))

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
        S_PCAL_CALC[4,p]=S_CAL_INPUT[4]  ;new for response matrix       
        S_PCAL_CALC[5,p]=S_CAL_INPUT[5]        ;new
    
    ENDFOR

    S_PCAL = double(FLTARR(6,num_files))
    FOR j = 0,5 DO FOR n = 0,num_files-1 DO S_PCAL[j,n] = S_PCAL_CALC[j,n]
    S_PCAL_NO_NORM = S_PCAL
    FOR j = 1,5 DO S_PCAL[j,*]=S_PCAL[j,*]/S_PCAL[0,*] ; normalize by first measurement
    FOR j = 0,0 DO S_PCAL[j,*]=S_PCAL[j,*]/S_PCAL[0,*] ; normalize by first measurement

    calc_values = DOUBLE([reform(S_PCAL[0,*]),reform(S_PCAL[1,*]),reform(S_PCAL[2,*]),reform(S_PCAL[3,*]),reform(S_PCAL[4,*]),reform(S_PCAL[5,*])])

    RETURN, calc_values
;calc_values: [4,80] vector made into [320] vector with theoretical input states for each measurement

END

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PRO aux_xcalc_v4,nbpolcal,yb,ya,w_2use,angle_values,parinfo,i;,mp_res_left_tmp,mp_res_right_tmp,meas_values_left,meas_values_right,calc_values_left,calc_values_right,xmatrix_right,xmatrix_left

print,'Process: ',i
wl=i
nwl = (size(nbpolcal))[4]
 

nyy = (size(nbpolcal))[5]
   FOR yy=0.,nyy-1,2 do begin  ;all y-pixels
;   FOR yy=0,10 do begin  ;test with 10 pixels
            print,'*** yy = ',yy,' of ',(size(nbpolcal))[5]-1,' ***'

;average 2 steps in y to make it faster
            s0l = (nbpolcal[*,0,0,wl,yy] + nbpolcal[*,0,0,wl,(yy+1)<(nyy-1)] ) / 2.
            s1l = (nbpolcal[*,1,0,wl,yy] + nbpolcal[*,1,0,wl,(yy+1)<(nyy-1)] ) / 2.
            s2l = (nbpolcal[*,2,0,wl,yy] + nbpolcal[*,2,0,wl,(yy+1)<(nyy-1)] ) / 2.
            s3l = (nbpolcal[*,3,0,wl,yy] + nbpolcal[*,3,0,wl,(yy+1)<(nyy-1)] ) / 2.
            s4l = (nbpolcal[*,4,0,wl,yy] + nbpolcal[*,4,0,wl,(yy+1)<(nyy-1)] ) / 2.
            s5l = (nbpolcal[*,5,0,wl,yy] + nbpolcal[*,5,0,wl,(yy+1)<(nyy-1)] ) / 2.
        
            meas_values  = DOUBLE([s0l(w_2use),s1l(w_2use),s2l(w_2use),s3l(w_2use),s4l(w_2use),s5l(w_2use)])
            ;help,meas_values
            if (yy eq 0) then meas_values_left  = dblarr(n_elements(meas_values),yb-ya+1,nwl)
            if (yy eq 0) then calc_values_left = dblarr(n_elements(meas_values),yb-ya+1,nwl)
            if (yy eq 0) then chisql=dblarr(yb-ya+1,nwl)
            if (yy eq 0) then chisqr=dblarr(yb-ya+1,nwl)

;;*** testing ***
;            ;try fixing WP offset
;            parinfo[24].value        = 3.88
;            parinfo[24].fixed        = 1.
;            ;try fixing WP retardance
;            parinfo[25].value        = 80.9
;            parinfo[25].fixed        = 1.
;;*** end testing ***

            mp_result_left    = MPFITFUN('ibis_polcal_function', angle_values, meas_values,$
                                PARINFO=PARINFO, NITER=num_iter, MAXITER=50,XTOL=(1e-35),/QUIET,bestnorm=bestnorm)


            if (yy eq 0) then mp_res_left_tmp = fltarr(n_elements(mp_result_left),yb-ya+1,nwl)
            mp_res_left_tmp[*,yy,wl] = mp_result_left
            mp_res_left_tmp[*,(yy+1)<(nyy-1),wl] = mp_result_left
            chisql[yy,wl] = bestnorm
            chisql[(yy+1)<(nyy-1),wl] = bestnorm
        
            if (yy eq 0) then xmatrix_left = fltarr(4,6,yb-ya+1,nwl)
            xmatrix_left[*,*,yy,wl]   = REFORM([mp_result_left(0:23)],4,6)
            xmatrix_left[*,*,(yy+1)<(nyy-1),wl]   = REFORM([mp_result_left(0:23)],4,6)
           input_values = mp_result_left
            calc_values  = ibis_polcal_function(angle_values,input_values)
        
 ;       PRINT,'-----------------------------------------------'
 ;       PRINT,' X MATRIX FOR LEFT BEAM IS: '
 ;       PRINT,' '
 ;       PRINT,xmatrix_left[*,*,yy,wl]
 ;       PRINT,' '
 ;       PRINT,' Wave Plate Offset [degrees]:     ',mp_result_left[16]
 ;       PRINT,' Wave Plate Retardance [degrees]: ',mp_result_left[17]
 ;       PRINT,' '
 
        meas_values_left[*,yy,wl] = meas_values
        calc_values_left[*,yy,wl] = calc_values
        meas_values_left[*,(yy+1)<(nyy-1),wl] = meas_values
        calc_values_left[*,(yy+1)<(nyy-1),wl] = calc_values
  


        ;; RIGHT BEAM 

;mod nov 2014 lk for y-dependence;[pos, 6 stokes, 2 beams, wl point, y-pixel]
        s0r = (nbpolcal[*,0,1,wl,yy] + nbpolcal[*,0,1,wl,(yy+1)<(nyy-1)] ) / 2.
        s1r = (nbpolcal[*,1,1,wl,yy] + nbpolcal[*,1,1,wl,(yy+1)<(nyy-1)] ) / 2.
        s2r = (nbpolcal[*,2,1,wl,yy] + nbpolcal[*,2,1,wl,(yy+1)<(nyy-1)] ) / 2.
        s3r = (nbpolcal[*,3,1,wl,yy] + nbpolcal[*,3,1,wl,(yy+1)<(nyy-1)] ) / 2.
        s4r = (nbpolcal[*,4,1,wl,yy] + nbpolcal[*,4,1,wl,(yy+1)<(nyy-1)] ) / 2.
        s5r = (nbpolcal[*,5,1,wl,yy] + nbpolcal[*,5,1,wl,(yy+1)<(nyy-1)] ) / 2.
            
        meas_values  = DOUBLE([s0r(w_2use),s1r(w_2use),s2r(w_2use),s3r(w_2use),s4r(w_2use),s5r(w_2use)])
        if (yy eq 0) then meas_values_right  = dblarr(n_elements(meas_values),yb-ya+1,nwl)
        if (yy eq 0) then calc_values_right = dblarr(n_elements(meas_values),yb-ya+1,nwl)

;*** testing ***
;            ;try fixing WP offset
;            parinfo[24].value        = 4.05
;            parinfo[24].fixed        = 1.
;            ;try fixing WP retardance
;            parinfo[25].value        = 83.3
;            parinfo[25].fixed        = 1.
;*** end testing ***

        mp_result_right   = MPFITFUN('ibis_polcal_function', angle_values, meas_values,$
                                PARINFO=PARINFO, NITER=num_iter, MAXITER=50,XTOL=(1e-35),/QUIET,bestnorm=bestnorm)
        if (yy eq 0) then mp_res_right_tmp = fltarr(n_elements(mp_result_right),yb-ya+1,nwl)
        mp_res_right_tmp[*,yy,wl] = mp_result_right
        mp_res_right_tmp[*,(yy+1)<(nyy-1),wl] = mp_result_right
  
        chisqr[yy,wl] = bestnorm
        chisqr[(yy+1)<(nyy-1),wl] = bestnorm
          
        if (yy eq 0) then xmatrix_right = fltarr(4,6,yb-ya+1,nwl)
        xmatrix_right[*,*,yy,wl]   = REFORM([mp_result_right(0:23)],4,6)
        xmatrix_right[*,*,(yy+1)<(nyy-1),wl]   = REFORM([mp_result_right(0:23)],4,6)
        input_values = mp_result_right
        calc_values  = ibis_polcal_function(angle_values,input_values)
        
 ;       PRINT,'-----------------------------------------------'
 ;       PRINT,' X MATRIX FOR RIGHT BEAM IS: '
 ;       PRINT,' '
 ;       PRINT,xmatrix_right[*,*,yy]
 ;       PRINT,' '
 ;       PRINT,' Wave Plate Offset [degrees]:     ',mp_result_right[16]
 ;       PRINT,' Wave Plate Retardance [degrees]: ',mp_result_right[17]
 ;       PRINT,' '


     
        meas_values_right[*,yy,wl] = meas_values
        calc_values_right[*,yy,wl] = calc_values
        meas_values_right[*,(yy+1)<(nyy-1),wl] = meas_values
        calc_values_right[*,(yy+1)<(nyy-1),wl] = calc_values
  

;these elements should be ~1 for IBIS modulation scheme
        if yy mod 20 eq 0 then begin
           plot,xmatrix_left[0,0,*,wl],title='Wavelength: '+string(wl),yrange=[-1,1]
           oplot,xmatrix_left[1,0,*,wl],lines=2
           oplot,xmatrix_left[3,1,*,wl],lines=3
           oplot,xmatrix_left[1,2,*,wl],lines=4
           oplot,xmatrix_left[3,3,*,wl],lines=5
           oplot,xmatrix_left[2,4,*,wl],lines=6
           oplot,xmatrix_left[2,5,*,wl],lines=6
        endif

 
     ENDFOR                           ;end y-dependence
  
    ;save parameters, since they cannot be passed out (they are
        ;overwritten due to fewer bridges than wavelength points)
        
        fname = 'tmpsav_'+string(wl,format='(I02)')+'.sav'
        print,'Saving: ',fname
        save,mp_res_left_tmp,mp_res_right_tmp,i,meas_values_left,meas_values_right,calc_values_left,$
             calc_values_right,xmatrix_right,xmatrix_left,filename=fname,chisqr,chisql


END
