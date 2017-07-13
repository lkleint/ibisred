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
;    v3: parallelize with IDL bridges, move mpfit progs to separate file


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


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PRO ibis_polcal_xcalc_v3, lambda, polcal_datafile, path1, SAVEFILE1 = savefile1, SAVEFILE2 = savefile2, $
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
        
 
       nwl = (size(nbpolcal))[4]
 

;nsplit will be lowered by split_for to the actual number of
;processors and wavelength points (whichever is lower)

;tmpfiles = file_search('tmpsav*.sav',count=ntmp)
;if ntmp ge 2 then goto,nocalc

split_for,0,nwl-1, $
          commands = 'aux_xcalc_v3,nbpolcal,yb,ya,which_bridge,w_2use,angle_values,parinfo,i',$
          varnames=['nbpolcal','ya','yb','w_2use','angle_values'],$
          nsplit=10,struct2pass1=parinfo
;,mp_res_left_tmp,mp_res_right_tmp,i,meas_values_left,meas_values_right,calc_values_left,calc_values_right,xmatrix_right,xmatrix_left',$

;nocalc:

;output is written as save files because bridges cannot directly pass
;output (some variables are overwritten each time the same bridge runs)

;Merge output variables
tmpfiles = file_search('tmpsav*.sav',count=ntmp)

;define variables
restore,'tmpsav_00.sav'
t_xmatrix_left = xmatrix_left
t_xmatrix_right = xmatrix_right
t_mp_res_left_tmp = mp_res_left_tmp
t_mp_res_right_tmp = mp_res_right_tmp
t_meas_values_left = meas_values_left
t_meas_values_right = meas_values_right
t_calc_values_left = calc_values_left
t_calc_values_right = calc_values_right

print,'Merging output...'
FOR i=0,ntmp-1 do begin
restore,tmpfiles[i]
t_xmatrix_left[*,*,*,i] = xmatrix_left[*,*,*,i]
t_xmatrix_right[*,*,*,i] = xmatrix_right[*,*,*,i]
t_mp_res_left_tmp[*,*,i] = mp_res_left_tmp[*,*,i]
t_mp_res_right_tmp[*,*,i] = mp_res_right_tmp[*,*,i]
t_meas_values_left[*,*,i] = meas_values_left[*,*,i]
t_meas_values_right[*,*,i] = meas_values_right[*,*,i]
t_calc_values_left[*,*,i] = calc_values_left[*,*,i]
t_calc_values_right[*,*,i] = calc_values_right[*,*,i]
ENDFOR
;save as original variable names
xmatrix_left = t_xmatrix_left
xmatrix_right = t_xmatrix_right
mp_res_left_tmp = t_mp_res_left_tmp
mp_res_right_tmp = t_mp_res_right_tmp
meas_values_left = t_meas_values_left
meas_values_right = t_meas_values_right
calc_values_left = t_calc_values_left
calc_values_right = t_calc_values_right

;delete partial X matrices
FOR i=0,ntmp-1 do spawn,'rm '+tmpfiles[i]


;----- EPS plots -----------
set_plot,'ps'
device,filename='xmat_left_'+string(lambda,format='(I4)')+'.eps',/encaps,/color,xsize=20,ysize=15
loadct,0
plot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_left[1,0,*,*],/xs,/ys,yrange=[-1,1],xtitle='y pixel',title='Left X-matrix',thick=2,xthick=2,ythick=2,charthick=2
for i=2,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_left[i,0,*,*],lines=i,thick=2
tek_color
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_left[i,1,*,*],lines=i,color=2,thick=2
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_left[i,2,*,*],lines=i,color=4,thick=2
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_left[i,3,*,*],lines=i,color=9,thick=2
al_legend,['[1,0]','[2,0]','[3,0]','[0,1]','[1,1]','[2,1]','[3,1]','[0,2]','[1,2]','[2,2]','[3,2]','[0,3]','[1,3]','[2,3]','[3,3]'],$
 color=[0,0,0,2,2,2,2,4,4,4,4,9,9,9,9],lines=[0,2,3,0,1,2,3,0,1,2,3,0,1,2,3],thick=fltarr(15)+2,pspacing=1.3,position=[400,1.]
device,/close
set_plot,'x'


set_plot,'ps'
device,filename='xmat_right_'+string(lambda,format='(I4)')+'.eps',/encaps,/color,xsize=20,ysize=15
loadct,0
plot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_right[1,0,*,*],/xs,/ys,yrange=[-1,1],xtitle='y pixel',title='Right X-matrix',thick=2,xthick=2,ythick=2,charthick=2
for i=2,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_right[i,0,*,*],lines=i,thick=2
tek_color
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_right[i,1,*,*],lines=i,color=2,thick=2
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_right[i,2,*,*],lines=i,color=4,thick=2
for i=0,3 do oplot,reform(replicas(findgen(yb-ya+1)+ya,nwl),nwl*(yb-ya+1)),xmatrix_right[i,3,*,*],lines=i,color=9,thick=2
al_legend,['[1,0]','[2,0]','[3,0]','[0,1]','[1,1]','[2,1]','[3,1]','[0,2]','[1,2]','[2,2]','[3,2]','[0,3]','[1,3]','[2,3]','[3,3]'],$
 color=[0,0,0,2,2,2,2,4,4,4,4,9,9,9,9],lines=[0,2,3,0,1,2,3,0,1,2,3,0,1,2,3],thick=fltarr(15)+2,pspacing=1.3,position=[400,1.]
device,/close
set_plot,'x'
;----- EPS plots -----------


    ;add to log (use first calculated X matrix)
        log_add_xdouble,path1,xmatrix_left[*,*,0,0],mp_res_left_tmp[16,0,0],mp_res_left_tmp[17,0,0],savefile1,$
                 xmatrix_right[*,*,0,0],mp_res_right_tmp[16,0,0],mp_res_right_tmp[17,0,0],savefile2,lambda
        ;### it would be nicer to add figures of all xmatrices...
      
        IF KEYWORD_SET(SAVEFILE1) THEN BEGIN
         
            Xmat = xmatrix_left
            wp_aoffset = mp_res_left_tmp[16,*,*] 
            wp_ret = mp_res_left_tmp[17,*,*]
            PRINT,'saving: ',savefile1
            SAVE, /VERB, /COMP, FILENAME = savefile1, $
              Xmat, meas_values_left,calc_values_left, wp_aoffset,wp_ret,ya,yb,wlobs
            
        ENDIF ELSE PRINT,' NO RESULTS SAVED FOR LEFT BEAM'
        
        IF KEYWORD_SET(SAVEFILE2) THEN BEGIN
            
            Xmat = xmatrix_right
            wp_aoffset = mp_res_right_tmp[16,*,*]
            wp_ret = mp_res_right_tmp[17,*,*]
    
            PRINT,'saving: ',savefile2
            SAVE, /VERB, /COMP, FILENAME = savefile2, $
              Xmat, meas_values_right,calc_values_right, wp_aoffset,wp_ret,ya,yb,wlobs
            
        ENDIF ELSE PRINT,' NO RESULTS SAVED FOR RIGHT BEAM'

     ENDELSE 

END
