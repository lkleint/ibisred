;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;
;; Main procedure to calibrate the IBIS observations for darks
;; gain, blueshift and image distortions (destretch). The procedure
;; uses the output of ibis_combine.pro as input data.
;;
;; oct 11: modified for flagncam
;; ### todo: check alignment because params refer to 500x500 images
;; single and spectroscopy speckle not done yet
;;
;; BRIDGE parallelism added by Tom Schad on 2 April 2012
;; jan 14: modified for new reduction
;; june 2015: added wl_obs to save variables
;; april 2017: adapted for single beam. Went line-by-line with
;; no_bridges and files now match. spectroscopy destretch not
;; parallelized yet. single beam pol. not done yet.
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PRO IBIS_CALIBRATE_BRIDGES, o_dir, g_file, d_file, s_file, wl_file, a_file, bshift, $ 
                    pupil, snumber, $ 
                    POL = pol, TPFILE = tpfile, DSTR_IDL = dstr_idl, $
                    SPECKLE = speckle, SINGLE = single, DUAL = dual, $
                    limb=limb, flagncam=flagncam

;is called once per wl and directory

;***************************************************************
; START UP IDL BRIDGES TO USE FOR PARALLEL PROCESSING 
;***************************************************************

ncpus    = !cpu.hw_ncpu
nthreads = !cpu.tpool_nthreads
IF N_ELEMENTS(nthreads) EQ 0 THEN nthreads = 1

PRINT,' '
PRINT,' IBIS_CALIBRATE_BRIDGES ::  found ',STRTRIM(ncpus,2),' processors'
PRINT,' '
PRINT,' using ',STRTRIM(ncpus,2),' bridge sessions'
PRINT,' program hard coded to use all processors found in !cpu.hw_ncpu variable'
PRINT,' '

bridges = OBJARR(ncpus)
cd, current = pwd
FOR cpu = 0,ncpus-1 DO BEGIN     
    output_file = pwd + '/output_bridge_' + STRTRIM(cpu,2) + '.txt'
    (bridges[cpu]) = OBJ_NEW('IDL_IDLBridge',OUTPUT = output_file)
    (bridges[cpu])-> EXECUTE, "cd, '" + pwd + "'"
    (bridges[cpu])-> EXECUTE, "cpu, tpool_nthreads=" + string(nthreads)
ENDFOR

ncpus = N_ELEMENTS(bridges)

;***************************************************************
; DESTRETCH AND SPECKLE PARAMETERS
;***************************************************************

IF KEYWORD_SET(DSTR_IDL) THEN BEGIN 

     DS_KERNEL = (dstr_idl.kernel)
     DT_DETR   = (dstr_idl.detrend)
     METHOD    = (dstr_idl.method)
     REGION    = (dstr_idl.region)
     AVG_IMG   = (dstr_idl.avg_img)

ENDIF

IF KEYWORD_SET(SPECKLE) THEN BEGIN 
     speckle_path = (speckle.datapath)
     speckle_nbdb = (speckle.database)
ENDIF

;***************************************************************
; READ DATA
;***************************************************************




RESTORE, g_file
RESTORE, d_file
RESTORE, s_file
RESTORE, wl_file
RESTORE, a_file

IF FILE_TEST(speckle_nbdb) THEN RESTORE, /VERB, speckle_nbdb  else print,'No speckle file found'

dim   = SIZE(nb_data)
Ncol  = dim[1]
Nrow  = dim[2]

 npol = diffelement(info_nb.stokes) 
;  IF KEYWORD_SET(TPFILE) THEN restore, /verb, tpfile
  Nwave = dim[3]/Npol
  modulation = (info_nb.stokes)[0:Npol-1]

;old data has different structure for info_flat_short
if (size(info_flat_short))[0] eq 2 then info_flat_short = reform(info_flat_short[*,0])

PRINT,'---------------------------------------------------'
PRINT,'Ok, here the dimensions:                           '       
PRINT,'Spatial dimensions: ', Ncol, Nrow                   
PRINT,'Wavelength points: ', Nwave
PRINT,'Number of modulation states : ', Npol
PRINT,'Modulation scheme : ', modulation
PRINT,'Does that seem right ?'
PRINT,'---------------------------------------------------'

;***************************************************************
; Determine mask indexes
;***************************************************************

mask = fix(pupil)
index_in = WHERE(mask EQ 1)
index_out = WHERE(mask EQ 0)

;***************************************************************
; Get index for ordering wavelengths
;***************************************************************


wtmp = REFORM(info_nb.wave)
wtmp     = wtmp[uniq(wtmp)]
index_wl = SORT(wtmp)
;index_ff = SORT(index_wl) ;april 2017: commented out to match no-bridges
wscale   = wtmp[index_wl]
;data should already be sorted by wl, at least for new cam

; deal with R>1 cases (repetitions of same WL)
  rr = n_elements(where(info_nb.wave eq info_nb.wave[0] and info_nb.stokes eq info_nb.stokes[0]))
  if rr ne 1 then begin
     print,'Repetitions of same wl step exist. R='+string(rr,format='(I3)')+' ! '
     nrep = rr
  endif


;#################################################################
;#################################################################
; 
; POLARIMETRY CASE
;
;#################################################################
;#################################################################

IF KEYWORD_SET(POL) THEN BEGIN

    IF KEYWORD_SET(DUAL) THEN BEGIN
        
        PRINT,'---------------------------------------------------'
        PRINT,' Ok, We are in Polarimetric dual-beam Mode          '
        PRINT,'---------------------------------------------------'
        
        ;#################################################################
        ; Define masks
        ;#################################################################

        ; Get one rectangular mask for both beams

 ;lk: test if this does anything. probably not.
    ;    t1 = GET_RECT_FROM_APS_POL(pupil[0:Ncol/2-1, 0:Nrow-1])
    ;    t2 = GET_RECT_FROM_APS_POL(pupil[Ncol/2:Ncol-1, 0:Nrow-1])
    ;    x1 = MAX([t1[0,0],t2[0,0]]) & dx = MIN([t1[1,0],t2[1,0]]) - x1
    ;    y1 = MAX([t1[0,1],t2[0,1]]) & dy = MIN([t1[1,1],t2[1,1]]) - y1

        ;#################################################################
        ; Get speckle reconstruction if existent
        ;#################################################################

        IF file_test(speckle_nbdb) THEN BEGIN

            ; Find corresponding speckle reconstruction using database

            PRINT,'------------------------------------------------------'
            PRINT,' Identify the speckle reconstruction for the scan      '
            PRINT,'------------------------------------------------------'

        ;mod Nov 18, 14
           index = where(strpos(nb_speckle_db.filename,strmid(s_file,29,30,/rev)) ne -1)
           if index[0] eq -1 then message,'No correct speckle file found'
           burst = speckle_path + strmid(nb_speckle_db.burst[index],31,33,/rev)
 


            IF (N_ELEMENTS(burst) EQ 1 and burst[0] ne -1) THEN BEGIN 

                PRINT,'------------------------------------------------------'
                PRINT,' One reconstruction found : ', burst
                PRINT,'------------------------------------------------------'
       
                RESTORE, /verb, burst           
                tmp=simage
       
                ;new 2014: just rotate speckle image
                simage = newwl_v2(tmp,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl)
              
                ; Shift speckle reconstruction on average whitelight or
                ; to correct for atmospheric refraction. This is
                ; corrected for in the whitelight already but not in
                ; the reconstruction!

;no longer to this (2014): align everything to speckle image
;                delta = SHC((AVG(mwld,2))[Ncol/4-64:Ncol/4+64,Nrow/2-64:Nrow/2+64], $ 
;                            simage[Ncol/4-64:Ncol/4+64,Nrow/2-64:Nrow/2+64], interpolate=1)
;                simage = (SHIFT_BICUB(simage,delta[0],delta[1]))[0:Ncol/2-1,0:Nrow-1]

            ENDIF               ;--BURST
            
            IF N_ELEMENTS(burst) GT 1 THEN BEGIN ;### should not happen for new cam
               print,'this should not happen, check ibis_calibrate_bridges'
               stop
              PRINT,'------------------------------------------------------'
              PRINT,' More than one reconstruction found : ', burst
              PRINT,'------------------------------------------------------'

            ;*lk: deleted all lines 
              
              
          ENDIF                 ;--BURST
          
      ENDIF                     ;--SPECKLE

      ;;#################################################################
      ;; Get telescope geometry and lightlevel if existent
      ;;#################################################################
      ;telparams is from a file saved by ibis_wl_db_new.pro (which works
      ;with new cam). But it is easier to take telparams from info_nb for
      ;new cam

        tp_azim  = info_nb.dst_az
        tp_elev  = info_nb.dst_el
        tp_table = info_nb.dst_tbl
        tp_pee = info_nb.dst_pee
        tp_bee =  info_nb.dst_bee
        tp_see =  info_nb.dst_see
        tp_slng =  info_nb.dst_slng
        tp_slat = info_nb.dst_slat
        tp_llevel = info_nb.dst_llevel
        lightlevel = tp_llevel /  AVG(tp_llevel)
     
           
      ;;#################################################################
      ;; Flatfield ((and lightlevel)) correction for nb and wl
      ;;#################################################################

        ;; Apply gain table and lightlevel for each wavelength separately.
        ;; omit light level correction for now. ll is updated each 10s
        ;; i.e. to little for a good correction. A jump/update in ll between
        ;; i+q and i-q would lead to wrong results

       print,'--------------------------------------------------------------------'
        ;;print,'Gain table correction for narrowband                '
           print,'Light level for whitelight channel                                  '
        print,'--------------------------------------------------------------------'

 ;       l = 0
        mwldn   = 0. * mwld
 ;       nbdwc   = FLTARR(Ncol, Nrow, dim[3])

;;what happens if flat is I+Q,I-Q,.. etc. and nb_data only I?
;;npol will be ok, since taken from nb_info,i.e. wcgain[*,*,*,0:npol-1]
;;would be used.
;;this probably doesn't matter for I observations but if npol=2 (I+-V)
;;it could be a problem. therefore: test if flat has same modulation as nb:
; npolflat = diffelement(info_flat_short.stokes)
; if npol ne npolflat then begin ;ff more pol states than obs
;  print,'*** WARNING: flat has more polarization states than observations! ***'
;  polflat = info_flat_short.stokes[0:npolflat-1]
;  flatind = fltarr(npol)
; ;if I is observed and flats are done for I+-S, then special care has to be taken
;  if modulation eq 'I' then begin
;   ;take polarization state where lcvr were set to same voltage
;    tmp=where(info_flat_short.lcvr1 eq info_nb.lcvr1[0] and info_flat_short.lcvr2 eq info_nb.lcvr2[0])
;    modulation = (info_flat_short.stokes[tmp])[0] ;this 'overwrites' I with usually I+Q
;  endif
;  for aa=0,npol-1 do flatind[aa] = where(modulation[aa] eq polflat)
; endif else begin
; flatind = findgen(npol) ;in the regular case flat modulation have same indices as obs
; endelse

    ;    FOR n=0, Nwave-1 DO BEGIN  
    ;        FOR i=0, Npol-1 DO BEGIN

        FOR i=0,(size(mwld))[3]-1 do mwldn[*,*,i] = mwld[*,*,i] / lightlevel[i]

 ;now done in ibis_combine
 ;              nbdwc[*, *, l] = ((nb_data[*, *, l] - dark) * wcgain[*, *, index_ff[n], flatind[i]]) ;/ lightlevel[l]
 ;              l += 1
 ;           ENDFOR
 ;       ENDFOR

        mwld = mwldn  ;changed 20110207 LK
        mwldn = 0
        wcgain = 0

        
        ;;#################################################################
        ;; Alignment between left and right channel
        ;;#################################################################
        ;;#################################################################
        ;; Determine alignment between left and right beam
        ;; after gain correction (/= Ali's new version)
        ;;#################################################################
         ;LK: Jan 2014: already done with dot grid, should not change during
        ;the day

;        print,'-----------------------------------------'
;        print,'Alignment between left and right channel '
;        print,'-----------------------------------------'
;        s = fltarr(2)
  
;        FOR i=0,dim[3]-1 DO BEGIN
;            s += shc(nbdwc[0:Ncol/2-1, *, i], nbdwc[Ncol/2:Ncol-1, *, i], /interp)
;        ENDFOR
;        s /= dim[3] ;average shift
 

        ;;
        ;; shift right subaperture on left (also apply to mask and offset)
        ;;

        ;this part stays in the new version
       ; aps_mod = pupil
       ; tmp = shift_bicub(float(pupil[Ncol/2:Ncol-1, *]), off[0], off[1])
       ; aps_mod[Ncol/2:Ncol-1, *] = FIX(ROUND(tmp))

        aps_mod = newwl_v2(pupil,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)

;        bshift_mod = bshift
;        tmp = shift_bicub(bshift[Ncol/2:Ncol-1, *], s[0], s[1])
;        bshift_mod[Ncol/2:Ncol-1, *] = tmp
        bshift_mod = newwl_v2(bshift,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)


;        FOR i=0, dim[3]-1 DO BEGIN
;            tmp = shift_bicub(nbdwc[Ncol/2:Ncol-1, *, i], s[0], s[1])
;            nbdwc[Ncol/2:Ncol-1, *, i] = tmp
;        ENDFOR




;Tom's method:
  ;      PRINT,'-----------------------------------------'
  ;      PRINT,'Alignment between left and right channel '
   ;     PRINT,'-----------------------------------------'
  
         ; FIND BULK SHIFTS BETWEEN MASKED PUPILS
	  
   ;     mask_li = pupil[0:Ncol/2-1, *]<1
   ;     mask_ri  = pupil[Ncol/2:Ncol-1, *]<1
   ;     s0 = shc(mask_li,mask_ri,/inter)
   ;     mask_ri_s0 = SHIFT_BICUB(mask_ri,s0[0],s0[1])

   ;     mask_tot = (mask_li + mask_ri_s0)
   ;     wovlp = ARRAY_INDICES(mask_tot,WHERE(mask_tot ge 2)) ;=overlap
       
   ;     xmmin = (MIN(wovlp[0,*])+0.15*(Ncol/2.))
   ;     xmmax = (MAX(wovlp[0,*])-0.15*(Ncol/2.))
   ;     ymmin = (MIN(wovlp[1,*])+0.15*(Nrow))
   ;     ymmax = (MAX(wovlp[1,*])-0.15*(Nrow))  

   ;     pp = FLTARR(2,2,4)
   ;     pp[0,0,0] = 10
   ;     pp[1,0,0] = 10
   ;     pp[0,0,1] = (xmmax-xmmin)-2.
   ;     pp[1,0,1] = (ymmax-ymmax)-2
   ;     pp[0,0,2] = 10
   ;     pp[1,0,2] = (ymmax-ymmax)-2
    ;    pp[0,0,3] = (xmmax-xmmin)-2.
   ;     pp[1,0,3] = 10
   ;     pp[*,1,*] = pp[*,0,*] 

   ;     tt = CALTRANS(pp)
   ;     pin = tt[*,0]
   ;     qin = tt[*,1]
        
   ;     print,' aligning one wavelength with auto_align'
   ;     i = FIX(dim[3]/2.)   
        
   ;     nbd_li = TOTAL(nbdwc[0:Ncol/2-1, *, *],3)
   ;     nbd_ri = TOTAL(nbdwc[Ncol/2:Ncol-1, *, *],3)
   ;     nbd_ri_s0 = SHIFT_BICUB(nbd_ri,s0[0],s0[1])	
        
   ;     timage = nbd_ri_s0[xmmin:xmmax,ymmin:ymmax]
   ;     rimage = nbd_li[xmmin:xmmax,ymmin:ymmax]
        
   ;     inew=auto_align_images_lk(timage,rimage,pin,qin,pint1,qint1,/AMOEBA,ITMAX=300,/NOPLOT)
   ;     inew=auto_align_images_lk(timage,rimage,pint1,qint1,pint2,qint2,/POWELL,/NOPLOT)
        
   ;     pq2rss,pint2,qint2,erot,exscl,eyscl,exshft,eyshft,enrss,(xmmax-xmmin+1),(ymmax-ymmin+1),/CENTER
   ;     s1 = s0 &     s1[0] +=  exshft &   s1[1] +=  eyshft
   ;     s = s1
   ;     dxy = FLTARR(2,dim[3])

	;FOR i =0,dim[3]-1 DO BEGIN 

            
    ;        nbd_li = nbdwc[0:Ncol/2-1, *, i]
    ;        nbd_ri = nbdwc[Ncol/2:Ncol-1, *, i]
    ;        nbd_ri_s0 = SHIFT_BICUB(nbd_ri,s0[0],s0[1])	
            
    ;        nbdwc[Ncol/2:Ncol-1, *, i] =  POLY_2D(nbd_ri_s0,pint2,qint2,2,Ncol/2.,Nrow,CUBIC=-0.5,MISSING = MISSING)
            
    ;        dxy[*,i] = s1
            
    ;    ENDFOR
     
    ;    s = TOTAL(dxy,2)/dim[3]
        
    ;    aps_mod = pupil
    ;    tmp = SHIFT_BICUB(float(pupil[Ncol/2:Ncol-1, *]), s[0], s[1])
    ;    aps_mod[Ncol/2:Ncol-1, *] = FIX(ROUND(tmp))
        
    ;    bshift_mod = bshift
    ;    tmp = SHIFT_BICUB(bshift[Ncol/2:Ncol-1, *], s[0], s[1])
;	bshift_mod[Ncol/2:Ncol-1, *] = tmp

       
        ;;#################################################################
        ;; Resample the arrays
        ;;#################################################################

;        nbsl = nbdwc[0:Ncol/2-1, 0:Nrow-1, *]  ;left nb, corrected
;        nbsr = nbdwc[Ncol/2:Ncol-1, 0:Nrow-1, *] ;right nb corrected

        nbsl = nb_data[*,*,*,0]
        nbsr = nb_data[*,*,*,1]


 ;       bsds = rebin(bshift_mod, Ncol, Nrow, Npol*Nwave)
 ;       blsl = bsds[0:Ncol/2-1, 0:Nrow-1, *] ;left bs map
 ;       blsr = bsds[Ncol/2:Ncol-1, 0:Nrow-1, *] ;right bs map

        bsds = rebin(bshift_mod, Ncol, Nrow, 2, Npol*Nwave)  ;same image for whole array
        blsl = reform(bsds[*,*,0,*])
        blsr = reform(bsds[*,*,1,*])
        bsds=0

        print,'-----------------------------------------'
        print,'Offset mask size: ', size(bshift)
        print,'-----------------------------------------'

        s = size(mwld)

        ;;#################################################################
        ;; Do the destretch with IDL code
        ;;#################################################################

        PRINT,'-----------------------------------------'
        PRINT,'Destretch starts now .....               '
        PRINT,'-----------------------------------------'

        IF KEYWORD_SET(DSTR_IDL) THEN BEGIN

           PRINT,'-----------------------------------------'
           PRINT,'Destretch mode: IDL-code                 '
           PRINT,'Method : ', method
           PRINT,'Kernel : ', DS_KERNEL
           PRINT,'-----------------------------------------'
 
 ;apr12: this part must be before 'avg', 'seq' etc to have all limits defined 
             mask_mod = newwl_v2(mask,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
 ;mod 150331 to avoid making mask too big if newwl_v2 shows
;interpolation issues (i.e. FOV too far right on the camera)
             ind = where(mask_mod[*,*,0] ge .9,complement=compind)
             tmpmask = mask_mod[*,*,0]
             tmpmask[compind] = 0.
             mask_mod[*,*,0] = tmpmask

             tmp = ibis_mask(mask_mod[*,*,0],cut=2)
             tmp = where2d(tmp EQ 1)
             xl = min(tmp[0,*])
             xr = max(tmp[0,*])
             yb = min(tmp[1,*])
             yt = max(tmp[1,*])
             region[0,*] = region[0,*] - xl
             region[1,*] = region[1,*] - yb


           PRINT,'running DESTRETCH_IBIS.pro'
           If method EQ 'AVG' THEN BEGIN
             ref = AVG(mwld,2)
             mwld2=mwld
             mwld2[xl:xr,yb:yt,*] = DESTRETCH_IBIS(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR, $
                         shifts=shft_tot, grid=grid, idlbridge = bridges)
           ENDIF

           IF method EQ 'SEQ' THEN BEGIN
              mwld2=mwld
              mwld2[xl:xr,yb:yt,*] = DESTRETCH_IBIS(AVG_IMG[0], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR, $
                shifts=shft_tot, grid=grid, idlbridge = bridges)
           ENDIF

           IF method EQ 'SPECKLE' THEN BEGIN
             ref = simage
             mwld2 = mwld
             mwld2[xl:xr,yb:yt,*] = DESTRETCH_IBIS(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], $
                                                   DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid, idlbridge = bridges)
        
	     ENDIF

           ;;#################################################################
           ;; Destretch narrowband and blueshift
           ;;#################################################################

           PRINT,'-------------------------------------------------------'
           PRINT,'Destretch of whitelight done .....                     '
           PRINT,'Now we destretch the narrowband and the blueshift map  '
           PRINT,'-------------------------------------------------------'


          IF N_ELEMENTS(DS_KERNEL) EQ 1 THEN BEGIN ;new version only
              
           PRINT,'Align shift :', shft_tot[*, 0, 0, 0]

            nbl = 0. * nb_data

           ;linear shift for left and right beam
           FOR i = 0,dim[3]-1 DO BEGIN
               nb[*, *, i,0] = shift_bicub(nb_data[*, *, i,0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
               nb[*, *, i,1] = shift_bicub(nb_data[*, *, i,1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
           ENDFOR

           ;linear shift for blueshift map
           blds = rebin(bshift_mod, Ncol, Nrow, 2, Npol*Nwave)
 ;mod LK: variable never used afterwards 
     ;     bshift_dstr = 0. * blds
      ;     FOR i = 0,dim[3]-1 DO BEGIN
      ;         bshift_dstr[*, *, i, 0] = shift_bicub(blds[*, *, i, 0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
      ;         bshift_dstr[*, *, i, 1] = shift_bicub(blds[*, *, i, 1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
      ;     ENDFOR
           FOR i = 0,dim[3]-1 DO BEGIN
              blsl[*, *, i, 0] = shift_bicub(blds[*, *, i, 0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              blsr[*, *, i, 1] = shift_bicub(blds[*, *, i, 1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
           ENDFOR



        ENDIF ELSE BEGIN

            ;;---------------------------
            ;; destretch narrowband left 
            ;;---------------------------

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->SetVar, "i",-1   
                (bridges[cpu])->SetVar,"grid",grid
                (bridges[cpu])->Execute, "dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))"
                (bridges[cpu])->Execute, "forward_function doreg, mkcps, repair"
            ENDFOR

            FOR i = 0,dim[3]-1 DO BEGIN
                bridge =  GET_IDLE_BRIDGE(bridges)
                bridge->Execute, "PRINT,'I IS:',i"
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN nbsl[xl:xr, yb:yt,iold] = bridge->GetVar('result')
                bridge->SetVar,  "i",i
                bridge->SetVar,"nbsl_i",REFORM(nbsl[xl:xr, yb:yt,i])
                bridge->SetVar,"shft_tot_i",REFORM(shft_tot[*,*,*,i])
                bridge->Execute, "result = DOREG(nbsl_i,grid,shft_tot_i)",/NOWAIT
            ENDFOR     
            
            BARRIER_BRIDGES,bridges
            
            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->Execute, "HELP"                 
                (bridges[cpu])->Execute, "PRINT,'CLEAN UP; I IS:',i"
                iold = (bridges[cpu])->GetVar('i')
                IF (iold ne -1) THEN nbsl[xl:xr, yb:yt,iold] = (bridges[cpu])->GetVar('result')
            ENDFOR

            ;;---------------------------
            ;; destretch narrowband right
            ;;----------------------------
            
            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->SetVar, "i",-1   
             ENDFOR
            
            FOR i = 0,dim[3]-1 DO BEGIN
                bridge =  GET_IDLE_BRIDGE(bridges)
                bridge->Execute, "PRINT,'I IS:',i"
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN nbsr[xl:xr, yb:yt,iold] = bridge->GetVar('result')
                bridge->SetVar,  "i",i
                bridge->SetVar,"nbsr_i",REFORM(nbsr[xl:xr, yb:yt,i])
                bridge->SetVar,"shft_tot_i",REFORM(shft_tot[*,*,*,i])
                bridge->Execute, "result = doreg(nbsr_i,grid,shft_tot_i)",/NOWAIT
            ENDFOR
            
            BARRIER_BRIDGES,bridges

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->Execute, "HELP"                 
                (bridges[cpu])->Execute, "PRINT,'CLEAN UP; I IS:',i"
                iold = (bridges[cpu])->GetVar('i')
                IF (iold ne -1) THEN nbsr[xl:xr, yb:yt,iold] = (bridges[cpu])->GetVar('result')
            ENDFOR
            ;;----------------------------
            ;; left blueshift
            ;;----------------------------

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->SetVar, "i",-1   
             ENDFOR
            
            FOR i = 0,dim[3]-1 DO BEGIN
                bridge =  GET_IDLE_BRIDGE(bridges)
                bridge->Execute, "PRINT,'I IS:',i"
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN blsl[xl:xr, yb:yt,iold] = bridge->GetVar('result')
                bridge->SetVar,  "i",i
                bridge->SetVar,"blsl_i",REFORM(blsl[xl:xr, yb:yt,i])
                bridge->SetVar,"shft_tot_i",REFORM(shft_tot[*,*,*,i])
                bridge->Execute, "result = doreg(blsl_i,grid,shft_tot_i)",/NOWAIT
            ENDFOR

            BARRIER_BRIDGES,bridges

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->Execute, "HELP"                 
                (bridges[cpu])->Execute, "PRINT,'CLEAN UP; I IS:',i"
                iold = (bridges[cpu])->GetVar('i')
                IF (iold ne -1) THEN blsl[xl:xr, yb:yt,iold] = (bridges[cpu])->GetVar('result')
            ENDFOR

            ;;--------------------------
            ;; right blueshift
            ;;--------------------------

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->SetVar, "i",-1   
            ENDFOR
            
            FOR i = 0,dim[3]-1 DO BEGIN
                bridge =  GET_IDLE_BRIDGE(bridges)
                bridge->Execute, "PRINT,'I IS:',i"
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN blsl[xl:xr, yb:yt,iold] = bridge->GetVar('result')
                bridge->SetVar,  "i",i
                bridge->SetVar,"blsr_i",REFORM(blsr[xl:xr, yb:yt,i])
                bridge->SetVar,"shft_tot_i",REFORM(shft_tot[*,*,*,i])
                bridge->Execute, "result = doreg(blsr_i,grid,shft_tot_i)",/NOWAIT
            ENDFOR
            
            BARRIER_BRIDGES,bridges

            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges[cpu])->Execute, "HELP"                 
                (bridges[cpu])->Execute, "PRINT,'CLEAN UP; I IS:',i"
                iold = (bridges[cpu])->GetVar('i')
                IF (iold ne -1) THEN blsr[xl:xr, yb:yt,iold] = (bridges[cpu])->GetVar('result')
            ENDFOR
	
         ENDELSE ;ds_kernel
        ENDIF ;--DSTR_IDL

        ;;#################################################################
        ;; Perform blueshift correction
        ;;#################################################################
    ;note: needed to change ncol/2 to ncol
        print,'-------------------------------------------------------'
        print,'Now we correct the narrowband for the blueshift        '
        print,'-------------------------------------------------------'

if rr eq 1 then begin
        nbsl = REFORM(nbsl, Ncol, Nrow, Npol, Nwave)
        nbsr = REFORM(nbsr, Ncol, Nrow, Npol, Nwave)
        blsl = REFORM(blsl, Ncol, Nrow, Npol, Nwave)
        blsr = REFORM(blsr, Ncol, Nrow, Npol, Nwave)
        nbdwc = FLTARR(Ncol, Nrow, Npol, Nwave, 2)
        FOR j = 0, Nrow-1 DO BEGIN
            FOR i = 0, Ncol-1 DO BEGIN
                FOR k = 0, Npol-1 DO BEGIN
                   nbdwc[i, j, k, *, 0] = REFORM(INTERPOL(REFORM(nbsl[i, j, k, index_wl]), wscale, $
                                                       wscale + REFORM(blsl[i, j, k, index_wl])))
                   nbdwc[i, j, k, *, 1] = REFORM(INTERPOL(REFORM(nbsr[i, j, k, index_wl]), wscale, $
                                                       wscale + REFORM(blsr[i, j, k, index_wl])))
                ENDFOR
            ENDFOR
        ENDFOR
nrep = 1
endif else begin
        nbsl = REFORM(nbsl, Ncol, Nrow, Npol, Nrep, Nwave/Nrep)
        nbsl = transpose(nbsl,[0,1,2,4,3])
        nbsr = REFORM(nbsr, Ncol, Nrow, Npol, Nrep, Nwave/Nrep)
        nbsr = transpose(nbsr,[0,1,2,4,3])
        blsl = REFORM(blsl, Ncol, Nrow, Npol, Nrep, Nwave/Nrep)
        blsl = transpose(blsl,[0,1,2,4,3]) 
        blsr = REFORM(blsr, Ncol, Nrow, Npol, Nrep, Nwave/Nrep)
        blsr = transpose(blsr,[0,1,2,4,3]) 
        nbdwc = FLTARR(Ncol, Nrow, Npol, Nwave/Nrep, 2, Nrep)
        FOR j = 0, Nrow-1 DO BEGIN
           FOR i = 0, Ncol-1 DO BEGIN
              FOR k = 0, Npol-1 DO BEGIN
                 FOR l=0, nrep-1 DO BEGIN
                    nbdwc[i, j, k, *, 0, l] = REFORM(INTERPOL(REFORM(nbsl[i, j, k, index_wl, l]), wscale, $
                                                     wscale + REFORM(blsl[i, j, k, index_wl, l])))
                    nbdwc[i, j, k, *, 1, l] = REFORM(INTERPOL(REFORM(nbsr[i, j, k, index_wl, l]), wscale, $
                                                     wscale + REFORM(blsr[i, j, k, index_wl, l])))
                 ENDFOR
              ENDFOR
           ENDFOR
        ENDFOR
nwave = nwave/nrep
endelse


         ;destretched wl is stored in mwld
        mwld = mwld2 & mwld2 = 0

goto,skip_fast_blueshift
    
;Tom's testing
;        nbsl = REFORM(nbsl, Ncol/2, Nrow, Npol, Nwave)
;        nbsr = REFORM(nbsr, Ncol/2, Nrow, Npol, Nwave)

;        blsl = REFORM(blsl, Ncol/2, Nrow, Npol, Nwave)
;        blsr = REFORM(blsr, Ncol/2, Nrow, Npol, Nwave)
        
;        PRINT,' FORMING ARRAY TO HOLD NARROWBAND DATA'
;        nbdwc = FLTARR(Ncol/2, Nrow, Npol, Nwave, 2)

;        PRINT,' NOW INTERPOLATING FOR THE BLUESHIFT'

;        systime0 = SYSTIME(/SECONDS)

;        xout = wscale
;        FOR j = 0, Nrow-1 DO $
;          FOR i = 0, Ncol/2-1 DO $
;          FOR k = 0, Npol-1 DO BEGIN
;            IF aps_mod[i,j] NE 0 THEN BEGIN 
;                ; left beam -------------------
;                v0 = REFORM(nbsl[i, j, k, index_wl])
;                xin = wscale + REFORM(blsl[i, j, k, index_wl])
;                s = VALUE_LOCATE(xout,xin) > 0L < (Nwave-2) 
;                diff = v0[s+1] - v0[s]
;                nbdwc[i, j, k, *, 0] = REFORM((xout-xin[s])*diff/(xin[s+1] - xin[s]) + v0[s])
;                ; right beam --------------------
;                v0 = REFORM(nbsr[i, j, k, index_wl])
;                xin = wscale + REFORM(blsr[i, j, k, index_wl])
;                s = VALUE_LOCATE(xout,xin) > 0L < (Nwave-2)
;                diff = v0[s+1] - v0[s]
;                nbdwc[i, j, k, *, 1] = REFORM((xout-xin[s])*diff/(xin[s+1] - xin[s]) + v0[s])
;            ENDIF
;        ENDFOR
;        PRINT,' BLUESHIFT INTERPOLATE TIME: ',systime(/seconds)-systime0
skip_fast_blueshift:

;lk: useless?
;        bshift_dstr = fltarr(Ncol/2, Nrow, Npol, Nwave, 2)
;help,bshift_dstr
;help,blsl
;help,blsr
;        bshift_dstr[0,0,0,0,0] = blsl
;        bshift_dstr[0,0,0,0,1] = blsr

;        mwld = mwld2 & mwld2 = 0

        ;;#################################################################
        ;; Mask outer field and fill with average value of inside
        ;;#################################################################

        mask = (aps_mod[*,*,0] gt 0.)
        index_out_new = WHERE(mask EQ 0)
        index_in_new = WHERE(mask EQ 1)
;april 2017: added R>1
        FOR k = 0, Npol-1 DO BEGIN
            FOR i = 0, Nwave-1 DO BEGIN
               for l = 0,nrep-1 do begin
               tmpl = REFORM(nbdwc[*,*,k,i,0, l])
               tmpl[index_out_new] = AVG(tmpl[index_in_new])
               nbdwc[*,*,k,i,0, l] = tmpl
               tmpr = REFORM(nbdwc[*,*,k,i,1, l])
               tmpr[index_out_new] = AVG(tmpr[index_in_new])
               nbdwc[*,*,k,i,1, l] = tmpr
               endfor
           ENDFOR
        ENDFOR

                                ;save only data, not outer parts of image -> files are 500 MB
                                ;use the same pixel cutoff for all
                                ;data on a given day (all lambda same cutoff)
      cpath = strmid(g_file,0,strlen(g_file)-13)

      if file_test(cpath+'cutoffpx.txt') then begin
         openr,1,cpath+'cutoffpx.txt'
         readf,1,lpos,rpos,bpos,tpos
         close,1
      endif else begin
           ;get cutoffs for image
           tmp = max(deriv(aps_mod[0:dim[1]/2,dim[2]/2,0]),lpos) ;left edge of mask
           tmp = max(deriv(aps_mod[dim[1]/2:*,dim[2]/2,0]),rpos) ;r edge of mask
           rpos = rpos + dim[1]/2
           tmp = max(deriv(aps_mod[dim[1]/2,0:dim[2]/2,0]),bpos) ;bottom edge of mask
           tmp = max(deriv(aps_mod[dim[1]/2,dim[2]/2:*,0]),tpos) ;top edge of mask
           tpos = max(where(max(deriv(aps_mod[dim[1]/2,dim[2]/2:*,0])) eq deriv(aps_mod[dim[1]/2,dim[2]/2:*,0]))) ;top edge of mask
           tpos = tpos + dim[2]/2

          ;write file
           openw,1,cpath+'cutoffpx.txt'
           printf,1,lpos,rpos,bpos,tpos
           close,1   
        endelse 


        nbdwc = nbdwc[lpos:rpos,bpos:tpos,*,*,*,*]
        mwld = mwld[lpos:rpos,bpos:tpos,*]
        simage = simage[lpos:rpos,bpos:tpos]
        bshift_mod = bshift_mod[lpos:rpos,bpos:tpos,*]

       ;mod 140414 LK, simage may not exist...
        if file_test(speckle_nbdb) then simage = simage[lpos:rpos,bpos:tpos]

        ;;#################################################################
        ;; Show data
        ;;#################################################################

    ;    window, 0, xs = 3*Ncol, ys = Nrow
    ;    tvscl, avg(reform(nbdwc[*,*,0,*,0]),2),0,0
    ;    tvscl, avg(reform(nbdwc[*,*,0,*,1]),2),Ncol,0
    ;    tvscl, avg(mwld,2),2*Ncol,0

        IF (Npol EQ 1) THEN nbdwc = REFORM(nbdwc[*,*,0,*,*,*])

     ENDIF ;--DUAL


;******************************** SINGLE BEAM ********************************************

     IF KEYWORD_SET(SINGLE) THEN BEGIN

         PRINT,'---------------------------------------------------'
         PRINT,'Ok, We are in Polarimetric single-beam Mode                    '
         PRINT,'---------------------------------------------------'

        ;;#################################################################
        ;; Get speckle reconstruction if existent
        ;;#################################################################

;### todo for new cam
      IF file_test(speckle_nbdb) THEN BEGIN

           print,'------------------------------------------------------'
           print,'Identify the speckle reconstruction for the scan      '
           print,'------------------------------------------------------'

           index = where(s_file eq nb_speckle_db.filename)
           if index[0] eq -1 then message,'No correct speckle file found'

           burst = nb_speckle_db.burst[index] ;### one reconstr. per file, burst is filename in this case

 

           IF (N_ELEMENTS(burst) EQ 1 and burst[0] ne -1) THEN BEGIN 

              print,'------------------------------------------------------'
              print,' One reconstruction found : ', burst
              print,'------------------------------------------------------'

              RESTORE, burst
              tmp=simage

               ;new 2014: just rotate speckle image
              message,'ibis align is not adapted for single beam pol. yet'
              simage = newwl_v2(tmp,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl) ;this will be wrong!

           ENDIF ;--BURST

         IF N_ELEMENTS(burst) GT 1 THEN BEGIN ;### should not happen for new cam
             print,'this should not happen, check ibis_calibrate_no_bridges'
             stop
             print,'------------------------------------------------------'
             print,' More than one reconstruction found : ', burst
             print,'------------------------------------------------------'
            ;*** lk: deleted all lines
           ENDIF ;--BURST


        ENDIF ;--SPECKLE

        ;;#################################################################
        ;; Get telescope geometry and lightlevel if existent
        ;;#################################################################

      tp_azim  = info_nb.dst_az
      tp_elev  = info_nb.dst_el
      tp_table = info_nb.dst_tbl
      tp_pee = info_nb.dst_pee
      tp_bee =  info_nb.dst_bee
      tp_see =  info_nb.dst_see
      tp_slng =  info_nb.dst_slng
      tp_slat = info_nb.dst_slat
      tp_llevel = info_nb.dst_llevel
      lightlevel = tp_llevel /  avg(tp_llevel)
 
        ;;#################################################################
        ;; Flatfield and lightlevel correction for nb and wl
        ;;#################################################################

        ;; Apply gain table and lightlevel for each wavelength separately.
 
  
        print,'--------------------------------------------------------------------'
     ;   print,'Light level and gain table correction for narrowband                '
        print,'Light level for whitelight channel                                  '
        print,'--------------------------------------------------------------------'

        mwldn   = 0. * mwld
        FOR i=0,(size(mwld))[3]-1 do mwldn[*,*,i] = mwld[*,*,i] / lightlevel[i]

        mwld = mwldn  ;changed 20110207 LK
        mwldn = 0
        wcgain = 0
        s = size(mwld)

;### todo: choose subaperture for destretch etc. see above
stop

        ;;#################################################################
        ;; Do the destretch with IDL code
        ;;#################################################################

        PRINT,'-----------------------------------------'
        PRINT,'Destretch starts now .....               '
        PRINT,'-----------------------------------------'

        IF KEYWORD_SET(DSTR_IDL) THEN BEGIN

           PRINT,'-----------------------------------------'
           PRINT,'Destretch mode: IDL-code                 '
           PRINT,'Method : ', method
           PRINT,'Kernel : ', DS_KERNEL
           PRINT,'-----------------------------------------'
;todo: add smaller fov
           If method EQ 'AVG' THEN BEGIN
             ref = AVG(mwld,2)
             mwld2 = DESTRETCH_IBIS(ref, mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid, idlbridge = bridges)
           ENDIF

           IF method EQ 'SEQ' THEN BEGIN
             mwld2 = DESTRETCH_IBIS(AVG_IMG[0], mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid, idlbridge = bridges)
           ENDIF

           IF method EQ 'SPECKLE' THEN BEGIN

             ;;
             ;; ref in this case can be one or >1 image!
             ;;
             stop ;add limb version, who would use single beam polarimetry anyway...
             ref = simage
             mwld2 = DESTRETCH_IBIS(ref, mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid, idlbridge = bridges)

           ENDIF

           ;;#################################################################
           ;; Destretch narrowband and blueshift
           ;;#################################################################

           PRINT,'-------------------------------------------------------'
           PRINT,'Destretch of whitelight done .....                     '
           PRINT,'Now we destretch the narrowband and the blueshift map  '
           PRINT,'-------------------------------------------------------'

           ;;
           ;; destretch narrowband
           ;;

           nb = 0. * nbdwc
           for i=0,dim[3]-1 DO BEGIN
               nb[*, *, i] = DOREG(nbdwc[*, *, i], grid, shft_tot[*, *, *, i])
           endfor

           ;;
           ;; destretch blueshift
           ;;

           bshift_mod = bshift
           blds = REBIN(bshift_mod, Ncol, Nrow, Npol*Nwave)

           bl = 0. * blds
           for i=0,dim[3]-1 DO BEGIN
               bl[*, *, i] = DOREG(blds[*, *, i], grid, shft_tot[*, *, *, i])
           endfor

        ENDIF ;--DSTR_IDL

        ;;#################################################################
        ;; Perform blueshift correction
        ;;#################################################################

        PRINT,'-------------------------------------------------------'
        PRINT,'Now we correct the narrowband for the blueshift        '
        PRINT,'-------------------------------------------------------'

        nb = REFORM(nb, Ncol, Nrow, Npol, Nwave)
        bl = REFORM(bl, Ncol, Nrow, Npol, Nwave)

        wtmp = REFORM(nb_expos.wavelength_offset)
        wtmp = wtmp[uniq(wtmp)]
        index_wl = sort(wtmp)
        wscale = wtmp[index_wl]

        nbdwc = FLTARR(Ncol, Nrow, Npol, Nwave)

        FOR j = 0, Nrow-1 DO BEGIN
            FOR i = 0, Ncol-1 DO BEGIN
                FOR k = 0, Npol-1 DO BEGIN
                   nbdwc[i, j, k, *] = REFORM(INTERPOL(REFORM(nb[i, j, k, index_wl]), wscale, $
                                                       wscale + REFORM(bl[i, j, k, index_wl])))
                ENDFOR
            ENDFOR
        ENDFOR

        mwld = mwld2 & mwld2 = 0

        ;;#################################################################
        ;; Mask outer field and fill with average value of inside
        ;;#################################################################

        mask = fix(pupil)
        index_out_new = WHERE(mask EQ 0)
        index_in_new = WHERE(mask EQ 1)

        FOR k = 0, Npol-1 DO BEGIN
            FOR i = 0, Nwave-1 DO BEGIN
               tmp = REFORM(nbdwc[*,*,k,i])
               tmp[index_out_new] = AVG(tmp[index_in_new])
               nbdwc[*,*,k,i] = tmp
            ENDFOR
        ENDFOR

        ;;#################################################################
        ;; Show data
        ;;#################################################################

       ; window, 0, xs = 3*Ncol, ys = Nrow
       ; tvscl, AVG(REFORM(nbdwc[*,*,0,*]),2),0,0
       ; tvscl, AVG(REFORM(nbdwc[*,*,0,*]),2),Ncol,0
       ; tvscl, AVG(mwld,2),2*Ncol,0

     ENDIF ;--SINGLE

     ;;#################################################################
     ;; Save data
     ;;#################################################################

     ;;### todo
     file = (STRMID(s_file, (STRSPLIT(s_file, '/'))))[N_ELEMENTS(STRSPLIT(s_file, '/'))-1]

     out_file = o_dir + '/' + file

     PRINT,'-------------------------------------------------------'
     PRINT, 'Saved : ', out_file
     PRINT,'-------------------------------------------------------'

     if flagncam EQ 1 then nb_expos = info_nb

     IF KEYWORD_SET(SPECKLE) THEN BEGIN

        SAVE, FILENAME = out_file, $ 
          nbdwc, mwld, nb_expos, simage, modulation, shft_tot, bshift_mod, $
          tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng, $
          /comp, /verb, wl_obs
        
     ENDIF ELSE BEGIN

           SAVE, FILENAME = out_file, $ 
                 nbdwc, mwld, nb_expos, modulation, shft_tot, bshift_mod, $ 
                 tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng,$
                 /comp, /verb, wl_obs
   
     ENDELSE

  ENDIF ELSE BEGIN ;--POL

 ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 ;; 
 ;; DONE SPECTROPOLARIMETRIC CASE
 ;;
 ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
 ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        PRINT,'---------------------------------------------------'
        PRINT,'Ok, We are in Spectroscopic Mode                   '
        PRINT,'---------------------------------------------------'

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Get lightlevel from telescope if existent or from Whitelight
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

         tp_azim  = info_nb.dst_az
         tp_elev  = info_nb.dst_el
         tp_table = info_nb.dst_tbl
         tp_pee = info_nb.dst_pee
         tp_bee =  info_nb.dst_bee
         tp_see =  info_nb.dst_see
         tp_slng =  info_nb.dst_slng
         tp_slat = info_nb.dst_slat
         tp_llevel = info_nb.dst_llevel
         lightlevel = tp_llevel /  avg(tp_llevel)
 
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Get speckle reconstruction if existent
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

           IF file_test(speckle_nbdb) THEN BEGIN

           print,'------------------------------------------------------'
           print,'Identify the speckle reconstruction for the scan      '
           print,'------------------------------------------------------'

           index = where(strpos(nb_speckle_db.filename,strmid(s_file,29,30,/rev)) ne -1)
           if index[0] eq -1 then message,'No correct speckle file found'

           burst = speckle_path + strmid(nb_speckle_db.burst[index],31,33,/rev)
     

           IF (N_ELEMENTS(burst) EQ 1 and burst[0] ne -1) THEN BEGIN 

              print,'------------------------------------------------------'
              print,' One reconstruction found : ', burst
              print,'------------------------------------------------------'

              RESTORE, /verb, burst
              tmp=simage

               ;new 2014: just rotate speckle image
              simage = newwl_v2(tmp,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl) 

   
           ENDIF ;--burst

         IF N_ELEMENTS(burst) GT 1 THEN BEGIN ;### should not happen for new cam
             print,'this should not happen, check ibis_calibrate_bridges'
             stop
             print,'------------------------------------------------------'
             print,' More than one reconstruction found : ', burst
             print,'------------------------------------------------------'
            ;*** lk: deleted all lines
           ENDIF ;--BURST

      ENDIF ;--speckle

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Flatfield and lightlevel correction for nb and wl
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

       ;;
        ;; Apply lightlevel for each wavelength offset separately.
        ;;
 
        mwldn = 0.* mwld
     
       ;LK, april 2017:    
        FOR i=0,(size(mwld))[3]-1 do mwldn[*,*,i] = mwld[*,*,i] / lightlevel[i]
 
       ; FOR n=0, Nwave-1 DO BEGIN  
       ;     wltmp = mwld[*,*,n] / lightlevel[n]
       ;     wltmp[index_out] = avg(wltmp[index_in])
       ;     mwldn[*, *, n] = wltmp
       ; ENDFOR

        mwld = mwldn
        mwldn = 0
        wcgain = 0
        s = size(mwld)

       ;copied from polarimetry part
        aps_mod = newwl_v2(pupil,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
        bshift_mod = newwl_v2(bshift,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Do the destretch with IDL-code
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        IF KEYWORD_SET(DSTR_IDL) THEN BEGIN

           PRINT,'-----------------------------------------'
           PRINT,'Destretch mode: IDL-code                 '
           PRINT,'Method : ', method
           PRINT,'Kernel : ', DS_KERNEL
           PRINT,'-----------------------------------------'

;this part must be before 'avg', 'seq' etc to have all limits defined 
             mask_mod = newwl_v2(mask,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
;mod 150331 to avoid making mask too big if newwl_v2 shows
;interpolation issues (i.e. FOV too far right on the camera)
             ind = where(mask_mod[*,*,0] ge .9,complement=compind)
             tmpmask = mask_mod[*,*,0]
             tmpmask[compind] = 0.
             mask_mod[*,*] = tmpmask

             tmp = ibis_mask(mask_mod[*,*,0],cut=2)
             tmp = where2d(tmp eq 1)
             xl = min(tmp[0,*])
             xr = max(tmp[0,*])
             yb = min(tmp[1,*])
             yt = max(tmp[1,*])
             region[0,*] = region[0,*] - xl
             region[1,*] = region[1,*] - yb
   
 
           ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           ;; Destretch whitelight
           ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;not parallelized yet...
             print,'running destretch_ibis.pro'
           IF method EQ 'AVG' THEN BEGIN
              ref = AVG(mwld,2)
               mwld2=mwld
               mwld2[xl:xr,yb:yt,*] = destretch_ibis(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, $
                           DT_DETR, shifts=shft_tot, grid=grid)
            ENDIF

           IF method EQ 'SEQ' THEN BEGIN
              mwld2=mwld
              mwld2[xl:xr,yb:yt,*] = destretch_ibis(AVG_IMG[0], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR,$
                     shifts=shft_tot, grid=grid)
        ENDIF

           IF method EQ 'SPECKLE' THEN BEGIN

              ;;
              ;; ref in this case can be one or >1 image!
              ;;
               message,'add limb version in code or type .continue if not limb'
               ref = simage
               mwld2 = mwld
               mwld2[xl:xr,yb:yt,*] = DESTRETCH_IBIS(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR, $
                                                    shifts=shft_tot, grid=grid, idlbridge = bridges) 

           ENDIF

           mwld = mwld2
           nbdwc = nb_data

           ;;#################################################################
           ;; Destretch narrowband and blueshift array
           ;;#################################################################

           print,'-------------------------------------------------------'
           print,'Destretch of whitelight done .....                     '
           print,'Now we destretch the narrowband and the blueshift map  '
           print,'-------------------------------------------------------'

           IF N_ELEMENTS(DS_KERNEL) EQ 1 THEN BEGIN
              
              PRINT,'Align shift :', shft_tot[*, 0, 0, 0]

              nb = 0. * nbdwc
              FOR i = 0,dim[3]-1 DO BEGIN
                  nb[*, *, i] = SHIFT_BICUB(nbdwc[*, *, i], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              ENDFOR

              blds = REBIN(bshift, Ncol, Nrow, Npol*Nwave)
              bl = 0. * blds
              FOR i = 0,dim[3]-1 DO BEGIN
                  bl[*, *, i] = SHIFT_BICUB(blds[*, *, i], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              ENDFOR

           ENDIF ELSE BEGIN

                 ;;
                 ;; destretch narrowband
                 ;;

                 nb = 0. * nbdwc
                 for i=0,s[3]-1 DO BEGIN
                    nb[xl:xr, yb:yt, i] = doreg(nbdwc[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
                 endfor

                 ;;
                 ;; destretch blueshift
                 ;;

                 blds = REBIN(bshift, Ncol, Nrow, Npol*Nwave)

                 bl = 0. * blds
                 for i=0,s[3]-1 DO BEGIN
                   bl[xl:xr, yb:yt, i] = doreg(blds[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
                 endfor

           ENDELSE

        ENDIF ;--DSTR_IDL

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Perform blueshift correction
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;R>1 not implemented. See polarimetry, where it already is implemented.


        nbdwc = FLTARR(s[1],s[2],Nwave)

        FOR i = 0, s[1]-1 DO BEGIN
            FOR j = 0, s[2]-1 DO BEGIN

                nbdwc[i, j, *] = REFORM(INTERPOL(REFORM(nb[i, j, index_wl]), wscale, $
                                                 wscale + REFORM(bl[i, j, *])))
            ENDFOR
        ENDFOR

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Mask outer field and fill with average value of inside
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;april 2017, get_ibis_aperture_new crashes, use own routine instead
     ;   maperture = get_ibis_aperture_new(reform(mask),/plot)
        mask_new = ibis_mask(reform(mask))

        index_out_new = WHERE(mask_new EQ 0)
        index_in_new = WHERE(mask_new EQ 1)

        IF N_ELEMENTS(burst) EQ 1 THEN simage[index_out] = AVG(simage[index_in_new])
                
        IF N_ELEMENTS(burst) NE 1 THEN BEGIN
           for i=0,N_ELEMENTS(burst)-1 DO BEGIN
               tmp = REFORM(simage[*,*,i])
               tmp[index_out] = AVG(tmp[index_in_new])
               simage[*,*,i] = tmp
           endfor
        ENDIF

        FOR i = 0, s[3]-1 DO BEGIN
            tmp = REFORM(mwld[*,*,i])
            tmp[index_out_new] = AVG(tmp[index_in_new])
            mwld[*,*,i] = tmp
            tmp = REFORM(nbdwc[*,*,i])
            tmp[index_out_new] = AVG(tmp[index_in_new])
            nbdwc[*,*,i] = tmp
        ENDFOR

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Save data
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;new april 2017 
       cpath = strmid(g_file,0,strlen(g_file)-13)

      if file_test(cpath+'cutoffpx.txt') then begin
         openr,1,cpath+'cutoffpx.txt'
         readf,1,lpos,rpos,bpos,tpos
         close,1
      endif else begin
           ;get cutoffs for image
           tmp = max(deriv(aps_mod[0:dim[1]/2,dim[2]/2,0]),lpos) ;left edge of mask
           tmp = max(deriv(aps_mod[dim[1]/2:*,dim[2]/2,0]),rpos) ;r edge of mask
           rpos = rpos + dim[1]/2
           tmp = max(deriv(aps_mod[dim[1]/2,0:dim[2]/2,0]),bpos) ;bottom edge of mask
;mod LK 150414. Issue was that if mask spanned full FOV, then tpos would
;find dim[2]/2 as index instead of dim[2]-1
           tpos = max(where(max(deriv(aps_mod[dim[1]/2,dim[2]/2:*,0])) eq deriv(aps_mod[dim[1]/2,dim[2]/2:*,0]))) ;top edge of mask
           tpos = tpos + dim[2]/2
     ;write file
           openw,1,cpath+'cutoffpx.txt'
           printf,1,lpos,rpos,bpos,tpos
           close,1   
        endelse 

        nbdwc = nbdwc[lpos:rpos,bpos:tpos,*] ;add one more dimension for R>1
        mwld = mwld[lpos:rpos,bpos:tpos,*]
        bshift_mod = bshift_mod[lpos:rpos,bpos:tpos]

        ;mod 140414 LK, simage may not exist...
        if file_test(speckle_nbdb) then simage = simage[lpos:rpos,bpos:tpos]

        file = (STRMID(s_file, (STRSPLIT(s_file, '/'))))[N_ELEMENTS(STRSPLIT(s_file, '/'))-1]

        out_file = o_dir + '/' + file

        PRINT,'----------------------'
        PRINT, 'Saved : ', out_file
        PRINT,'----------------------'

if flagncam eq 1 then nb_expos = info_nb

          SAVE, FILENAME = out_file, $ 
                 nbdwc, mwld, nb_expos, simage, shft_tot, bshift_mod, $
                 tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng, $
                 /comp, /verb, wl_obs

        ENDIF ELSE BEGIN
;maybe add simage, or is it not defined in non-speckle mode?
              SAVE, FILENAME = out_file, $ 
                    nbdwc, mwld, nb_expos, shft_tot, bshift_mod, $
                    tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng, $
                     /comp, /verb, wl_obs

        ENDELSE

       ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ;;
       ;; DONE SPECTROSCOPIC CASE
       ;;
       ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  ENDELSE

;;***************************************************************
;; Done
;;***************************************************************

;;#################################################################
;; DESTROY IDL BRIDGES
;;#################################################################

PRINT,' now destroying bridges '
FOR i = 0.,ncpus-1 DO begin
   print,i,' destroying...'
  OBJ_DESTROY,(bridges[i])
ENDFOR
print,'done'
END
