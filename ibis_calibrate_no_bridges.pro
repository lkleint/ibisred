;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Main procedure to calibrate the IBIS observations for darks
;; gain, blueshift and image distortions (destretch). The procedure
;; uses the output of ibis_combine.pro as input data.
;;
;; oct 11: modified for flagncam
;;; single and spectroscopy speckle not done yet
;; jan 14: modified for new reduction
;; march 2015: fixed bugs in seq/avg and for shifts due to newwl_v2 
;; june 2015: ### todo: probably restoring d_file etc is not necessary
;; because this is being done in ibis_combine
;; june 2015: added wl_obs to save variables
;; oct 2016: added reduction for R>1 (only) for dual beam. nbdwc will
;; have another dimension containing R images
;; april 2017: adapted and tested spectroscopy case (single beam
;; polarimetry remains untested and would require changes)
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_calibrate_no_bridges, o_dir, g_file, d_file, s_file, wl_file, a_file, bshift, $ 
                    pupil, snumber, $ 
                    POL = pol, TPFILE = tpfile, DSTR_IDL = dstr_idl, $
                    SPECKLE = speckle, SINGLE = single, DUAL = dual, $
                    limb=limb, flagncam=flagncam

;is called once per wl and directory

;;***************************************************************
;; Parameters
;;**************************************************************

  ;;
  ;; Parameters for the destretch grids
  ;;

  If keyword_set(DSTR_IDL) then begin
     DS_KERNEL = (dstr_idl.kernel)
     DT_DETR   = (dstr_idl.detrend)
     METHOD    = (dstr_idl.method)
     REGION    = (dstr_idl.region)
     AVG_IMG   = (dstr_idl.avg_img)
  endif

  If keyword_set(SPECKLE) then begin
     speckle_path = (speckle.datapath)
     speckle_nbdb = (speckle.database)
  endif


;;***************************************************************
;; Read data
;;***************************************************************
  ;;
  ;; Restore files.
  ;; 
  restore, g_file
  restore, d_file
  restore, s_file
  restore, wl_file
  restore, a_file

  IF file_test(speckle_nbdb) then restore, /verb, speckle_nbdb else print,'No speckle file found'

  dim   = size(nb_data)
  Ncol  = dim[1]
  Nrow  = dim[2]
  
  npol = diffelement(info_nb.stokes) 
;  IF KEYWORD_SET(TPFILE) THEN restore, /verb, tpfile
  Nwave = dim[3]/Npol
  modulation = (info_nb.stokes)[0:Npol-1]

 ;old data has different structure for info_flat_short
if (size(info_flat_short))[0] eq 2 then info_flat_short = reform(info_flat_short[*,0])

  print,'---------------------------------------------------'
  print,'Ok, here the dimensions:                           '       
  print,'Spatial dimensions: ', Ncol, Nrow                   
  print,'Wavelength points: ', Nwave
  print,'Number of modulation states : ', Npol
  print,'Modulation scheme : ', modulation
  print,'Does that seem right ?'
  print,'---------------------------------------------------'


;;***************************************************************
;; Determine mask indexes
;;***************************************************************

  mask = fix(pupil)
  index_in = WHERE(mask EQ 1)
  index_out = WHERE(mask EQ 0)


;;***************************************************************
;; Get index for ordering wavelengths
;;***************************************************************

  wtmp = reform(info_nb.wave)
  wtmp = wtmp[uniq(wtmp)]
  index_wl = sort(wtmp)  ;wavelength index for increasing order of lambda
  wscale = wtmp[index_wl] ;wavelengths in increasing order
;### check this. could probably be done in 1 line with diffelement()

; deal with R>1 cases (repetitions of same WL)
  rr = n_elements(where(info_nb.wave eq info_nb.wave[0] and info_nb.stokes eq info_nb.stokes[0]))
  if rr ne 1 then begin
     print,'Repetitions of same wl step exist. R='+string(rr,format='(I3)')+' ! '
     nrep = rr
  endif

;need to create additional dimension of array. cannot simply replicate
;wscale rr-times because blueshift interpolation would no longer work.

;;#################################################################
;;#################################################################
;; 
;; POLARIMETRY CASE
;;
;;#################################################################
;;#################################################################

  IF KEYWORD_SET(POL) THEN BEGIN

     IF KEYWORD_SET(DUAL) THEN BEGIN

         print,'---------------------------------------------------'
         print,'Ok, We are in Polarimetric dual-beam Mode                    '
         print,'---------------------------------------------------'

        ;;#################################################################
        ;; Define masks
        ;;#################################################################

        ;; Get one rectangular mask for both beams

;        t1 = get_rect_from_aps_pol(pupil[0:Ncol/2-1, 0:Nrow-1])
;        t2 = get_rect_from_aps_pol(pupil[Ncol/2:Ncol-1, 0:Nrow-1])
;        x1 = MAX([t1[0,0],t2[0,0]]) & dx = MIN([t1[1,0],t2[1,0]]) - x1
;        y1 = MAX([t1[0,1],t2[0,1]]) & dy = MIN([t1[1,1],t2[1,1]]) - y1

        ;;#################################################################
        ;; Get speckle reconstruction if existent
        ;;#################################################################

        IF file_test(speckle_nbdb) THEN BEGIN

           ;;
           ;; Find corresponding speckle reconstruction using database
 
           print,'------------------------------------------------------'
           print,'Identify the speckle reconstruction for the scan      '
           print,'------------------------------------------------------'

           ;mod LK 31.7.14: issue if computers/paths changed during reduction
           ;new: only compare timestamp and filename
;           index = where(s_file eq nb_speckle_db.filename)
           index = where(strpos(nb_speckle_db.filename,strmid(s_file,29,30,/rev)) ne -1)
           if index[0] eq -1 then message,'No correct speckle file found'
;           burst = nb_speckle_db.burst[index] ;### one reconstr. per file, burst is filename in this case
           burst = speckle_path + strmid(nb_speckle_db.burst[index],31,33,/rev)
 

           IF (N_ELEMENTS(burst) EQ 1 and burst[0] ne -1) THEN BEGIN 

              print,'------------------------------------------------------'
              print,' One reconstruction found : ', burst
              print,'------------------------------------------------------'

              RESTORE, /verb, burst
              tmp=simage

               ;new 2014: just rotate speckle image
               simage = newwl_v2(tmp,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl)

   
              ;;
              ;; Shift speckle reconstruction on average whitelight or
              ;; to correct for atmospheric refraction. This is
              ;; corrected for in the whitelight already but not in
              ;; the reconstruction!

;no longer to this (2014): align everything to speckle image
;              delta = shc((avg(mwld,2))[Ncol/4-64:Ncol/4+64,Nrow/2-64:Nrow/2+64], $ 
;                          simage[Ncol/4-64:Ncol/4+64,Nrow/2-64:Nrow/2+64], interpolate=1)
;              simage = (shift_bicub(simage,delta[0],delta[1]))[0:Ncol/2-1,0:Nrow-1]
;              simage[index_out] = avg(simage[index_in])

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
               lightlevel = tp_llevel /  avg(tp_llevel)
 

  
        ;;#################################################################
        ;; Flatfield ((and lightlevel)) correction for nb and wl
        ;;#################################################################
;-> mod January 2014: now done in ibis_combine

        ;; Apply gain table and lightlevel for each wavelength separately.
        ;; omit light level correction for now. ll is updated each 10s
        ;; i.e. too little for a good correction. A jump/update in ll between
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



;---- testing -------
;new Nov 2014: get rid of intensity variations due to modulation
;such that difference of orthogonal polarization states actually shows some
;polarization
;the flats do not do this well enough

;        print,'************ ADAPTING POL STATES FOR SAME LIGHT LEVEL !!! **************'
;        ibis_adapt_polstates,nb_data,nbout,s_file,info_nb
;        nb_data=nbout
;this turned out to be bad, because one loses the information for the polcal. So one would need to rotate
;the poincare sphere possibly differently at each pixel with no info how much...


        ;;#################################################################
        ;; Alignment between left and right channel
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


        print,'-----------------------------------------'
        print,'Destretch starts now .....               '
        print,'-----------------------------------------'

        IF KEYWORD_SET(DSTR_IDL) THEN BEGIN

           print,'-----------------------------------------'
           print,'Destretch mode: IDL-code                 '
           print,'Method : ', method
           print,'Kernel : ', DS_KERNEL
           print,'-----------------------------------------'

 ;apr12: this part must be before 'avg', 'seq' etc to have all limits defined 
             mask_mod = newwl_v2(mask,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
;mod 150331 to avoid making mask too big if newwl_v2 shows
;interpolation issues (i.e. FOV too far right on the camera)
             ind = where(mask_mod[*,*,0] ge .9,complement=compind)
             tmpmask = mask_mod[*,*,0]
             tmpmask[compind] = 0.
             mask_mod[*,*,0] = tmpmask

             tmp = ibis_mask(mask_mod[*,*,0],cut=2)
             tmp = where2d(tmp eq 1)
             xl = min(tmp[0,*])
             xr = max(tmp[0,*])
             yb = min(tmp[1,*])
             yt = max(tmp[1,*])
             region[0,*] = region[0,*] - xl
             region[1,*] = region[1,*] - yb

        ;     print,xl,xr,yb,yt,region[0,0],region[1,0],region[0,1],region[1,1]
           print,'running destretch_ibis.pro'
           If method eq 'AVG' then begin
             ref = avg(mwld,2)
             mwld2=mwld
             mwld2[xl:xr,yb:yt,*] = destretch_ibis(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, $
                          DT_DETR, shifts=shft_tot, grid=grid)
           ENDIF

           IF method EQ 'SEQ' THEN BEGIN
              ;avg_img is a number to indicate #image to average
              mwld2=mwld
              mwld2[xl:xr,yb:yt,*] = destretch_ibis(AVG_IMG[0], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR,$
                     shifts=shft_tot, grid=grid)
           ENDIF

           IF method EQ 'SPECKLE' THEN BEGIN 
            ref = simage
            mwld2 = mwld
            mwld2[xl:xr,yb:yt,*] = DESTRETCH_IBIS(ref[xl:xr,yb:yt], mwld[xl:xr,yb:yt,*], DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid, idlbridge = bridges) 
	
          ENDIF

      ;     save,filename='destretch_test.sav',mwld2,mwld,ref,xl,xr,yb,yt,ds_kernel,region,dt_detr,shft_tot,grid
     ;      stop

           ;;#################################################################
           ;; Destretch narrowband and blueshift
           ;;#################################################################

           print,'-------------------------------------------------------'
           print,'Destretch of whitelight done .....                     '
           print,'Now we destretch the narrowband and the blueshift map  '
           print,'-------------------------------------------------------'



          IF N_ELEMENTS(DS_KERNEL) EQ 1 THEN BEGIN ;new version only
              
           print,'Align shift :', shft_tot[*, 0, 0, 0]

           nbl = 0. * nb_data

           ;linear shift for left and right beam
           FOR i = 0,dim[3]-1 DO BEGIN
               nb[*, *, i,0] = shift_bicub(nb_data[*, *, i,0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
               nb[*, *, i,1] = shift_bicub(nb_data[*, *, i,1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
           ENDFOR

           ;linear shift for blueshift map
           blds = rebin(bshift_mod, Ncol, Nrow, 2, Npol*Nwave)
        ;   bshift_dstr = 0. * blds
        ;   FOR i = 0,dim[3]-1 DO BEGIN
        ;       bshift_dstr[*, *, i, 0] = shift_bicub(blds[*, *, i, 0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
        ;       bshift_dstr[*, *, i, 1] = shift_bicub(blds[*, *, i, 1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
        ;   ENDFOR
           FOR i = 0,dim[3]-1 DO BEGIN
              blsl[*, *, i, 0] = shift_bicub(blds[*, *, i, 0], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              blsr[*, *, i, 1] = shift_bicub(blds[*, *, i, 1], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
           ENDFOR


        ENDIF ELSE BEGIN  ;usual case:

           ;;
           ;; destretch narrowband left and right
           ;;

            ;shft_tot is given by the destretch of the wl images and now applied to nb
            ;only for subframe (not the black edges)
           FOR i = 0,dim[3]-1 DO BEGIN
               nbsl[xl:xr, yb:yt, i] = doreg(nbsl[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
               nbsr[xl:xr, yb:yt, i] = doreg(nbsr[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
           ENDFOR

           ;;
           ;; right and left blueshift
           ;;
           ;shifts applied to offset maps
           FOR i=0,dim[3]-1 DO BEGIN
              blsl[xl:xr, yb:yt, i] = doreg(blsl[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
              blsr[xl:xr, yb:yt, i] = doreg(blsr[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
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
;mod 27.10.16 to make it work for R>1
;another option would be to include lucky imaging or speckle of 5
;images here

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

        ;;#################################################################
        ;; Mask outer field and fill with average value of inside
        ;;#################################################################

        mask = (aps_mod[*,*,0] gt 0.)
        index_out_new = WHERE(mask EQ 0)
        index_in_new = WHERE(mask EQ 1)

;lk mod 27.10.16, added nrep
        FOR k = 0, Npol-1 DO BEGIN
            FOR i = 0, Nwave-1 DO BEGIN
               for l = 0,nrep-1 do begin
               tmpl = reform(nbdwc[*,*,k,i,0,l])
               tmpl[index_out_new] = AVG(tmpl[index_in_new])
               nbdwc[*,*,k,i,0,l] = tmpl
               tmpr = reform(nbdwc[*,*,k,i,1,l])
               tmpr[index_out_new] = AVG(tmpr[index_in_new])
               nbdwc[*,*,k,i,1,l] = tmpr
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
;mod LK 150414. Issue was that if mask spanned full FOV, then tpos would
;find dim[2]/2 as index instead of dim[2]-1
           tpos = max(where(max(deriv(aps_mod[dim[1]/2,dim[2]/2:*,0])) eq deriv(aps_mod[dim[1]/2,dim[2]/2:*,0]))) ;top edge of mask
           tpos = tpos + dim[2]/2
          ;write file
           openw,1,cpath+'cutoffpx.txt'
           printf,1,lpos,rpos,bpos,tpos
           close,1   
        endelse 


        nbdwc = nbdwc[lpos:rpos,bpos:tpos,*,*,*,*]
        mwld = mwld[lpos:rpos,bpos:tpos,*]
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

         print,'---------------------------------------------------'
         print,'Ok, We are in Polarimetric single-beam Mode                    '
         print,'---------------------------------------------------'

        ;;#################################################################
        ;; Get speckle reconstruction if existent
        ;;#################################################################

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

        print,'-----------------------------------------'
        print,'Destretch starts now .....               '
        print,'-----------------------------------------'

        IF KEYWORD_SET(DSTR_IDL) THEN BEGIN

           print,'-----------------------------------------'
           print,'Destretch mode: IDL-code                 '
           print,'Method : ', method
           print,'Kernel : ', DS_KERNEL
           print,'-----------------------------------------'
;todo: add smaller fov
           If method eq 'AVG' then begin
             ref = avg(mwld,2)
             mwld2 = destretch_ibis(ref, mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid)
           ENDIF

           IF method EQ 'SEQ' THEN BEGIN
             mwld2 = destretch_ibis(AVG_IMG[0], mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid)
           ENDIF

           IF method EQ 'SPECKLE' THEN BEGIN

             ;;
             ;; ref in this case can be one or >1 image!
             ;;
             stop ;add limb version, who would use single beam polarimetry anyway...
             ref = simage
             mwld2 = destretch_ibis(ref, mwld, DS_KERNEL, REGION, DT_DETR, shifts=shft_tot, grid=grid)

           ENDIF

           ;;#################################################################
           ;; Destretch narrowband and blueshift
           ;;#################################################################

           print,'-------------------------------------------------------'
           print,'Destretch of whitelight done .....                     '
           print,'Now we destretch the narrowband and the blueshift map  '
           print,'-------------------------------------------------------'

           ;;
           ;; destretch narrowband
           ;;

           nb = 0. * nbdwc
           for i=0,dim[3]-1 do begin
               nb[*, *, i] = doreg(nbdwc[*, *, i], grid, shft_tot[*, *, *, i])
           endfor

           ;;
           ;; destretch blueshift
           ;;

           bshift_mod = bshift
           blds = rebin(bshift_mod, Ncol, Nrow, Npol*Nwave)

           bl = 0. * blds
           for i=0,dim[3]-1 do begin
               bl[*, *, i] = doreg(blds[*, *, i], grid, shft_tot[*, *, *, i])
           endfor

        ENDIF ;--DSTR_IDL

        ;;#################################################################
        ;; Perform blueshift correction
        ;;#################################################################

        print,'-------------------------------------------------------'
        print,'Now we correct the narrowband for the blueshift        '
        print,'-------------------------------------------------------'

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
               tmp = reform(nbdwc[*,*,k,i])
               tmp[index_out_new] = AVG(tmp[index_in_new])
               nbdwc[*,*,k,i] = tmp
            ENDFOR
        ENDFOR

        ;;#################################################################
        ;; Show data
        ;;#################################################################

;        window, 0, xs = 3*Ncol, ys = Nrow
;        tvscl, avg(reform(nbdwc[*,*,0,*]),2),0,0
;        tvscl, avg(reform(nbdwc[*,*,0,*]),2),Ncol,0
;        tvscl, avg(mwld,2),2*Ncol,0

     ENDIF ;--SINGLE

     ;;#################################################################
     ;; Save data
     ;;#################################################################

;### todo
     file = (STRMID(s_file, (STRSPLIT(s_file, '/'))))[N_ELEMENTS(STRSPLIT(s_file, '/'))-1]

     out_file = o_dir + '/' + file

     PRINT,'-------------------------------------------------------'
     PRINT, 'Saved : ', out_file
     PRINT,'-------------------------------------------------------'

if flagncam eq 1 then nb_expos = info_nb

     IF KEYWORD_SET(SPECKLE) THEN BEGIN

        SAVE, FILENAME = out_file, $ 
              nbdwc, mwld, nb_expos, simage, modulation, shft_tot, bshift_mod, $
              tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng, $
              /comp, /verb, wl_obs

     ENDIF ELSE BEGIN

           SAVE, FILENAME = out_file, $ 
                 nbdwc, mwld, nb_expos, modulation, shft_tot, bshift_mod, $ 
                 tp_azim, tp_elev, tp_table, tp_llevel, tp_pee, tp_bee, tp_see, tp_slat, tp_slng, $
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

        print,'---------------------------------------------------'
        print,'Ok, We are in Spectroscopic Mode                   '
        print,'---------------------------------------------------'

        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; Get lightlevel from telescope if existent or from Whitelight
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

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

              RESTORE, burst,/ ve
              tmp=simage

               ;new 2014: just rotate speckle image
              simage = newwl_v2(tmp,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl) 

         
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


        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ;; lightlevel correction for nb and wl
        ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

        ;;
        ;; Apply lightlevel for each wavelength offset separately.
        ;;
 ;-> mod January 2014: now done in ibis_combine

        mwldn = 0.* mwld

 ;LK, april 2017:    
    FOR i=0,(size(mwld))[3]-1 do mwldn[*,*,i] = mwld[*,*,i] / lightlevel[i]
    

;        FOR n=0, Nwave-1 DO BEGIN  
;            wltmp = mwld[*,*,n] / lightlevel[n]
;            wltmp[index_out] = avg(wltmp[index_in])
;            mwldn[*, *, n] = wltmp
;        ENDFOR

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

           print,'-----------------------------------------'
           print,'Destretch mode: IDL-code                 '
           print,'Method : ', method
           print,'Kernel : ', DS_KERNEL
           print,'-----------------------------------------'

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

             print,'running destretch_ibis.pro'
            IF method EQ 'AVG' THEN BEGIN
              ref = avg(mwld,2)
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
              
              print,'Align shift :', shft_tot[*, 0, 0, 0]

              nb = 0. * nbdwc
              FOR i = 0,dim[3]-1 DO BEGIN
                  nb[*, *, i] = shift_bicub(nbdwc[*, *, i], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              ENDFOR

              blds = rebin(bshift, Ncol, Nrow, Npol*Nwave)
              bl = 0. * blds
              FOR i = 0,dim[3]-1 DO BEGIN
                  bl[*, *, i] = shift_bicub(blds[*, *, i], shft_tot[0, 0, 0, i], shft_tot[1, 0, 0, i])
              ENDFOR

           ENDIF ELSE BEGIN

                 ;;
                 ;; destretch narrowband
                 ;;
;mod april 2017
                 nb = 0. * nbdwc
                 for i=0,s[3]-1 do begin
                     nb[xl:xr, yb:yt, i] = doreg(nbdwc[xl:xr, yb:yt, i], grid, shft_tot[*, *, *, i])
                 endfor

                 ;;
                 ;; destretch blueshift
                 ;;

                 blds = rebin(bshift, Ncol, Nrow, Npol*Nwave)

                 bl = 0. * blds
                 for i=0,s[3]-1 do begin
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
     ;   mask_new = shift(dist(Ncol), Ncol/2, Nrow/2)
     ;  mask_new = mask_new LT (maperture.radius-5)
        index_out_new = WHERE(mask_new EQ 0)
        index_in_new = WHERE(mask_new EQ 1)

        IF N_ELEMENTS(burst) EQ 1 THEN simage[index_out] = AVG(simage[index_in_new])
       
        IF N_ELEMENTS(burst) NE 1 THEN begin
           for i=0,N_ELEMENTS(burst)-1 do begin              
               tmp = REFORM(simage[*,*,i])
               tmp[index_out] = AVG(tmp[index_in_new])
               simage[*,*,i] = tmp
           endfor
        endif

        FOR i = 0, s[3]-1 DO BEGIN
            tmp = reform(mwld[*,*,i])
            tmp[index_out_new] = AVG(tmp[index_in_new])
            mwld[*,*,i] = tmp
            tmp = reform(nbdwc[*,*,i])
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


        If KEYWORD_SET(SPECKLE) THEN BEGIN
    
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

END
