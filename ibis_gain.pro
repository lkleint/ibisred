;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;;
;;
;;
;;requires: diffelement.pro
;;mod 23.11.14: make sure mean flat is = 1 for each polstate separately
;;mod 21.6.15: only take central 100x100 pixels for super bad profile
;;06/2016: issue for 6302 telluric line, gain looks strange
;;there. current solution is a manual change of gain afterwards
;;(outside of pipeline)
;;mod 13.4.17: fixed offl->off for single beam
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_gain, f_file, d_file, offset, aps, savefile, fproffile, $ 
               Pol = pol, SMOOTH = smooth, SINGLE = single, DUAL = dual, $
               flagncam = flagncam ;STOKES = stokes
       

;------------------ TEST IF GAIN EXISTS -----------------------------        
if file_test(savefile) then begin ;test if gain for given wavelength already exists
   print,'Gain for this wavelength already exists. Overwrite (y,n, def=n)?'
   print,savefile
   ans = ' '
   read,ans
   if ans ne 'y' then return
endif



;----------------- restore mean dark and flat -------------------------------

  restore, f_file, /verb ;restores _flat.sav
       ;info_flat_short has correct stokes sequence but wl are ascending (by ibis_blueshift.pro)
       ;which is not corrected in info_flat_short

  flat = nb_flat
  restore, d_file, /verb

     ;; get wavelength scale and make equidistant on 10 mA grid
     if flagncam ne 1 then $ ;old camera
     wtmp = REFORM((exposures.wavelength_offset)[*,0]) else $
     wtmp = reform(info_flat_short.wave) ;new camera

    ;new 110217 lk:
    wscale=diffelement(wtmp,/name,/double) ;array of observed wl offsets
    nwscale = ROUND((max(wscale) - min(wscale))/0.01d) 
    wscale_equi = findgen(nwscale)/(nwscale-1) $ 
                * (max(wscale) - min(wscale)) + min(wscale)

    ;; Get dimensions
    dim   = SIZE(nb_flat)
    Nx    = dim[1]
    Ny    = dim[2]
    Npol = diffelement(exposures[*,0].stokes) ;hoping that spectroscopy mode still has this header entry
    Nwave = dim[3]/Npol
    nb_flat = 0 ;free memory

    ;; Order flats when in rabbit mode -> flats already ordered by ibis_blueshift
    ;; interior of the aperture     
    temp = fltarr(Nx, Ny)

   ;; Get the outside indices for whole field
   index_out = where(aps eq 0)
   index_in = where(aps ge 1)

;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectroplarimetric modes
;;---------------------------------------------------------
;;---------------------------------------------------------

  IF keyword_set(POL) then begin

     print,'---------------------------------------------'
     print,'Ok, we are in spectropolarimetric mode ....  '
     print,'---------------------------------------------'

     IF keyword_set(DUAL) then begin

        print,'---------------------------------------------'
        print,'DUAL-BEAM mode ....  '
        print,'---------------------------------------------'

        ;; Get the full aperture and for left and right channel

        idx  = where(aps ge 1, Num)
        idxx = idx mod Nx
        idxy = idx / Nx

        idxl  = where_n(aps[0:Nx/2-1,*] ge 1) ;LEFT
        cxl = avg(idxl[*,0])  ;average ='center of gravity'
        cyl = avg(idxl[*,1])

 
        idxlx = idxl[*,0]
        idxly = idxl[*,1]
        Numl = n_elements(idxlx)

        idxr  = where_n(aps[Nx/2:Nx-1,*] ge 1) ;RIGHT
        cxr = avg(idxr[*,0])                    ;average ='center of gravity'
        cyr = avg(idxr[*,1])


        idxrx = idxr[*,0]
        idxry = idxr[*,1]
        Numr = n_elements(idxrx)

  
        ;; divide offset into left and right

        offl = offset[0:Nx/2-1,*]
        offr = offset[Nx/2:Nx-1,*]


     ;-------- dark ---------------
        ;; dark correction

        Nwave = dim[3]/Npol
        wcgain = fltarr(Nx, Ny, Nwave, Npol)

       ;Lk: the average dark is subtracted from single flats. what if T changed during
       ;the observations? Still correct result?
       print,'dark correction...' 
       FOR n=0, Nwave-1 DO begin
            for i=0, Npol-1 do begin
                temp[index_in] = (reform(flat[*,*,n*Npol+i]) - dark)[index_in]
                temp[index_out] = 1.
                wcgain[*, *, n, i] = temp
            ENDFOR
        endfor
      flat=0

    
        ;; Calculate "super-bad profile" on fine grid for both channels separately
        ;; Interpolate in wavelength to shift the wavelength scale at
        ;; each valid pixel position back to a reference
        ;LK: super-bad = averaged flat profile over FOV
        ;interpol shifts all profiles to the same value (i.e. no blueshift)

        sbpprof_l = fltarr(nwscale, Npol)
        sbpprof_r = fltarr(nwscale, Npol)

        print,'interpolating for average flat profile over FOV...'
        for j=0, Npol-1 do begin
            print,'polstate:',j,' of',npol-1
   ;         FOR i=0L, Numl-1 DO BEGIN
   ;             p = interpol(reform(wcgain[idxlx[i], idxly[i], *,j]), wscale, $
   ;                           wscale_equi+offl[idxlx[i], idxly[i]], /quadratic)
   ;             sbpprof_l[*,j] += p
   ;         endfor
            ;new June 2015:
           ;only take central 100x100 pixels for flat profile, because of differential 
           ;prefilter effects across fov, which should be calibrated too.
           ;this may lead to an error for a bad cxl, but that should not happen. If
           ;program crashes here, check aps.
            ctr=0
            FOR xx=(cxl-50),cxl+50 do begin
               FOR yy=(cyl-50),cyl+50 do begin
                  p = interpol(reform(wcgain[xx, yy, *,j]), wscale, $
                              wscale_equi+offl[xx,yy], /quadratic)
                  sbpprof_l[*,j] += p
                  ctr++
               ENDFOR
            ENDFOR


         ;   sbpprof_l[*,j] /= FLOAT(Numl)
            sbpprof_l[*,j] /= float(ctr)


           ; FOR i=0L, Numr-1 DO BEGIN
           ;     p = interpol(reform(wcgain[Nx/2+idxrx[i], idxry[i], *,j]), $
           ;       wscale, wscale_equi+offr[idxrx[i], idxry[i]], /quadratic)
           ;     sbpprof_r[*,j] += p
           ; endfor
           ; sbpprof_r[*,j] /= FLOAT(Numr)
            ctr=0
            FOR xx=(cxr-50),cxr+50 do begin
               FOR yy=(cyr-50),cyr+50 do begin
                  p = interpol(reform(wcgain[xx, yy, *,j]), wscale, $
                              wscale_equi+offl[xx,yy], /quadratic)
                  sbpprof_r[*,j] += p
                  ctr++
               ENDFOR
            ENDFOR
            sbpprof_r[*,j] /= float(ctr)

        ENDFOR

         
        if flagncam eq 0 then info_flat_short = exposures
        ;; save "super bad profile" for prefilter correction later
        save, sbpprof_r, sbpprof_l, wscale_equi, wscale, info_flat_short, filename=fproffile,/ve


        ;; Create shifted array to "gain" the mean flat array.
        wcgain_shift = 0. * wcgain
   
        print,'blueshifting mean profile...'
        for j=0, Npol-1 do begin ;for each pol state
            print,'polstate:',j,' of',npol-1
                for i=0L, Numl-1 do begin ;for each point in the left FOV

                wscale_blue  = wscale - offl[idxlx[i], idxly[i]] ;blueshifted wavelength
                wcgain_shift[idxlx[i], idxly[i], *,j] = INTERPOL(reform(sbpprof_l[*,j]), $
                   wscale_equi, wscale_blue, /quadratic) ;blueshifted mean profile

            endfor

            for i=0L, Numr-1 do begin

                wscale_blue  = wscale - offr[idxrx[i], idxry[i]]
                wcgain_shift[Nx/2+idxrx[i], idxry[i], *,j] = INTERPOL(reform(sbpprof_r[*,j]), $
                     wscale_equi, wscale_blue, /quadratic)

            endfor

        endfor
   
;wcgain_shift contains mean profiles at each pixel, shifted by the correct blueshift
;wcgain contains each single recorded flat - average dark

        ;; Calculate gain (1 / (divide flat by shifted sbpprof))
        print,'calculating gain...'
        wcgain_final = wcgain_shift / wcgain


;---- this was for testing and is incorrect even though the stokes
;     profiles look nice after this balancing ------------------------------
     ; this divides out the intensity differences -> this is ok if each
     ; flat is normalized to 1. The requirement is that the intensity
     ; differences need to be identical for X matrix and actual data        

     ;  cog1x = round(avg(idxlx))
     ;  cog1y = round(avg(idxly))
     ;  cog2x = round(avg(idxrx))+Nx/2.
     ;  xrg = round(stddev(idxlx)) 
     ;  yrg = round(stddev(idxly))

     ;  ;average over all wavelengths -> they ;vary by ~5% sometimes. Can it be done better?
     ;  wcgain_facl = fltarr(npol)
     ;  for i=0,npol-1 do wcgain_facl[i] = avg(wcgain[cog1x-xrg:cog1x+xrg,cog1y-yrg:cog1y+yrg,*,0])/avg(wcgain[cog1x-xrg:cog1x+xrg,cog1y-yrg:cog1y+yrg,*,i])
     ;  wcgain_facr = fltarr(npol)
     ;  for i=0,npol-1 do wcgain_facr[i] = avg(wcgain[cog2x-xrg:cog2x+xrg,cog1y-yrg:cog1y+yrg,*,0])/avg(wcgain[cog2x-xrg:cog2x+xrg,cog1y-yrg:cog1y+yrg,*,i])
     
     ;  ;determine difference in intensities of L and R beam
     ;  beamdiff =  avg(wcgain[cog1x-xrg:cog1x+xrg,cog1y-yrg:cog1y+yrg,*,0]) / avg(wcgain[cog2x-xrg:cog2x+xrg,cog1y-yrg:cog1y+yrg,*,0])

    ;   ;now scale wcgain_final with these values
    ;   ;beamdiff would not be necessary because beams are combined after balancing.
    ;    for j=0, Npol-1 do begin ;for each pol state
    ;        wcgain_final[0:nx/2.-1,*,*,j] =  wcgain_final[0:nx/2.-1,*,*,j] * wcgain_facl[j]
    ;        wcgain_final[nx/2.:*,*,*,j] =  wcgain_final[nx/2.:*,*,*,j] * wcgain_facr[j] * beamdiff
    ;   endfor
    ;   ;beamdiff would be different for the different polarization states
    ;   ;this means that the states are not even close to orthogonal


        wcgain=0
        wcgain_shift=0

     ENDIF ;DUAL keyword

     IF keyword_set(SINGLE) then begin

        print,'---------------------------------------------'
        print,'SINGLE-BEAM mode ....  '
        print,'---------------------------------------------'

     
         idx  = where_n(aps GE 1)
        idxx = idx[*,0]
        idxy = idx[*,1]
        Num = N_ELEMENTS(idxx)
        cx = avg(idx[*,0])  ;average ='center of gravity'
        cy = avg(idx[*,1])

        off = offset

        ;; dark correction

        Nwave = dim[3]/Npol
        wcgain = fltarr(Nx, Ny, Nwave, Npol)
        print,'dark correction...' 
        FOR n=0, Nwave-1 DO begin
            for i=0, Npol-1 do begin
                temp[index_in] = (reform(flat[*,*,n*Npol+i]) - dark)[index_in]
                temp[index_out] = 1.
                wcgain[*, *, n, i] = temp
            ENDFOR
        endfor
        ;memory
        flat=0

        ;wcgain = wcgain

        ;; Calculate "super-bad profile" on fine grid.
        ;; Interpolate in wavelength to shift the wavelength scale at
        ;; each valid pixel position back to a reference

        sbpprof = fltarr(nwscale, Npol)

        ;tmp = fltarr(Nx, Ny, nwscale, Npol)

        print,'interpolating for average flat profile over FOV...'
        for j=0, Npol-1 do begin
           print,'polstate:',j,' of',npol-1
 
          ctr=0
            FOR xx=(cx-50),cx+50 do begin
               FOR yy=(cy-50),cy+50 do begin
                  p = interpol(reform(wcgain[xx, yy, *,j]), wscale, $
                              wscale_equi+off[xx,yy], /quadratic)
            
                  sbpprof[*,j] += p
                  ctr++
               ENDFOR
            ENDFOR
            sbpprof[*,j] /= float(ctr)


        ENDFOR

        sbpprof_l = sbpprof
        sbpprof_r = sbpprof
        save, sbpprof_l, sbpprof_r, wscale_equi, wscale, info_flat_short, filename=fproffile,/ve

        ;; Create shifted array to "gain" the mean flat array.

        wcgain_tmp = 0. * wcgain
        wcgain_shift = 0. * wcgain

        print,'blueshifting mean profile...'
        for j=0, Npol-1 do begin
           print,'polstate:',j,' of',npol-1
             for i=0L, Num-1 do begin

                wscale_blue  = wscale - off[idxx[i], idxy[i]]
                wcgain_tmp[idxx[i], idxy[i], *,j] = INTERPOL(reform(sbpprof[*,j]), $
                             wscale_equi, wscale_blue, /quadratic)

            endfor
        endfor

        wcgain_shift = wcgain_tmp[idxlx,idxly,*,0] / wcgain_tmp[idxlx,idxly,*,0]

        ;; Calculate gain (1 / (divide flat by shifted sbpprof))

        wcgain_final = wcgain_shift / wcgain

     ENDIF ;SINGLE keyword

     If keyword_set(SMOOTH) then begin
stop
;LK: do not use smooth

        ;; and now the voodoo starts.
        ;; the voodoo is ment to remove residual solar variations present
        ;; in the gain. 

        print,'---------------------------------------------'
        print,'Ok, we smooth ....                           '
        print,'---------------------------------------------'

;number (3) as defined in template
        smooth_kernel = smooth

        wcgain_smooth = 0. * wcgain_final

        wscale_even = findgen(nwscale)/(nwscale-1) $ 
                               * (max(wscale) - min(wscale)) + min(wscale)
        
        for j=0, Npol-1 do begin
            for i=0L, Num-1 do begin
            
                tmp = reform(wcgain_final[idxx[i], idxy[i], *, j])
                tmp_new = INTERPOL(tmp, wscale, wscale_even, /quadratic)
                tmp_sm = smooth(tmp_new, smooth_kernel, /edge)
                tmp = INTERPOL(tmp_sm, wscale_even, wscale, /quadratic)
                wcgain_smooth[idxx[i], idxy[i], *, j] = tmp

            endfor
        endfor

        wcgain_final = wcgain_smooth

     endif ;end SMOOTH

     ;; now correct outside FOV

     FOR n=0, Nwave-1 DO begin
         for i = 0,Npol-1 do begin

             tmp = reform(wcgain_final[*,*,n,i])
             tmp[index_out] = 1.
             wcgain_final[*,*,n,i] = tmp

         endfor
     endfor   

  endif else begin ;keyword POL

;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectroscopic mode
;;---------------------------------------------------------
;;---------------------------------------------------------

     print,'---------------------------------------------'
     print,'Ok, we are in spectroscopic mode ....  '
     print,'---------------------------------------------'

     ;; Get the aperture for this field

     idx  = where(aps ge 1, Num)
     idxx = idx mod Nx
     idxy = idx / Nx
    
     cx = avg(idxx)  ;average ='center of gravity'
     cy = avg(idxy)

     Nwave = dim[3]
     wcgain = fltarr(Nx, Ny, Nwave)

     FOR n=0, Nwave-1 DO begin
         temp[idx] = (reform(flat[*,*,n]) - dark)[idx]
         temp[index_out] = 1.
         wcgain[*, *, n] = temp
     endfor

    off = offset

     ;; Calculate "super-bad profile" on fine grid!!!
     ;; Interpolate in wavelength to shift the wavelength scale at
     ;; each valid pixel position back to a reference

     sbpprof = fltarr(nwscale)
;     tmp = fltarr(Nx,Ny,nwscale)

;     FOR i=0L, Num-1 DO BEGIN

;         p = interpol(reform(wcgain[idxx[i], idxy[i], *]), wscale, wscale_equi+offset[idxx[i], idxy[i]], /quadratic)
;         tmp[idxx[i], idxy[i], *] = p
;         sbpprof[*] += p
;     ENDFOR
;     sbpprof /= float(Num)

;new june 2015
     ctr=0
            FOR xx=(cx-50),cx+50 do begin
               FOR yy=(cy-50),cy+50 do begin
                  p = interpol(reform(wcgain[xx, yy, *]), wscale, $
                              wscale_equi+off[xx,yy], /quadratic)
            
                  sbpprof[*] += p
                  ctr++
               ENDFOR
            ENDFOR
            sbpprof /= float(ctr)

            sbpprof_l = sbpprof
            sbpprof_r = sbpprof
            save, sbpprof_l, sbpprof_r, wscale_equi, wscale, info_flat_short, filename=fproffile,/ve

     ;; Create shifted array to "gain" the mean flat array.

     wcgain_shift = 0. * wcgain

     for i=0L, Num-1 do begin

         wscale_blue  = wscale - offset[idxx[i], idxy[i]]
         wcgain_shift[idxx[i], idxy[i], *] = INTERPOL(sbpprof, wscale_equi, wscale_blue, /quadratic)

     endfor

     ;; Calculate gain (1 / (devide flat by shifted sbpprof))

     wcgain_final = wcgain_shift / wcgain

     If keyword_set(SMOOTH) then begin

        smooth_kernel = smooth

        ;; and now the voodoo starts.
        ;; the voodoo is meant to remove residual solar variations present
        ;; in the gain. 

        wcgain_smooth = 0. * wcgain_final

        wscale_even = findgen(n_elements(wscale))/(n_elements(wscale)-1) $ 
                               * (max(wscale) - min(wscale)) + min(wscale)

        ;; smoothing process

        for i=0L, Num-1 do begin

            tmp = reform(wcgain_final[idxx[i], idxy[i], *])
            tmp_new = INTERPOL(tmp, wscale, wscale_even, /quadratic)
            tmp_sm = smooth(tmp_new, smooth_kernel, /edge)
            tmp = INTERPOL(tmp_sm, wscale_even, wscale, /quadratic)
            wcgain_smooth[idxx[i], idxy[i], *] = tmp

        endfor

        wcgain_final = wcgain_smooth

    endif

    ;; now correct outside FOV

    for i=0,dim[3]-1 do begin

        tmp = reform(wcgain_final[*,*,i])
        tmp[index_out] = 1.
        wcgain_final[*,*,i] = tmp

    endfor   

  endelse

  ;; Store the gain tables for each wavelength 

  wcgain = wcgain_final

      print,'-------------------------------------'
      print,'IBIS gain saved : ', savefile
      print,'-------------------------------------'

 
  save, FILENAME = savefile, wcgain, info_flat_short

;info_flat_short: correct stokes seq.,wl may not be ascending in structure but wcgain is
;issue: intensity variation of polarization states is divided out,
;i.e. I+Q_left - I-Q_left still shows I. Need to scale flats?

END
