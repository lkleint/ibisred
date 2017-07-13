;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;+
; NAME:
;       ibis_dark_avg_nb
;
; PURPOSE:
;   dark for new ibis camera (past aug 2010)
;   makes darks for all observed filters!
;   apr 12: added logging
; CATEGORY:
;
; CALLING SEQUENCE:
;       ibis_dark_avg_nb,input_dir,output_dir,[/overwrite]
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       dark = array with all darks per filter
;
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; ;lk taken from ibis_flat_dark_avg_nb.pro
; apr 2013: no longer save full dark arrays
;-
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_dark_avg_nb, input_dir, output_dir, path1, overwrite=overwrite
;--------------  test if darks already exist --------------------  
  IF (FILE_TEST(output_dir+'*_dark.sav') and not keyword_set(overwrite)) THEN BEGIN
 
        PRINT,'----------------------------------'
        PRINT, 'Mean dark file(s) already exist, returning !! '
        PRINT,'----------------------------------'
   
  return
  ENDIF ELSE BEGIN
;--------------  main program  ----------------------------------  


     tmp = find_all_dir(input_dir)
     ind = where(strpos(tmp,'DarkCalibration') ne -1)
     dfiles = file_search( tmp[ind] + '/*.DarkCalibration.fits', COUNT = nd) ;nd=1
     dfiles = diffelement(dfiles,/names) ;just in case there are double files
     ndfiles= n_elements(dfiles)
  
     ;ask user if all directories should be used for mean dark
     print,'available dark files:'
     print,dfiles
     PRINT, 'Enter files not used for average dark (def = use all, enter 0 1 2 3 ...)'
     SPAUSE, /nopause, ans = ans

     IF ans EQ '' THEN BEGIN
	PRINT,'Processing all'
        n = ndfiles ;1 file
        excluded = 0

     ENDIF ELSE BEGIN
           ans = fix(get_words(ans))
           excluded = n_elements(ans)
           n = ndfiles - n_elements(ans)
           index = ibis_good_frame_index(ndfiles,ans)
           dfiles = dfiles[index]    
     ENDELSE

;get number of extensions for each file
     ndfiles= n
     ni = fltarr(ndfiles)
     for i=0,ndfiles-1 do ni[i] = readni_ncam(dfiles[i])
    
;enable logging
   time=systime()
   newtext=strarr(3)
   newtext[0] = '\textbf{NB dark: ibis\_dark\_avg\_nb.pro} \\'
   newtext[1] = string(time)+' \\'
   newtext[2] = 'total dark files: '+string(ndfiles)+', excluded: '+string(excluded)+'\\'
   log_write,path1,newtext

;read all headers, get sizes, filters and wl   
   ntotal = total(ni)
   wlobs = dblarr(ntotal)
   filterobs = fltarr(ntotal)
   stokesobs = strarr(ntotal)
   fname = strarr(ntotal)
   extno = fltarr(ntotal)

l=0 ;counter
     print,'reading all headers for wl, filter, etc'
     for i=0,ndfiles-1 do begin
       print,'file',i,' of',ndfiles-1
       for j=1,ni[i] do begin
       hdr = headfits(dfiles[i],ext=j)
       wlobs[l] = sxpar(hdr,'WAVELNTH')
       filterobs[l] = sxpar(hdr,'FILTER')
       stokesobs[l] = sxpar(hdr,'STOKES')
       extno[l] = j
       fname[l] = dfiles[i]
       l=l+1
       endfor
     endfor

;split and do the following once per filter

     nfilters = diffelement(filterobs)
     filters = diffelement(filterobs,/names)
     sz = read_size(dfiles[0])
     possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters
 
;----------------------------------------------------------------------------------------
;------------------- make dark separately for each filter -------------------------------  

     FOR i=0,nfilters-1 do begin ;for each filter, make average dark

     ind = where(filterobs eq filters[i])  ;current filter
     nind = n_elements(ind)
 
     ;number of wavelengths for this filter
     nwlf = diffelement(wlobs[ind])
     wlf = diffelement(wlobs[ind],/names,/double) ;wl steps for this filter
     ;number of stokes states for this filter
     nstksf = diffelement(stokesobs[ind])
     ;dark probably doesn't depend on stokes state, but keep them to be safe
     ; stokesf =stokesobs[ind[0]:ind[0]+nstksf-1] ;mod 131001: this will fail if
                                ; stokes system changed during the different darks
     stokesf = diffelement(stokesobs[ind],/names)

     nb_dark = FLTARR(sz[0], sz[1], nwlf*nstksf)
  
     ;find current wavelength to label output file correctly
     wl = avg(wlobs[ind])
     a=  min(abs(possiblewl-wl),pos)
     lambda = possiblewl[pos]

   
     PRINT,'------------------------------------------------------'
     PRINT,'Dark files found: ', ndfiles
     PRINT,'Current wavelength: ', lambda
     PRINT,'Total number of dark images: ', ntotal
     PRINT,'Number of darks to be used (for this filter): ',nind
     PRINT,'------------------------------------------------------'

;log this in tex file
    newtext=strarr(1)
    newtext[0] = 'wavelength: '+string(lambda)+', total images: '+string(nind)+', '
    log_write,path1,newtext
 

     dlevel = FLTARR(nind)
     drms   = FLTARR(nind)   
     info_dark = ibis_create_struct(nind)
     info_dark_short = ibis_create_struct(nwlf*nstksf) ;here the stokes info and wl 
     ;corresponds do the actual image in nb_dark, but the times are not correct (since nb_dark is an average)
 
     print,'all darks into one array...'

  l=0 ;counter for all files
     ctr=0 ;counter for flat array

    FOR aa=0,nwlf-1 do begin ;all wavelength steps in current filter
       FOR b=0,nstksf-1 do begin ;all stokes in current filter
         tmp = where(wlf[aa] eq wlobs and stokesf[b] eq stokesobs)
        ;tmp is index of certain wl per filter per stokes state
       if tmp[0] eq -1 then message,'something went wrong. headers? float/double problem? ibis_dark_avg_nb.pro'  

         FOR k=0,n_elements(tmp)-1 do begin ;tmp is >= 1
         data = readfits(fname[tmp[k]],ext=extno[tmp[k]],header)
         info_dark = ibis_info(header, info_dark, fname[tmp[k]], extno[tmp[k]], l)
         info_dark_short = ibis_info(header,info_dark_short,fname[tmp[k]], extno[tmp[k]], ctr)
         nb_dark[*,*,ctr] += FLOAT(data)  ;sum of all equal darks
         dlevel[l] = AVG(FLOAT(data))
         drms[l] = stdev(data)/avg(data)
         if l mod 100 eq 0 then print,'done:',l,' of',nind
         l=l+1
      ENDFOR

       nb_dark[*,*,ctr] = nb_dark[*,*,ctr]/k ;make average, ### possibly wrong if only 1 stokes state and 1 wl step observed
       ctr=ctr+1
       ENDFOR ;stokes
    ENDFOR ;wavelength step
   

     ;darks are not necessarily ordered with time
     stime = sort(info_dark.time)



      window,0
      plot, dlevel, psym=-4, xtitle = 'image number', ytitle = 'mean dark counts',/xs
         
     window,1
     plot, info_dark.time[stime]-info_dark.time[stime[0]], dlevel, psym=-4,/xs,xtitle = 'time   [sec]', ytitle = 'mean dark counts'
     window,2
     plot, info_dark.time[stime]-info_dark.time[stime[0]], drms, psym=-4, xtitle = 'time   [sec]', ytitle = 'rms',/xs

   
   ;   nb_dark /= FLOAT(n) ;no division necessary since my nb_dark only
                                ;   has one image per index

  ;   PRINT,'----------------------------'
  ;   PRINT,'statistics mean dark :      '
  ;   PRINT,'----------------------------'

  ;   statist,nb_dark ;takes forever...

;logging (create eps image and add in .tex)
    files = file_search(path1+$
       '/log/nbdark'+string(lambda, FORMAT='(I4)')+'_*.eps',count=nfiles)
    avgdark = avg(nb_dark,2)

    set_plot,'PS'
    fdark = path1+'/log/nbdark'+string(lambda, FORMAT='(I4)')+$
            '_'+string(nfiles,format='(I1)')+'.eps'
    device,filename=fdark
    device,/encaps,bits_per_pixel=16,xsize=5,ysize=5
    tvscl,avgdark,xsize=5,/cent
    device,/close
    set_plot,'x'

    newtext=strarr(8)
    newtext[0] = 'min: '+string(min(avgdark))+', max: '+$
                  string(max(avgdark))+', Fig.~\ref{nbd'+string(lambda,format='(I4)')+$
                  string(nfiles,format='(I02)')+'} \\'
    newtext[1] = '\begin{figure}[htb]'
    newtext[2] = '\begin{center}'
    newtext[3] = '\includegraphics[width=5cm]{nbdark'+string(lambda,format='(I4)')+$
                  '_'+string(nfiles,format='(I1)')+'}'
    newtext[4] = '\caption{NB dark, '+string(lambda,format='(I4)')+'}'
    newtext[5] = '\label{nbd'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
    newtext[6] = '\end{center}'
    newtext[7] = '\end{figure}'
    log_write,path1,newtext
  
     filename = output_dir + string(lambda, FORMAT='(I4, "_dark.sav")')

         PRINT,'-------------------------------------'
         PRINT,'save nb dark: ',filename
         PRINT,'-------------------------------------'

dark_level = dlevel
dark_rms = drms
info_dark_nb = info_dark    

if (size(nb_dark))[3] lt 10 then print,'WARNING: less than 10 darks being averaged!'
dark = avg(nb_dark[*,*,1:*],2) ;; NEVER TAKE THE FIRST DARK!!!!

;        SAVE, nb_dark, dark_level, dark_rms, info_dark_nb, info_dark_short, FILENAME = filename,/ve
;nb_dark is [x,y,n darks] and is averaged later by ibis_calib.pro
;2013, save only average dark:
        SAVE, dark, dark_level, dark_rms, info_dark_nb, info_dark_short, FILENAME = filename,/ve
  

     PRINT,''

  ENDFOR ;going through filters

  newtext=' '
  log_write,path1,newtext ;add empty line in .tex

  ENDELSE
END
