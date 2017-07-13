;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;+
; NAME:
;       ibis_flat_avg_nb
;
; PURPOSE:
;      flat for new ibis camera (past aug 2010)
;      makes flat for all observed filters
; CATEGORY:
;
; CALLING SEQUENCE:
;       ibis_flat_avg,input_dir,output_dir
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       flat =    flat array where equal wavelengths and stokes states
;                 are averaged into one image
;
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; nmax not necessary since aborted series are found with file_search
; and thus correct.
;-### are nb flat split per polarization state?
; 31.5.13: modified reading of fits: fits_open instead of headfits
;          takes 2s/file instead of 34s
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PRO ibis_flat_avg_nb,input_dir,output_dir,ffdir,path1,overwrite=overwrite

;--------------  test if flats already exist --------------------  
  IF (FILE_TEST(output_dir+'*_flat.sav') and not keyword_set(overwrite)) THEN BEGIN
 
        PRINT,'----------------------------------'
        PRINT, 'Mean flat file(s) already exist, returning !! '
        PRINT,'----------------------------------'

  return
;---------------- main program ------------------------------------
  ENDIF ELSE BEGIN
  print,'entering ibis_flat_avg_nb'

   ;enable logging
   time=systime()
   newtext=strarr(3)
   newtext[0] = '\textbf{NB flat: ibis_flat_avg_nb.pro} \\'
   newtext[1] = string(time)+' \\'
   tstr=''
   for i=0,n_elements(ffdir)-1 do tstr=tstr+ffdir[i]+' ' ;strarray -> string
   newtext[2] = 'flatfield directories: \textit{'+tstr+'} \\ '
   ;----- check for underscores (need to be \_ in tex) ----
    for i=0,n_elements(newtext)-1 do begin
    res = strsplit(newtext[i],'_', count = n,/extr)

     if n gt 1 then begin
     newstring = res[0]
     for ll=1,n-1 do newstring = newstring+'\_'+res[ll]
     newtext[i] = newstring
     endif
    endfor
   log_write,path1,newtext

     nff = n_elements(ffdir) ;number of chosen flat directories
     ;file_search can only search one directory at a time
     subdirs = find_all_dir(input_dir) ;get all subdirectories
 
     openw,2,'flatdirs.txt' ;I didn't find a possibility to 
     ;save all paths to the flats in one variable considering that there 
     ;might be directories with a different number of files etc. 
     ;the simplest solution is to write a txt file and read it later.
 
     for i=0,nff-1 do begin ;for all flat directories
     currdir = subdirs[where(strpos(subdirs,ffdir[i]) ne -1)] ;find full path of given ffdir timestamp
     ffiles = file_search(currdir + '/*.FlatFieldCalibration.fits', COUNT = nd) ;find files
     for j=0,n_elements(ffiles)-1 do printf,2,ffiles[j] ;write in txt file
     endfor
     close,2

     ;now read the created txt file and make ffiles with correct paths to each file
     ln=file_lines('flatdirs.txt')
     ffiles = strarr(ln)
     openr,2,'flatdirs.txt'
     tmptxt = ''
     for i=0,ln-1 do begin
     readf,2,tmptxt
     ffiles[i] = tmptxt
     endfor 
     close,2 

     ;find number of extensions for each file
     print,'flats: reading headers for number of extensions per file'
     nffiles = n_elements(ffiles)
     ni = fltarr(nffiles)
     for i=0,nffiles-1 do ni[i] = readni_ncam(ffiles[i])
    
; save,ni,filename='ni.sav'
;restore,'ni.sav'

     ;read all headers, get sizes, filters and wl   
     ntotal = total(ni)
     wlobs = dblarr(ntotal)
     filterobs = fltarr(ntotal)
     stokesobs = strarr(ntotal)
     fname = strarr(ntotal)
     extno = fltarr(ntotal)


     l=0 ;counter
     print,'reading all headers for wl, filter, etc'
     for i=0,nffiles-1 do begin
       print,'file',i,' of',nffiles-1
       fits_open,ffiles[i],fcb ;new 31.5.13

       for j=1,ni[i] do begin
 ;      hdr = headfits(ffiles[i],ext=j)
       fits_read,fcb,data,hdr,/header_only,exten_no=j ;new 31.5.13
       wlobs[l] = sxpar(hdr,'WAVELNTH')
       filterobs[l] = sxpar(hdr,'FILTER')
       stokesobs[l] = sxpar(hdr,'STOKES')
       extno[l] = j
       fname[l] = ffiles[i]
       l=l+1
       endfor
       fits_close,fcb ;new 31.5.13
     endfor

     ;temporary, for faster execution
  ;   save,wlobs,filterobs,stokesobs,extno,fname,filename='tmp.sav'
  ;   restore,'tmp.sav',/ve

     ;make arrays with different filters etc (may not be necessary)
     nfilters = diffelement(filterobs)
     filters = diffelement(filterobs,/names)
     nwl = diffelement(wlobs)
     wl = diffelement(wlobs,/names,/double) ;is string!
     nstks = diffelement(stokesobs)
     stokes = diffelement(stokesobs,/names)

     sz = read_size(ffiles[0])
     possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters


;----------------------------------------------------------------------------------------
;------------------- make flat separately for each filter -------------------------------  
 FOR i=0,nfilters-1 do begin ;for each filter, make flat sequence
; print,i,' of',nfilters-1


     ind = where(filterobs eq filters[i])  ;current filter
     nind = n_elements(ind) ;images in current filter
  
     ;number of wavelengths for this filter
     nwlf = diffelement(wlobs[ind])
     wlf = diffelement(wlobs[ind],/names,/double) ;wl steps for this filter
     ;number of stokes states for this filter
     nstksf = diffelement(stokesobs[ind])
        ;   stokesf = diffelement(stokesobs[ind],/names)  ;while this works, it gives the
        ;   wrong modulation sequence
     stokesf = stokesobs[ind[0]:ind[0]+nstksf-1]   

     nb_flat = FLTARR(sz[0], sz[1], nwlf*nstksf)
     ;flat array will be averaged over all same wavelengths and stokes states  
     ;i.e. fflever, info_flat have more entries
     ;otherwise nb_flat may run out of memory

     ;find current wavelength to label output file correctly
     wlcurr = avg(wlobs[ind])
     a=  min(abs(possiblewl-wlcurr),pos)
     lambda = possiblewl[pos]

   
     PRINT,'---------------------------------------------------------'
     PRINT,'Flat files found: ', nffiles
     PRINT,'Current wavelength: ', lambda
     PRINT,'Total number of flat images: ', ntotal
     PRINT,'Number of flats to be used (for this filter):',nind
     print,'modulation sequence:',stokesf
     PRINT,'---------------------------------------------------------'

    ;--- log this in tex file
    ;find modulation sequence
    newtext=strarr(1)
    tstr=''
    for ss=0,n_elements(stokesf)-1 do tstr=tstr+stokesf[ss]+' ' ;strarray -> string
    newtext[0] = 'wavelength: '+string(lambda)+', total images: '+string(nind)+', modulation: \textit{'+tstr+'} \\'
 
    log_write,path1,newtext
    ;---- end logging

 
     fflevel = fltarr(nind)
     info_flat = ibis_create_struct(nind) ;full info, longer than nb_flat
     frms    = FLTARR(nind)
     info_flat_short = ibis_create_struct(nwlf*nstksf) ;here the stokes info and wl 
     ;corresponds do the actual image in nb_flat, but the times are not correct (since nb_flat is an average)
   


     ;summation over same wl and same stokes
     print,'all flats into one array...'
     l=0 ;counter for all files
     ctr=0 ;counter for flat array

    FOR aa=0,nwlf-1 do begin ;all wavelength steps in current filter
       FOR b=0,nstksf-1 do begin ;all stokes in current filter
         tmp = where(wlf[aa] eq wlobs and stokesf[b] eq stokesobs)
        ;tmp is index of certain wl per filter per stokes state
       if tmp[0] eq -1 then message,'something went wrong. headers? float/double problem? ibis_flat_avg_nb.pro'  

;this is actually a factor of 2 faster than fits_open because the
;fname is generally changing for each iteration
         FOR k=0,n_elements(tmp)-1 do begin ;tmp is >= 1
         data = readfits(fname[tmp[k]],ext=extno[tmp[k]],header)
         info_flat = ibis_info(header, info_flat, fname[tmp[k]], extno[tmp[k]], l)
         info_flat_short = ibis_info(header,info_flat_short,fname[tmp[k]], extno[tmp[k]], ctr)
         nb_flat[*,*,ctr] += FLOAT(data)  ;sum of all equal flats
         fflevel[l] = AVG(FLOAT(data))
         frms[l] = stdev(data)/avg(data)
         if l mod 100 eq 0 then print,'done:',l,' of',nind
         l=l+1
      ENDFOR

       nb_flat[*,*,ctr] = nb_flat[*,*,ctr]/k ;make average, ### possibly wrong if only 1 stokes state and 1 wl step observed
       ctr=ctr+1
       ENDFOR ;stokes
    ENDFOR ;wavelength step
   

  ;deleted exluding images part from program because that can be done in gui
         
     ;info_flat is constructed from same ext first (time not increasing)  
     stime = sort(info_flat.time)

     window,4,title='FOV averaged flat counts'
     plot, fflevel, psym=-4, xtitle = 'image number', ytitle = 'mean flat counts',/xs
          
     window,0,title='FOV averaged flat signal'
     plot, info_flat.time[stime]-info_flat.time[stime[0]], fflevel, psym=-4, xtitle = 'time   [sec]', ytitle = 'mean flat counts'
     window,1,title='RMS of flat signal'
     plot, info_flat.time[stime]-info_flat.time[stime[0]], frms, psym=-4, xtitle = 'time   [sec]', ytitle = 'rms'

     window,2,title='DST guider lightlevel'
     plot, info_flat.time[stime]-info_flat.time[stime[0]], info_flat.dst_llevel[stime], psym=-4, xtitle = 'time   [sec]', ytitle = 'DST guider light level'

 
  ;   PRINT,'----------------------------'
  ;   PRINT,'statistics mean nb flat :      '
  ;   PRINT,'----------------------------'
   ;  statist,flat ;takes forever

        filename = output_dir + string(lambda, FORMAT='(I4, "_flat.sav")')

         PRINT,'-------------------------------------'
         PRINT,'save nb flat: ',filename
         PRINT,'-------------------------------------'
 
        exposures = info_flat

        SAVE, nb_flat, fflevel, frms, exposures,info_flat_short, filename = filename,/ve
;big arrays but since ibis_gain_nb.pro does the averaging, save whole array
;saves _flat.sav

   ;logging (create eps image and add in .tex)
     files = file_search(path1+$
       '/log/nbflat'+string(lambda, FORMAT='(I4)')+'_*.eps',count=nfiles)
     avgflat = avg(nb_flat,2)

     set_plot,'PS'
     fflat = path1+'/log/nbflat'+string(lambda, FORMAT='(I4)')+$
            '_'+string(nfiles,format='(I1)')+'.eps'
    device,filename=fflat
    device,/encaps,bits_per_pixel=16,xsize=5,ysize=5
    tvscl,avgflat,xsize=5,/cent
    device,/close
    set_plot,'x'

    newtext=strarr(9)
    newtext[0] = 'wavelength: '+string(lambda)+', min: '+string(min(avgflat))+', max: '+$
                  string(max(avgflat))+', Fig.~\ref{nbf'+string(lambda,format='(I4)')+$
                  string(nfiles,format='(I02)')+'} \\'
    newtext[1] = '\begin{figure}[htb]'
    newtext[2] = '\begin{center}'
    newtext[3] = '\includegraphics[width=5cm]{nbflat'+string(lambda,format='(I4)')+$
                  '_'+string(nfiles,format='(I1)')+'}'
    newtext[4] = '\caption{avg NB flat, '+string(lambda,format='(I4)')+'}'
    newtext[5] = '\label{nbf'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
    newtext[6] = '\end{center}'
    newtext[7] = '\end{figure}'
    newtext[8] = ' '
    log_write,path1,newtext


 ENDFOR              ;end each filter


 ENDELSE ;end flat does not exist yet

END
