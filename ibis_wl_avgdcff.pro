;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION mkdark, fn, llevel, flagncam=flagncam

;catch multiple dark files
if n_elements(fn) gt 1 then begin
   print,'Multiple dark timestamps found:'
   print,fn
   print,'Which one to use? [0 1 ...], def=0'
   read,ans
   if ans ge n_elements(fn) then ans=0
   fn=fn[ans]
endif

     if flagncam eq 1 then ni=readni_ncam(fn) else ni = readni(fn)
     sz = readpx(fn)
     dark = FLTARR(sz[0], sz[1])
     dfiles = file_search( fn, COUNT = nd)

     PRINT,'----------------------------'
     PRINT,'Dark files found: ', nd
     PRINT,'----------------------------'

     FOR k = 0, nd-1 DO BEGIN

         PRINT,'Dark file : ', dfiles[k]
         llevel = FLTARR(ni)

         FOR i = 1, ni DO BEGIN

                statist,FLOAT(READFITS(dfiles[k], EXT = i, /SILENT))
                llevel[i-1] = AVG(FLOAT(READFITS(dfiles[k], EXT = i, /SILENT)))

                dark += FLOAT(READFITS(dfiles[k], EXT = i, /SILENT))

         ENDFOR

         window,0
         plot, llevel, psym=-4, xtitle = 'image number', ytitle = 'mean dark counts',/xs

   	 REPEAT BEGIN

;PGJ change pause to spause
		SPAUSE,'Enter scans to reject (def = use all, enter without commas)',/nopause, ans = ans
		IF ans EQ '' THEN BEGIN

		   PRINT,'No bad scans.. Processing all'

                ENDIF ELSE BEGIN

	              bad_scans = get_words(ans)
		      nbad = N_ELEMENTS(bad_scans)
		      PRINT,'To reject ' + stringit(nbad) + ' scans.'

		      IF nbad GT 0 THEN PRINT, ' Scans to reject: ', stringit(bad_scans)

	              SPAUSE,'Is this correct? (y,n, def=y)', /nopause, ans=ans

		ENDELSE

	 ENDREP UNTIL (ans NE 'n')

        ;
        ; If there are no bad scans use the average data already calculated
        ; else repeat the averaging without the bad-scans
        ;

	IF (n_elements(bad_scans) GT 0) THEN BEGIN

	    dark = FLTARR(sz[0], sz[1])
            ndk = 0

            FOR i = 0, ni-1 DO BEGIN

		IF (in_set(bad_scans, i) EQ 0) THEN BEGIN

                   dark += FLOAT(READFITS(dfiles[k], EXT = i+1, /SILENT))
	           ndk = ndk + 1

		ENDIF

	    ENDFOR
            
            ni = ndk

	ENDIF

     ENDFOR     

     dark /= FLOAT(nd*ni)

     PRINT,'----------------------------'
     PRINT,'statistics mean dark :      '
     PRINT,'----------------------------'

     statist,dark

     RETURN, dark
END
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION mkflat, fn, dark, llevel, flevel, flagncam=flagncam

;stop
;catch multiple dark files
if n_elements(fn) gt 1 then begin
   print,'Multiple flat timestamps found:'
   print,fn
   print,'Which one to use? [0 1 ...], def=0'
   read,ans
   if ans ge n_elements(fn) then ans=0
   fn=fn[ans]
endif

     help, fn, dark, llevel, flevel
     if flagncam eq 1 then ni=readni_ncam(fn) else ni = readni(fn)
     sz = readpx(fn)
     flat = FLTARR(sz[0], sz[1])
     ffiles = file_search( fn, COUNT = nf)

     PRINT,'----------------------------'
     PRINT,'Flat files found : ', nf
     PRINT,'----------------------------'

     FOR k = 0, nf-1 DO BEGIN

         print,'Flat file : ', ffiles[k]
         llevel = fltarr(ni)
         flevel = fltarr(ni)

         FOR i = 1, ni DO BEGIN

             statist, FLOAT( readfits( ffiles[k], EXT = i, hdr, /SILENT)) - dark
             llevel[i-1] = SXPAR(hdr, 'DST_LLVL')
             flevel[i-1] = AVG(FLOAT( readfits( ffiles[k], EXT = i, hdr, /SILENT)) - dark)

             flat += FLOAT( readfits( ffiles[k], EXT = i, /SILENT)) - dark

         ENDFOR

         window,0
         plot, llevel, psym=-4, xtitle = 'image number', ytitle = 'mean light level', /xs
         oplot,flevel/mean(flevel) * mean(llevel), psym=-4, lines=1

   	 REPEAT BEGIN

		PRINT & SPAUSE,'Enter scans to reject (def = use all, enter without commas)',$
						/nopause, ans = ans
		IF ans EQ '' THEN BEGIN

		   PRINT,'No bad scans.. Processing all'

                ENDIF ELSE BEGIN

	              bad_scans = get_words(ans)
		      nbad = N_ELEMENTS(bad_scans)
		      PRINT,'To reject ' + stringit(nbad) + ' scans.'

		      IF nbad GT 0 THEN PRINT, ' Scans to reject: ', stringit(bad_scans)

	              SPAUSE,'Is this correct? (y,n, def=y)', /nopause, ans=ans

		ENDELSE

	 ENDREP UNTIL (ans NE 'n')

        ;
        ; If there are no bad scans use the average data already calculated
        ; else repeat the averaging without the bad-scans
        ;

	IF (n_elements(bad_scans) GT 0) THEN BEGIN

	    flat = FLTARR(sz[0], sz[1])
            nfk = 0

            FOR i = 0, ni-1 DO BEGIN

		IF (in_set(bad_scans, i) EQ 0) THEN BEGIN

                   flat = temporary(flat) + FLOAT(READFITS(ffiles[k], EXT = i+1, /SILENT))
	           nfk = nfk + 1

		ENDIF

	    ENDFOR
            
            ni = nfk

	ENDIF

     ENDFOR     

     flat /= FLOAT(ni*nf)

     PRINT,'----------------------------'
     PRINT,'statistics mean flat :      '
     PRINT,'----------------------------'

     statist,flat

     RETURN, flat
END
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Calculate mean dark and flat image for broadband channel. 
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_wl_avgdcff, ffile, dfile, offile, odfile,overwrite=overwrite,flagncam=flagncam
;changes LK: overwrite to make new flat and dark
;flagncam as flag if new camera was used (other filesystem)

;;***************************************************************
;; create mean dark if necessary
;;***************************************************************


     IF (FILE_TEST(odfile) and not keyword_set(overwrite)) THEN BEGIN
 
        PRINT,'----------------------------------'
        PRINT, 'Mean dark file already exists !! '
        PRINT,'----------------------------------'

        RESTORE, odfile, /verb

     ENDIF ELSE BEGIN

        ;catch if somebody forgot first button
          if dfile[0] eq '' then begin
             print,'Dark path is empty. Did you use the first button?'
             return
          endif

           dark = mkdark(dfile, dclevel, flagncam=flagncam)

           PRINT,'------------------------------'
           PRINT,'save dark: ', odfile
           PRINT,'------------------------------'

           SAVE, dark, dclevel, FILENAME = odfile

     ENDELSE

     PRINT,''

;;***************************************************************
;; create mean flat if necessary
;;***************************************************************

     IF (FILE_TEST(offile) and not keyword_set(overwrite)) THEN BEGIN

        PRINT,'-----------------------------------'
        PRINT, 'Mean flat file already exists  !! '
        PRINT,'-----------------------------------'

     ENDIF ELSE BEGIN

if n_elements(ffile) ne 1 then ffile=ffile[0]
           flat = mkflat(ffile, dark, llevel, fflevel, flagncam=flagncam)

           PRINT,'------------------------------'
           PRINT,'save flat: ', offile
           PRINT,'------------------------------'

           SAVE, flat, fflevel, llevel, FILENAME = offile
 
     ENDELSE

     PRINT,''

;;***************************************************************
;; Done!
;;***************************************************************

END
