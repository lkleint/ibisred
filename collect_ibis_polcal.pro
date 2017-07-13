;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;;Collect IBIS flats for the different lines. Store entire array.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO collect_ibis_polcal, input_dir, output_dir, polardir, NMAX = nmax,exclude=exclude,$
   auto=auto,flagncam=flagncam
;lk: added polardir keyword for different directory structure
;lk: added exclude to exclude certain sequences (due to clouds for example)
;     exclude=[15,22] will need nmax=30 and Seq. 16 is saved as wl_pol015.sav
;    keyword auto: for automatic polcal, polardir must contain 28 timestamps
;    'exclude' not compatible with 'auto' (because observers need to restart anyway if
;    something goes wrong).
;oct 11: added flagncam
;        ### todo: telparams from header into structure and replace
;        nb_expos

xpath = input_dir ;because input_dir is changed later

if not keyword_set(exclude) then exclude=1000 ;some number, higher than # scans
  ;; Collect IBIS scans for the different lines. Store each scan seperately.
  ;; Do not add white-light images.

;observer's log starts numbering with sequence 1: clear, sequence 2: dark etc.
;idl starts with the index 0. i.e. if I want to exclude sequence 15, the idl 
; index will be 14. Therefore:
exclude = exclude -1


  UADD = 2L^15 ;not necessary
  exclindex = 0 ;index for excluded sequences (such that filenames are correct, see below)
  nn = 0 ;numeration so that filename is correct for auto sequence
  no = 0


 if keyword_set(auto) then ndirs=28 else ndirs=1
 FOR idir = 0, ndirs-1 DO BEGIN 


 ;; First read the log file for this directory and fill the
  ;; exposures structure.
;if idir eq 1 then stop
  ; cmd='find -L ' + xpath + ' -wholename "*'+ 'log_'+ polardir[idir] + '*" -name "*.txt" ' ; 
  ;  spawn, cmd, log_file

    log_file = file_search(polardir[idir]+'/log*txt',count=nfiles)
    if nfiles ne 1 then message,'Problem finding logfile, check that polardir has path included'

 ; log_file  = $
 ; input_dir + PATH_SEP() + "log_" + file_basename(input_dir) + ".txt"
  print,'Logfile : ', log_file
  IF KEYWORD_SET(NMAX) THEN BEGIN
;;     exposures = read_ibis_log_new(log_file, NMAX=nmax) 
     exposures = read_ibis_log(log_file, NMAX=nmax) 

  endif

  ;; Find dimensions

  dim = size(exposures)
  Nframe = dim[1]
  IF (dim[0] EQ 1) THEN  Niter = 1 ELSE Niter = dim[2]
 
  input_dir =  strmid(log_file,0,strpos(log_file,'log')) ;lk changed for old directory structure
 
if flagncam ne 1 then begin ;new cam has different file structure
  if (file_test(input_dir + PATH_SEP() + '1/' + '0000.s000.i00.fits')) then begin
    dummy = $
        readfits_ibis(input_dir + PATH_SEP() + '1/' + '0000.s000.i00.fits', header, /noscale,/silent)
    fflag = 0
  endif else begin 
    dummy = $
        readfits_ibis(input_dir + PATH_SEP() + '0000.s000.i00.fits', header, /noscale,/silent)
    fflag = 1
  endelse
endif else begin
 fls = file_search(input_dir+'/*.fits') ;should have only one file
 dummy = readfits(fls[0],ext=1,header,/silent)
endelse  

  Nx = sxpar(header, 'NAXIS1')
  Ny = sxpar(header, 'NAXIS2')

  ;; Find the different spectral lines by looking for unique
  ;; wavelengths.

  spectra = unique_elements(exposures.wavelength, COUNT=Nspectra)
  

  ;; Now collect the scans. Outer loop is over number of scans, second
  ;; loop is over different lines, innermost loop is over wavelength
  ;; offsets. Store file seperately for each scan and spectral line.
  ;; also find the corresponding whitelight images and store them. 

  FOR n=0, Niter-1 DO BEGIN ;28 or 29 sequences  ;=0 for autopolcal
    FOR k=0, Nspectra-1 DO BEGIN ;number of observed wl
      index   = where(exposures[*, n].wavelength EQ spectra[k], count)
      nb_data = fltarr(Nx, Ny, count)
      info_polcal = ibis_create_struct(count) ;full info

       if flagncam eq 1 then begin
       fits_open,fls[0],hdr
       endif  
 
    FOR m=0, count-1 DO BEGIN ;54 wl points
 
      if flagncam ne 1 then begin ;old cam
        i23str = (index[m] LT 100) ? "I2.2" : "I3"
        scanno = LONG(n)*LONG(Nframe) + LONG(index[m])
        i45str = (scanno LT 10000) ? "I4.4" : "I5"

        if (fflag eq 0) then begin
        ; deal with insanity
        if (scanno lt 5000) then subdir = '1' + path_sep() $
        else if (scanno lt 10000) then subdir = '2' + path_sep() $
        else if (scanno lt 15000) then subdir = '3' + path_sep() $
        else if (scanno lt 20000) then subdir = '4' + path_sep() $
        else if (scanno lt 25000) then subdir = '5' + path_sep() $
        else if (scanno lt 30000) then subdir = '6' + path_sep() $
        else if (scanno lt 35000) then subdir = '7' + path_sep() $
        else if (scanno lt 40000) then subdir = '8' + path_sep() $
        else if (scanno lt 45000) then subdir = '9' + path_sep() $
        else if (scanno lt 50000) then subdir = '10' + path_sep()

        filename = input_dir + PATH_SEP() + subdir + $
                   string(scanno, n, index[m], $ 
                          FORMAT='(' + i45str + ', ".s", I3.3, ".", "i",' + i23str + ', ".fits")')
        endif else begin
          filename = input_dir + PATH_SEP() + $
                   string(scanno, n, index[m], $ 
                          FORMAT='(' + i45str + ', ".s", I3.3, ".", "i",' + i23str + ', ".fits")')
        endelse

        nb_data[*, *, m] = readfits_ibis(filename, header, /noscale,/silent)
     endif else begin ;new cam
 
        fits_read,hdr,data,header,exten_no=index[m]+1
        nb_data[0,0,m] = data
;        nb_data[*, *, m] = readfits(fls[0], ext = index[m]+1, header,/silent)
        info_polcal =  ibis_info(header,info_polcal,fls[0], index[m]+1, m) 
     endelse

      ENDFOR ;end wl points, m

      if flagncam eq 1 then begin
       fits_close,hdr
      endif

      nb_expos = exposures[index, n] ;dimensions are: number of observations (=6 stokes * 6 wl steps)


      ;; Save narrow-band and white-light data together, including
      ;; exposure times and other exposure data.

     if flagncam eq 1 then  filename = output_dir +  PATH_SEP() + $  ;new cam
                 string(spectra[k], nn-exclindex, FORMAT='(I4, "_pol", I3.3, ".sav")') else $
                             filename = output_dir +  PATH_SEP() + $  ;old cam
                 string(spectra[k], n-exclindex, FORMAT='(I4, "_pol", I3.3, ".sav")') 



      save, FILENAME=filename, nb_data, nb_expos, info_polcal
      print,'Saved: ',filename
 
   ENDFOR ;end wl
     ;if there are sequences to exclude:
       if total(nn eq exclude) eq 1 then exclindex = exclindex + 1
    ;possible failure if last seq repeated many times? 

     nn = nn+1

              if nn eq 28 then begin
                 nn = 0
                 no = no+1
              endif


  ENDFOR ;end sequence


endfor                           ;end auto
;;***************************************************************
;; Done
;;***************************************************************

END


