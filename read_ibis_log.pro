FUNCTION read_ibis_log, log_file, NMAX=Nmax,show=show

  ;; Read ibis log file and return array of exposure structures.

;return array 'exposures[m,n]' where m=number of frames and n=iterations
;lk: modified for 2007 log files which contained more empty lines

;the ibis log may have lines where the times are not in the format ss.xxx but end in ss. 
;The whole line is then shifted. The resulting values for FP1 voltage etc. are not correct. 
;Reason is that read_ibis_iter_line uses strmid with fixed values instead of a solution with spaces -> fixed

  exposure = create_struct('wavelength', 0, 'position', 0, $
                           'wavelength_offset', 0.0, 'time_string', "", 'exposure_time', 0.0, $
                           'fp1_v1', 0, 'fp2_v2', 0, 'stokes', "")

  line  = ''
  openr, lun, log_file, /GET_LUN

  readf, lun, line

;regular observations have the first line:
;'Start Data Acquisition - Selected Wavelengths  -  12/Jun/2010 18:30:07.905'
;autopolcal has:
;'Start Automatic Polarimetric Response Calibration'
;one might use read_vee_log for autopolcal but the resulting structure is completely different
;than the 'exposures' structure => test if autopolcal and read additional lines in that case
partline = strmid(line,0,15)
if partline eq 'Start Automatic' then autopol=1 else autopol=0

if autopol eq 1 then begin ;read useless lines of log
    readf, lun, line ;calibration optics settings
    readf, lun, line ;date
endif



  date_string = strmid(line, 23, 24, /REVERSE_OFFSET) ;not smart: the msec are variable in lengths
                                                      ;so far no crashes, but will crash if .000
  print, "Start Data Acquisition: ", date_string


  year = fix(strmid(strtrim(date_string,2),7,4)) ;strtrim removes blanks (occurs if msec are too short)
  readf, lun, line
  if year le 2007 then readf,lun,line ;there were empty lines in 2007
  substrings = strsplit(line, "-", COUNT=count, /EXTRACT)
  ;added for 2007 data
     while n_elements(substrings) eq 1 do begin
        readf,lun,line
        print,line
     endwhile
  substrings = strsplit(line, "-", COUNT=count, /EXTRACT)
  observe_type = strtrim(substrings[1], 2)
  print, observe_type

  FOR i=0, 1 DO readf, lun, line
  if year le 2007 then FOR i=0, 3 DO readf, lun, line
  iter = read_ibis_iter_line(line, Niter, Nframe)

  IF (keyword_set(NMAX)) THEN Niter = MIN([Niter, Nmax])
  exposures = replicate(exposure, Nframe, Niter)


  FOR n=0, Niter-1 DO BEGIN
      iter = read_ibis_iter_line(line, Niter, Nframe, date_string)

     if keyword_set(show) then  print, $
        FORMAT='("Iteration #", I3, "  started at ", A,' + $
        '" with ", I3, " frames")', n, date_string, Nframe
    

        FOR m=0, Nframe-1 DO BEGIN
           readf, lun, line
           if year le 2007 then readf, lun, line
           ;;      print, n, m
           exposures[m, n] = read_ibis_exposure_line(line)
        ENDFOR
      readf, lun, line
      if year le 2007 then readf, lun, line
  ENDFOR

  substrings = strsplit(line, "-", COUNT=count, /EXTRACT)
  print, "Observing ended at ", strtrim(substrings[2], 2) 

  free_lun, lun
  return, exposures
END
