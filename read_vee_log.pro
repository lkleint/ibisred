;-------------------------------------------------------------
;+
; NAME:
;       read_vee_log
; PURPOSE:
;       Read and parse log file written by VEE component of the 
;       IBIS Control System
; CATEGORY:
; CALLING SEQUENCE:
;       veelog = read_vee_log(log_file_name)
; INPUTS:
;       log_file_name = path and filename to input log file to parse
; KEYWORD PARAMETERS:    
;       log_format_version = the format version of the log file being
;                            read. The program will try to determine 
;                            the version from the log file itself, but
;                            this can be used to force the issue.
;                            Format Version Changes:
;                            1.0  - June 2003
;                            2.0  - 29 January, 2004
;        save_parsed_log   = determines whether structure containing parsed log file
;                            information is saved in an IDL save file. If save_parsed_log
;                            is a string variable, the output filename is set to that string.
;                            If save_parsed_log is instead any other non-zero value, the
;                            output file is named log_file_name+'parsed.sav'
;
;         record_count     = returns the total number of valid records found in the log file
;
;         series_count     = returns the total number of separate series found in the
;                            log file
;
; OUTPUTS:
;       veelog = structure containing details of individual images, 
;                    sequences, and series
; COMMON BLOCKS:
;       
; NOTES:
;       series_type = 0  -- Unknown
;       series_type = 1  -- Normal Observation
;       series_type = 2  -- Imaging Spectral Scan
;       series_type = 3  -- Dark/Bias Calibration
;       series_type = 4  -- Flat Field Calibration
;       series_type = 5  -- Other Calibration
;       series_type = 6  -- Testing
;
;  Some sample log file entries:
;
;  Start Data Acquisition - Selected Wavelengths  -  18/Feb/2004 16:03:01.413
;  Observation Type  - Science Observation - 18/Feb/2004 16:03:01.413
;  18/Feb/2004 16:03:01.443
;  18/Feb/2004 16:03:08.102
;  Begin Iteration  #1 of 200  -  Number of Frames : 43  -  18/Feb/2004 16:03:08.102
;  Filter : 7090 (Position 4)  | Relative Wavelength :  0.1815 | Acquisition Time : 18/Feb/2004 16:03:09.213 | FP1 V1 :    52 | FP2 V2 :   471 | ExpTime (sec) : 0.100
;  Filter : 7090 (Position 4)  | Relative Wavelength :  0.1295 | Acquisition Time : 18/Feb/2004 16:03:09.605 | FP1 V1 :    17 | FP2 V2 :   461 | ExpTime (sec) : 0.100
;  Filter : 7090 (Position 4)  | Relative Wavelength : -0.2433 | Acquisition Time : 18/Feb/2004 16:03:10.004 | FP1 V1 :  -234 | FP2 V2 :   389 | ExpTime (sec) : 0.100
;
;       It is called the "VEE Log" because it is the log file written by
;       the main IBIS control software, which is written in the VEE environment.
;
; MODIFICATION HISTORY:
;          Apr, 2003 - KPR - Initial Implementation
;       18 Nov, 2003 - KPR - updating for consistency with read_acq_log
;                            and agreed upon nomenclature
;       17 May, 2004 - KPR - added versioning information and support for
;                            exposure times and observation types written
;                            in file
;       28 Dec, 2004 - KPR - added possibility to explicity set output filename for
;                            saving parsed file; added record_count and series_count
;                            output keywords; added explicit finding of series end; 
;                            added "LCVR Calibration" series type
;-
;-------------------------------------------------------------
FUNCTION read_vee_log, log_file_vee, save_parsed_log=save_parsed_log, $
                       log_format_version=log_format_version, $
                       record_count=record_count, series_count=series_count

log_file_vee_fully_qualified = FILE_SEARCH(log_file_vee,/FULLY_QUALIFY_PATH)
vee_log_structure_version    = 2.0

IF NOT KEYWORD_SET(log_format_version) THEN log_format_version = 1.0

IF KEYWORD_SET(save_parsed_log) THEN BEGIN
   save_parsed_log_size = SIZE(save_parsed_log,/STRUCTURE)
   IF (save_parsed_log_size.type EQ 7) THEN BEGIN
       save_parsed_filename = save_parsed_log
   ENDIF ELSE BEGIN 
       save_parsed_filename = log_file_vee + '.parsed.sav'
   ENDELSE
ENDIF

input_row               = ''
output_text_length      = 0

; the different supported observing mode
obs_modes               = STRARR(9)
obs_modes(0)            = 'Unknown'
obs_modes(1)            = 'Science Observation' 
obs_modes(2)            = 'Imaging Spectral Scan'
; Modes 3, 4, 5 and 6 added in log format version 2,0 (Jan 2004)
obs_modes(3)            = 'Dark Calibration'
obs_modes(4)            = 'Flat Field Calibration'
obs_modes(5)            = 'Other Calibration'
obs_modes(6)            = 'Testing'
; Mode 7 added in Dec 2004
obs_modes(7)            = 'LCVR Polarimeter Calibration'
obs_modes(8)            = 'Automatic Pol Cal'

; the starting text for the different lines in the log file
observation_start       = 'Start Data Acquisition'
observation_end         = 'End Data Acquisition'
obs_mode_start          = 'Observation Type'
iteration_start         = 'Begin Iteration'
spectral_scan_start     = 'Begin Spectral Range Scan'
spectral_scan_end       = 'End Spectral Range Scan'
image_start             = 'Filter'
;iteration_start         = 'Iteration' ;in Feb 2010 there was no 'begin iteration'

; extract a common length string from each of the possible starting lines
; this will allow a simple comparison using a fixed length stringg taken from
; the beginning of each line of the log file (which should be much faster than 
; using STRMATCH
observation_start_len   = STRLEN(observation_start)
observation_end_len     = STRLEN(observation_end)
obs_mode_start_len      = STRLEN(obs_mode_start)
iteration_start_len     = STRLEN(iteration_start)
spectral_scan_start_len = STRLEN(spectral_scan_start)
spectral_scan_end_len   = STRLEN(spectral_scan_end)

min_string_len          = MIN([observation_start_len, observation_end_len, $
                               obs_mode_start_len, iteration_start_len, $
                               spectral_scan_start_len, spectral_scan_end_len])
observation_start_cut   = STRMID(observation_start,   0, min_string_len)   
observation_end_cut     = STRMID(observation_end,     0, min_string_len)   
obs_mode_start_cut      = STRMID(obs_mode_start,      0, min_string_len)   
iteration_start_cut     = STRMID(iteration_start,     0, min_string_len)            
spectral_scan_start_cut = STRMID(spectral_scan_start, 0, min_string_len)  
spectral_scan_end_cut   = STRMID(spectral_scan_end,   0, min_string_len)       

is_observation_start = 0
is_observation_end   = 0
is_iteration_start   = 0  
is_spectscan_start   = 0  
is_sequence_start    = 1 
is_series_end        = 0    
counter              = 0L
line_count           = 0L
series_num           = 0L
images_per_iteration = 0L
iteration_total      = 0L
iteration_num        = 0L

IF (FLOAT(STRMID(!VERSION.RELEASE,0,3)) GE 5.6) THEN BEGIN
    file_line_count     = FILE_LINES(log_file_vee)
ENDIF ELSE BEGIN
    spawn,'wc ' + log_file_vee, wc_output
    wc_output_split     = STRSPLIT(wc_output, /EXTRACT)
    file_line_count     = wc_output_split(0)
ENDELSE

filter                  = STRARR(file_line_count)
filter_wheelpos         = INTARR(file_line_count) - 1
wavelength              = DBLARR(file_line_count)
voltage_fp1             = INTARR(file_line_count)
voltage_fp2             = INTARR(file_line_count)
exposure_start_time     = DBLARR(file_line_count)
exposure_start_time_sec = DBLARR(file_line_count)
exposure_start_string   = STRARR(file_line_count)
series_id               = LONARR(file_line_count)
sequence_starts         = LONARR(file_line_count)
sequence_num            = INTARR(file_line_count)
series_start            = LONARR(file_line_count)
series_date_jd          = DBLARR(file_line_count)
series_date_string      = STRARR(file_line_count)
series_enddate_jd       = DBLARR(file_line_count)
series_enddate_string   = STRARR(file_line_count)
sequence_total          = INTARR(file_line_count)
images_per_seq          = INTARR(file_line_count)
series_type             = INTARR(file_line_count)
series_obs_mode         = STRARR(file_line_count)
series_obs_mode_num     = STRARR(file_line_count)
exposure_duration       = FLTARR(file_line_count)


OPENR,use_lun,log_file_vee, /GET_LUN

; make sure we're on a new line before starting program output

WHILE NOT (EOF(use_lun)) DO BEGIN
   READF,use_lun,input_row

   line_count = line_count + 1

   IF ((line_count MOD 100) EQ 0) THEN BEGIN
       numchar = '(I' + STRTRIM(STRLEN(STRTRIM(file_line_count,2)),2) + ')'
       output_text = 'Reading Line #' + STRING(line_count,FORMAT=numchar) + ' of ' + STRTRIM(file_line_count,2)
       ibis_overwrite_line, output_text_length, output_text, new_overwrite_length=output_text_length
   ENDIF
   
   line_start = STRMID(input_row, 0, min_string_len)    
;   stop
;***** Data Acquisition Series Start *****
   ; check for lines like: 
   ; Start Data Acquisition - Selected Wavelengths  -  18/Feb/2004 16:03:01.413
   IF (line_start EQ observation_start_cut)   THEN BEGIN
       is_observation_start = 1
       is_spectscan_start   = 0
       is_series_start      = 1
       is_sequence_start    = 1 
       iteration_num_fields = STRSPLIT(input_row, '-', /EXTRACT)
       series_start_string  = iteration_num_fields(2)
       series_start_jd      = ibis_log_date_convert(series_start_string)
       ;PRINT,'Observation Start - ' + series_start_string
   ENDIF ELSE $   
;***** Observation Mode Definition *****
   ; check for lines like: 
   ; Observation Type  - Science Observation - 18/Feb/2004 16:03:01.413
;stop
   IF (line_start EQ obs_mode_start_cut)      THEN BEGIN ;false for feb 2010 data?
       ; if there is an Observation Mode line, then the log must be at least version 2.0 (Jan 2004)
       log_format_version   = 2.0
       FOR obsmodenum = 0,N_ELEMENTS(obs_modes)-1 DO BEGIN
           IF STRMATCH(input_row,'*' + obs_modes(obsmodenum) + '*', /FOLD) THEN BEGIN
               series_obs_mode_temp     = obs_modes(obsmodenum)
               series_obs_mode_num_temp = obsmodenum
           ENDIF
       ENDFOR
   ENDIF ELSE $
;***** Single Iteration Start *****
   ; check for lines like: 
   ; Begin Iteration  #1 of 200  -  Number of Frames : 43  -  18/Feb/2004 16:03:08.102
   IF (line_start EQ iteration_start_cut)     THEN BEGIN
       is_sequence_start   = 1 
       interation_num_start = STRPOS(input_row, '#')
       interation_num_end   = STRPOS(input_row, '-')
       iteration_num_text   = STRMID(input_row, interation_num_start+1,interation_num_end - interation_num_start - 2)
       iteration_num_fields = STRSPLIT(iteration_num_text, /EXTRACT)
       iteration_num        = iteration_num_fields(0)
       iteration_total      = iteration_num_fields(2)
       images_per_iteration = STRMID(input_row,STRPOS(input_row,':')+2,4)
   ENDIF ELSE $
;***** Imaging Spectral Scan Start *****
   ; check for lines like: 
   ; Begin Spectral Range Scan - Wavelength 8542 - 107 Frames : Start Time - 18/Feb/2004 15:14:01   
   IF (line_start EQ spectral_scan_start_cut) THEN BEGIN
       is_spectscan_start   = 1
       is_observation_start = 0
       is_series_start      = 1
       is_sequence_start    = 1 
       iteration_num_fields = STRSPLIT(input_row, '-', /EXTRACT)
       images_per_iteration = FIX(STRMID(iteration_num_fields(2), 0, STRPOS(iteration_num_fields(2),'Frame')))
       series_start_string  = iteration_num_fields(3)
       series_start_jd      = ibis_log_date_convert(series_start_string)
       iteration_num        = 1
       iteration_total      = 1
   ENDIF ELSE $
;***** Series End *****
   IF (line_start EQ observation_end_cut) OR (line_start EQ spectral_scan_end_cut) THEN BEGIN
       is_series_end        = 1
       iteration_num_fields = STRSPLIT(input_row, '-', /EXTRACT)
       IF (line_start EQ observation_end_cut) THEN $
          series_end_string = STRTRIM(iteration_num_fields(2),2)
       IF (line_start EQ spectral_scan_end_cut) THEN $
          series_end_string = STRTRIM(iteration_num_fields(4),2)
       series_end_jd        = ibis_log_date_convert(series_end_string)       
       series_enddate_jd(current_series)        = series_end_jd
       series_enddate_string(current_series)    = series_end_string           
   ENDIF ELSE $

;******************************************
;***** Single Image Log Entry *****
;******************************************
;stop
   ; Look for lines like this (beginning with 'Filter' and sufficiently long):
   ; Filter : 7090 (Position 4)  | Relative Wavelength :  0.1815 | Acquisition Time : 18/Feb/2004 16:03:09.213 | FP1 V1 :    52 | FP2 V2 :   471 | ExpTime (sec) : 0.100
   IF (STRMID(input_row,0,6) EQ image_start) AND (STRLEN(input_row) GE 65) THEN BEGIN
 ;stop  
       IF (is_observation_start EQ 1) THEN BEGIN
           series_start_index            = counter
       ENDIF
       IF (is_sequence_start EQ 1)   THEN BEGIN
           sequence_id                       = counter
           images_per_seq(counter)           = images_per_iteration
           spect_series_id                   = -1
       ENDIF
       IF (is_spectscan_start EQ 1)   THEN BEGIN
          series_start_index                = counter
          sequence_id                       = counter
          images_per_seq(counter)           = images_per_iteration
       ENDIF
       
       IF (is_series_start EQ 1) THEN BEGIN
           current_series                    = series_num
           series_start(series_num)          = counter
           series_date_jd(series_num)        = series_start_jd
           series_date_string(series_num)    = series_start_string
          if iteration_total eq 0 then iteration_total=1 ;in case log does not have # 1 of 100 but only # 0
            sequence_total(series_num)        = iteration_total
           if images_per_iteration eq 0 then images_per_iteration=1 ;in case log does not have # 1 of 100 but only # 1
           images_per_seq(series_num)        = images_per_iteration
           IF log_format_version LE 1.9 THEN BEGIN
               ; before version 2 of the log, there was no explicit definition of the
               ; observation type, so we assign most things to either 'Science Observation'
               ; or 'Imaging Spectral Scan'
               IF (is_observation_start EQ 1) THEN series_type(series_num) = 1 ELSE $
               IF (is_spectscan_start EQ 1)   THEN series_type(series_num) = 2 ELSE $
                                                   series_type(series_num) = 0
               series_obs_mode_num(series_num)   = series_type(series_num)
               series_obs_mode(series_num)       = obs_modes(series_type(series_num))
           ENDIF ELSE BEGIN
               ; from version 2 and onward, there was the option to specify observing mode
               ; in the IBIS interface that can be included in the log
               series_type(series_num)           = series_obs_mode_num_temp
               series_obs_mode_num(series_num)   = series_obs_mode_num_temp
               series_obs_mode(series_num)       = series_obs_mode_temp
           ENDELSE
           series_num                     = series_num + 1
       ENDIF

       ; reset all series/sequence markers -- if it is a single image these are no longer valid
       is_observation_start = 0
       is_sequence_start    = 0  
       is_spectscan_start   = 0  
       is_series_start      = 0  
       is_series_end        = 0  
       
       series_id(counter)            = series_start_index
       sequence_starts(counter)      = sequence_id
       sequence_num(counter)         = iteration_num

       ; fields in individual image lines are separated by '|'
       ; Filter : 7090 (Position 4)  | Relative Wavelength :  0.1815 | Acquisition Time : 18/Feb/2004 16:03:09.213 | FP1 V1 :    52 | FP2 V2 :   471 | ExpTime (sec) : 0.100       
       input_fields   = STRSPLIT(input_row,'|', /EXTRACT)
       
       filter_fields  = STRSPLIT(input_fields(0), /EXTRACT)
       filter(counter) = filter_fields(2)
       filter_wheelpos(counter) = STRMID(filter_fields(4),0,1)

       wavelengths    = STRSPLIT(input_fields(1),':', /EXTRACT)
       wavelength(counter) = wavelengths(1)

       time_string_start   = STRPOS(input_fields(2), ':')
       exposure_start_string(counter) = STRTRIM(STRMID(input_fields(2), time_string_start + 2), 2)

       time_fields    = STRSPLIT(input_fields(2), /EXTRACT)
       date_fields    = STRSPLIT(time_fields(3),'/',/EXTRACT)
       ; Month name to number conversion adapted from DATE2YMD routine from JHU/APL IDL Library
       ; probably written before STRMATCH was added to IDL
       textmonth      = STRUPCASE(date_fields(1))
       monthpos       = STRPOS('JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',textmonth)
       IF (monthpos GE 0) THEN monthnum       = 1 + monthpos/3. ELSE monthnum = 0

       obstime        = STRMID(time_fields(4),0,12)
       obstime_fields = STRSPLIT(obstime,':',/EXTRACT)
       obstime_sec    =   (obstime_fields(0) * 60L +  obstime_fields(1)) * 60. + $
                           DOUBLE(obstime_fields(2))
       julian_day     = JULDAY(monthnum, date_fields(0), date_fields(2), $
                               obstime_fields(0), obstime_fields(1), DOUBLE(obstime_fields(2)))

       exposure_start_time_sec(counter)      = obstime_sec
       exposure_start_time(counter)          = julian_day

       voltages_fp1   = STRSPLIT(input_fields(3),':', /EXTRACT)
       voltage_fp1(counter)    = LONG(voltages_fp1(1))

       voltages_fp2   = STRSPLIT(input_fields(4),':', /EXTRACT)
       voltage_fp2(counter)    = LONG(voltages_fp2(1))

       IF (N_ELEMENTS(input_fields) GE 6) THEN BEGIN
           exptime    = STRSPLIT(input_fields(5),':', /EXTRACT)
           exposure_duration(counter)    = FLOAT(STRTRIM(exptime(1),2))
       ENDIF

       ;vee_log_file_all    = [ vee_log_file_all, vee_log_file_temp ]
       ;vee_log_file_all(counter)    = vee_log_file_temp

       counter = counter + 1

   ENDIF

ENDWHILE


; Print the final line number that was read - should be the same as the number of lines in the file
numchar = '(I' + STRTRIM(STRLEN(STRTRIM(file_line_count,2)),2) + ')'
output_text = 'Reading Line #' + STRING(line_count,FORMAT=numchar) + ' of ' + STRTRIM(file_line_count,2)
ibis_overwrite_line, output_text_length, output_text, new_overwrite_length=output_text_length

PRINT

FREE_LUN,use_lun

vee_log_file_all = CREATE_STRUCT( $
                               'filter',                  filter(0:counter-1),                $
                               'filter_wheelpos',         filter_wheelpos(0:counter-1),       $
                               'wavelength',              wavelength(0:counter-1),            $
                               'voltage_fp1',             voltage_fp1(0:counter-1),           $
                               'voltage_fp2',             voltage_fp2(0:counter-1),           $
                               'exposure_duration',       exposure_duration(0:counter-1),     $
                               'exposure_start_time',     exposure_start_time(0:counter-1),   $
                               'exposure_start_time_sec', exposure_start_time_sec(0:counter-1), $
                               'exposure_start_string',   exposure_start_string(0:counter-1), $
                               'series_id',               series_id(0:counter-1),             $
                               'sequence_starts',         sequence_starts(0:counter-1),       $
                               'sequence_num',            sequence_num(0:counter-1),          $
                               'series_start',            series_start(0:series_num-1),       $
                               'series_date_jd',          series_date_jd(0:series_num-1),     $
                               'series_date_string',      series_date_string(0:series_num-1), $
                               'series_enddate_jd',       series_enddate_jd(0:series_num-1),  $
                               'series_enddate_string',   series_enddate_string(0:series_num-1), $
                               'sequence_total',          sequence_total(0:series_num-1),     $
                               'images_per_seq',          images_per_seq(0:series_num-1),     $
                               'series_type',             series_type(0:series_num-1),        $
                               'series_obs_mode',         series_obs_mode(0:series_num-1),    $
                               'series_obs_mode_num',     series_obs_mode_num(0:series_num-1),$
                               'log_struct_version',      vee_log_structure_version,          $
                               'log_type',                'VEE',                              $
                               'log_file_name',           log_file_vee_fully_qualified        $
                   )

IF KEYWORD_SET(save_parsed_log) THEN $
    SAVE,vee_log_file_all,log_file_vee,FILENAME=save_parsed_filename

record_count = counter
series_count = N_ELEMENTS(vee_log_file_all.series_start)
PRINT,'Found ' + STRTRIM(counter,2) + ' Records in ' + STRTRIM(series_count,2) + ' Series in VEE Log File.'

RETURN,vee_log_file_all

END

