FUNCTION read_ibis_exposure_line, exposure_line

  ;; Fill IBIS exposure structure from line read from log file.

  exposure = create_struct('wavelength', 0, 'position', 0, $
                           'wavelength_offset', 0.0, 'time_string', "", $
                           'exposure_time', 0.0, $
                           'fp1_v1', 0, 'fp2_v2', 0, 'stokes', "")

;stop
  trim = strtrim(exposure_line, 2) ;remove leading and trailing blanks
print,exposure_line
;-- commented out lk because sometimes the time string does not have 3 decimals and all voltages and stokes params are wrong then---
;  exposure.wavelength          = fix(strmid(trim, 9, 4))
;  exposure.position            = fix(strmid(trim, 24, 1))
;  exposure.wavelength_offset   = float(strmid(trim, 52, 7))
;  exposure.time_string         = strmid(trim, 81, 24)

;  exposure.fp1_v1              = fix(strmid(trim, 117, 5))
;  exposure.fp2_v2              = fix(strmid(trim, 134, 5))
;  exposure.exposure_time       = float(strmid(trim, 158, 5))
;  exposure.stokes              = strtrim(strmid(trim, 176, 3), 2)

;new:
tmp = strsplit(trim,' ',/extract)
  exposure.wavelength          = fix(tmp[2])
  exposure.position            = fix(strmid(trim, 24, 1))
  exposure.wavelength_offset   = float(tmp[9])
  exposure.time_string         = tmp[14]+' '+tmp[15]

  exposure.fp1_v1              = fix(tmp[20])
  exposure.fp2_v2              = fix(tmp[25])
  exposure.exposure_time       = float(tmp[30])
  exposure.stokes              = tmp[34]


  return, exposure
END

