FUNCTION read_ibis_wl_date_string, date_string
;lk modified apr 2012
  ;; read IBIS white light date string.

;@ibis_struct

  date = {year:2006,month:'',day:24,hour:1,min:1,sec:0.0,sid:0.0}
  months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  trim = strtrim(date_string, 2)

  date.day   = fix(strmid(trim, 8, 2))
  date.month = months[fix(strmid(trim, 5, 2))-1]
  date.year  = fix(strmid(trim, 0, 4))

  date.hour  = fix(strmid(trim, 11, 2))
  date.min   = fix(strmid(trim, 14, 2))
  date.sec   = float(strmid(trim, 17, 6))

  ;; Compute seconds into the day

  date.sid   = 60.0*(60.0*date.hour + date.min) + date.sec

  return, date
END
