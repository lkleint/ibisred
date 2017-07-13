FUNCTION Time_conv_new, intime, f2s=f2s
;+
; NAME:
;	Time_conv
; PURPOSE:
;	Onvert a Time string HH:MM:SS to seconds or vice versa
; CALLING SEQUENCE:
;	Res = Time_conv( Time )
; INPUTS:
;	Time : Either integer value with number of seconds or string
;              with time of the day in format HH:MM:SS
; KEYWORDS:
;       F2S  : Default is convert string to integer, F2S reverts.
; OUTPUTS:
; 	Res : integer with number of seconds or string in format
;             HH:MM:SS, depending on Keyword F2S
; MODIFICATION HISTORY:
;	01-Apr-1991  P.Suetterlin, KIS
;-      june 2012: lk fixed msec display by removing fix()

on_error, 2

IF keyword_set(f2s) THEN BEGIN
    tim1 = '00:00:00.000'
    z = strtrim(fix(intime/3600), 2)
    IF strlen(z) EQ 1 THEN z = '0'+z
    strput, tim1, z
    z1 = strtrim(fix((intime-z*3600l)/60), 2)
    IF strlen(z1) EQ 1 THEN z1 = '0'+z1
    strput, tim1, z1, 3
    z2 = strtrim(intime-z*3600l-z1*60, 2)
    IF strlen(z2) EQ 1 THEN z2 = '0'+z2
    strput, tim1, z2, 6
ENDIF ELSE BEGIN
    tim1 = 3600d*double(strmid(intime, 0,2))+60d*double(strmid(intime, 3, 2))
    tim1 = tim1+double(strmid(intime, 6, 6))
ENDELSE

return, tim1

END
