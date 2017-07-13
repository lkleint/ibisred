PRO Xy, pos=p, count=n, norm=norm, device=dev
;+
; NAME:
;       XY
; PURPOSE:
;       Frontend for cursor: print marked position to screen
; CATEGORY:
;       
; CALLING SEQUENCE:
;       Xy [ , Keywords]
; INPUTS:
;       none
; KEYWORDS:
;       POS   : (output) In this variable the marked position(s) are
;               stored for further use.
;       COUNT : (input) Number of positions to mark.
;       NORM  : (Flag) Use normalized (i.e. from 0. to 1.) coordinates.
;       DEVICE: (Flag) Use device coordinates.
; OUTPUTS:
;       none
; SIDE EFFECTS:
;       Marked position is printed to screen
; PROCEDURE:
;       Call cursor, print result
; MODIFICATION HISTORY:
;       01-Aug-1993  P.Suetterlin, KIS
;-

on_error, 2

IF !D.name EQ 'PS' THEN message, 'No interactive cursor in ' + $
  'PostScript!'

If NOT keyword_set(n) THEN n = 1

p = fltarr(2, n)

FOR i = 0, n-1 DO BEGIN
    wait, 0.5
    IF keyword_set(norm) THEN $
      cursor, a, b, /norm $
    ELSE IF keyword_set(dev) THEN $
      cursor, a, b, /dev $
    ELSE $
      cursor, a, b
    p(*, i) = [a, b]
    print, 'x= '+strtrim(a, 1)+'   y= '+strtrim(b, 1)
ENDFOR
END

