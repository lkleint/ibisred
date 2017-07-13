FUNCTION shc, d1, d2, filter=filt, interpolate=int_max
;+
; NAME:
;       SHC
; PURPOSE:
;       Find the linear shift between two 2-d images using the
;       forier crosscorrelation method and optional interpolation for
;       sub-pixel shifts.
; CATEGORY:
;
; CALLING SEQUENCE:
;       RESULT = SHC ( IMG1, IMG2 )
; INPUTS:
;       IMG1,IMG2 : 2-d arrays of same size
; KEYWORDS:
;       FILTER : (Flag) if set and not zero, an edge filter is used
;                for the fft.
;   INTERPOLATE: (Flag) If set and not zero, interpolate the position
;                of the maximum of the cross correlation function to
;                sub-pixel accuracy.
; OUTPUTS:
;       RESULT : 2-element vector with the shift of IMG2 relative to
;                IMG1. RESULT(0) is shift in X-direction, RESULT(1) is
;                shift in Y-direction.
; PROCEDURE:
;       Compute the crosscorrelation function using FFT and locate the
;       maximum.
; MODIFICATION HISTORY:
;       13-Aug-1992  P.Suetterlin, KIS
;       29-Aug-1995  PS Added Edge filter
;       30-Aug-1995  PS Add Subpix interpolation. Slight rewrite of
;                    normal maximum finding (Use shift)
;       11-Sep-1995  PS: Forgot the 1-d case. Re-implemented.
;-

p1 = reform(d1-avg(d1)) &  s1=size(p1)
p2 = reform(d2-avg(d2)) &  s2=size(p2)
sx = s1(1) & sy=s1(2)

IF n_elements(p1) NE n_elements(p2) THEN $
  message, 'Arrays must have the same dimension'

IF (s1(0) GT 2) OR (s1(0) EQ 0) THEN $
  message, 'Only 1-d or 2-d Data!'

IF s1(0) EQ 1 THEN GOTO, onedim

IF keyword_set(filt) THEN BEGIN
    x = findgen(sx)
    x = exp(-(shift(x < (sx-x), sx/2)/(sx/2))^2)
    y = findgen(sy)
    y = exp(-(shift(y < (sy-y), sy/2)/(sy/2))^2)
    mm = x#y
    p1 = p1*mm
    p2 = p2*mm
ENDIF

cc = shift(abs(fft(fft(p1, -1)*conj(fft(p2, -1)), 1)), sx/2, sy/2)
mx = max(cc, loc)
 ;;; Simple Maximum location
ccsz = size (cc)
xmax = loc mod ccsz(1)
ymax = loc/ccsz(1)

IF NOT keyword_set(int_max) THEN GOTO, ende

if (xmax*ymax gt 0) and (xmax lt (ccsz(1)-1)) $
  and (ymax lt (ccsz(2)-1)) then BEGIN
      ;;; Sep 91 phw try including more points in interpolations
    denom = mx*2 - cc(xmax-1, ymax) - cc(xmax+1, ymax)
    xfra = (xmax-.5) + (mx-cc(xmax-1, ymax))/denom
    denom = mx*2 - cc(xmax, ymax-1) - cc(xmax, ymax+1)
    yfra = (ymax-.5) + (mx-cc(xmax, ymax-1))/denom
    xmax = xfra
    ymax = yfra
ENDIF

Ende:

return, [xmax-sx/2, ymax-sy/2]

Onedim:
IF keyword_set(filt) THEN BEGIN
    x = findgen(sx)
    x = exp(-(shift(x < (sx-x), sx/2)/(sx/2))^2)
    p1 = p1*x
    p2 = p2*x
ENDIF

cc = shift(abs(fft(fft(p1, -1)*conj(fft(p2, -1)), 1)), sx/2)
mx = max(cc, xmax)

IF keyword_set(int_max) THEN BEGIN
     ;;; Polyfit of degree 2 for three points, extremum
    c1 = (cc(xmax+1)-cc(xmax-1))/2.
    c2 = cc(xmax+1)-c1-cc(xmax)
    xmax = xmax-c1/c2/2
ENDIF
return, xmax-sx/2
END
