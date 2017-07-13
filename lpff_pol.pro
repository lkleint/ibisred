PRO LPFF_pol, arr,pos
;+
; Procedure:         LPFF
;
; Purpose:           Measures the position of a spectral line (absorption
;                    or emission within the interval specified.
;
; Category:          Spectral Analysis
;
; Calling sequence:  LPFF,interval,pos
;
; Input:             arr: 1-dim array containing the line profile
;
; Output:            pos: position of line at subpixel accuracy,
;                         measured from left boundary of 'arr'
;                         [pos=1 means: line core is located at arr(1)]
;
; Restrictions:     The interval may be much  wider than the line 
;                   or contain only the line core. The routine works
;                   for odd and even numbers of elements of 'arr'.
;                   the line core intensity is NOT measured.
;
; Procedure:        The Fourier phase method is used. This method is 
;                   very insensitive to noise and is very fast. 
;-
; History:
; 1997-Jan-?? ws@kis: written
; 1998-???-?? ws@kis: updated.
; 2000-Jul-24 nlte@kis: documentation.
;
; 
; --------------------------------------------------------------------------
on_error,2

  sz=size(arr)
  length=sz(3)
  mid=sz(3)/2.
  tpi=2.*3.141592654
  dp=360./length                    ;Grad/pixel
  l1=double(arr)
  fl1=fft(l1,-1)
  lp=-atan(imaginary(fl1(1))/float(fl1(1)))/tpi*360.
  pos= lp/dp + mid
end
