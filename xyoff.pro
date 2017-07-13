;-------------------------------------------------------------
;+
; NAME:
;      XYOFF
; PURPOSE:
;      This function, given two images, calculates the 
;      correlation function between those two images
;      using a box of size (bx,by) surrounding a point in the image.
;      By default this point is the center of the image,
;      (nx/2,ny/2).  The routine calculates the offsets by finding the 
;      maximum of the correlation fucntion.
; CATEGORY:
;      IMAGE PROCESSING
; CALLING SEQUENCE:
;      s = xyoff(a,b,bx,by, [wdow,mask,hx,hy,fna])
; INPUTS:
;      a = The reference image.
;        type: array,any type,arr(nx,ny)
;      b = The image that you want to track with 
;        respect to a.
;        type: array,any type,arr(nx,ny)
;      bx = The size of the box in the x-dimension that you wish
;        to use to generate the cross correlation function.
;        type: scalar,integer
;      by = The size of the box in the y-dimension that you wish
;        to use to generate the cross correlation function.
;        type: scalar,integer
;      wdow = A 2-d apodization window.  (Optional)
;        type: array,any type,arr(nx,ny)
;      mask = A 2-d mask to be applied in fourier space. (Optional)
;        type: array,any type,arr(nx,ny)
;      hx = The x position in the image that corresponds to the center 
;        of the box. (Optional)
;        type: scalar,integer
;      hy = The y position in the image that corresponds to the center 
;        of the box. (Optional)
;        type: scalar,integer
;      fna = The fourier transform of the 256x256  (Optional)
;        subtended image from the center of a.
;        type: array,floating point,fltarr(nx,ny)
; KEYWORD PARAMETERS:
;      /SOBEL = If specified, then SOBEL keyword causes this routine
;        to track on the gradient of the image instead of the 
;        image itself.  (It uses a Sobel edge enhancement opetrator)
; OUTPUTS:
;      s = A vector containing the x and y offsets that 
;        should be applied to an image to maximize
;        the cross correlation.
;        type: vector,floating point,fltarr(2)
;      s(0) = The x offset.
;        type: scalar,floating point
;      s(1) = The y offset.
;        type: scalar,floating point
; COMMON BLOCKS:
; NOTES:
;      The optional input parameters allow the user to 
;      speed up long jobs that, for instance, use
;      the same reference. Simply add the 3 variable names
;      hx, hy and fna to the calling of xyoff.pro in your driver
;      routine. The first time it is called, those 3 variables
;      won't be defined...so, xyoff will define them,
;      the next time, xyoff, will see that they are defined
;      and will not define them again, therefore removing
;      that part of the algorithm and speeding up the routine.
;      (If you don't want either a mask or a window, simply
;      define them as 1.)
;      
;      If you wan't to change the reference some time
;      later you must remove those 3 variables from the
;      calling of xyoff.pro or make sure that the 3 
;      variables are undefined.
; MODIFICATION HISTORY:
;      H. Cohl,  25 Mar, 1992 --- Made documentation more understandable.
;      H. Cohl,  13 Feb, 1992 --- Made it so that you can enter
;                                 in hx or hy.
;      H. Cohl,  13 Dec, 1991 --- Added SOBEL keyword.
;      H. Cohl,   7 Jun, 1991 --- Initial programming.
;-
;-------------------------------------------------------------

function xyoff,a,b,bx,by,wdow,mask,hx,hy,fna,$
               sobel=sobel,help=help

  ;Display idl header if help is required.
  if keyword_set(help) or n_params() lt 4 then begin
    get_idlhdr,'xyoff.pro'
    s=-1
    goto,finishup
  endif

  ;Set correlation box size.
  mx=bx/2 & my=by/2

  ;Test to see if window is specified.
  if n_elements(wdow) eq 0 then wdow=1.
  if n_elements(mask) eq 0 then mask=1.
  
  ;Calculate size of image.
  ssz=size(a)
  
  ;Determine whether hx exists if not, then define it.
  if n_elements(hx) eq 0 then begin
    nx=ssz(1) 
    hx=nx/2
  endif 
  
  ;Determine whether hy exists if not, then define it.
  if n_elements(hy) eq 0 then begin
    ny=ssz(2) 
    hy=ny/2 
  endif

  ;Speedup section.
  if n_elements(fna) eq 0 then begin
    na=a(hx-mx:hx+mx-1,hy-my:hy+my-1)
    if n_elements(sobel) eq 0 then na=(na-mean(na))*wdow $
      else na=sobel(na-mean(na))
    fna=fft(na*mask,-1)
  endif

  ;Subset correltion box around the center of image.
  nb=b(hx-mx:hx+mx-1,hy-my:hy+my-1)
  if n_elements(sobel) eq 0 then nb=(nb-mean(nb))*wdow $
    else nb=sobel(nb-mean(nb))

  ;Compute the fourier transform of nb. 
  fnb=fft(nb*mask,-1)

  ;Compute the correlation function between na and nb.
  ccf=fft(fna*conj(fnb),1)

  ;Rename the cross correlation function as it's real part.
  ccf=float(ccf)

  ;Shift correlation function.
  ccf=shift(ccf,mx,my)

  ;Find maximum of correlation function.
  ps=max_pos(ccf)

  ;Calculate shifts.
  s=fltarr(2)
  s(0)=ps(0)-mx
  s(1)=ps(1)-my

  print,'   Shift in X - ',s(0)
  print,'   Shift in Y - ',s(1)

  finishup:

return,s

end
