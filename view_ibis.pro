;lk 12.5.11
;June 11,13: added irange
;
;purpose: input stokesout [x,y,lambda,4] and display stokes profiles
;including division to get Q/I,U/I,V/I
; keywords: lambda = vector of lambda for x axis
;           imgind = index of image to display, default = middle one
;           nofrac = do not calculate fractional polarization states
;           equal = equal scaling for plots, i.e. +- max value of pol. state
;           xl = xline at certain wavelength (for example at line center)
;           vv =  show v instead of i
;           irange,qrange,urange,vrange = [low,high]

;right click closes the windows, left click creates ps file with current spectra
;warning: if the image is larger than your screen size, the
;coordinates will be off!

pro view_ibis,data,lambda=lambda,imgind=imgind,nofrac=nofrac,equal=equal,$
              xl=xl,vv=vv,irange=irange,qrange=qrange,urange=urange,vrange=vrange

scrsz=get_screen_size()
xs=scrsz(0) & ys=scrsz(1)

;----------------- variables ---------------------------
 stokes = data ;avoid pass by reference
 dimension = size(stokes)
  IF (dimension[0] NE 4) THEN BEGIN
    print, "VIEW_IBIS: data needs to be 4-D spectral data cube"
    print, 'USAGE: view_ibis,data,lambda=lambda,imgind=imgind,nofrac=nofrac,equal=equal,xl=xl,vv=vv'
    return
  ENDIF

  while (dimension[1] ge xs || dimension[2] ge ys) do begin
  if dimension[1]/2 ne dimension[1]/2. then message,'Problem: Image too large and cannot rebin'
  if dimension[2]/2 ne dimension[2]/2. then message,'Problem: Image too large and cannot rebin'
  stokes = rebin(stokes,dimension[1]/2,dimension[2]/2,dimension[3],4)
  dimension = size(stokes)
  print,'data was binned to fit on screen (each of these messages is a factor of 2 binning)'
  endwhile

  Nx = dimension[1]
  Ny = dimension[2]
  Nlambda = dimension[3]
  dmax = max(stokes, MIN=dmin)
  TRUE = 1
  WAIT = 2
  LEFT_BUTTON   = 1
  MIDDLE_BUTTON = 2
  RIGHT_BUTTON  = 4


;--- make fractional polarization states -----------
if not keyword_set(nofrac) then begin
  stokes[*,*,*,3] = stokes[*,*,*,3]/stokes[*,*,*,0]
  stokes[*,*,*,1] = stokes[*,*,*,1]/stokes[*,*,*,0]
  stokes[*,*,*,2] = stokes[*,*,*,2]/stokes[*,*,*,0]
  ytitleq = 'Q/I'
  ytitleu = 'U/I'
  ytitlev = 'V/I'
endif else begin
  ytitleq = 'Q'
  ytitleu = 'U'
  ytitlev = 'V'
endelse

 window, /FREE,xsize=nx,ysize=ny
 originalWindow = !D.WINDOW

;------- display reference image ------------
if not keyword_set(imgind) then imgind = nlambda/2
if not keyword_set(vv) then tvscl, stokes[*, *, imgind] else tvscl, stokes[*, *, imgind,3]


;------ set up plotting for profiles -----------
 
  IF (keyword_set(LAMBDA)) THEN $
   xtitle = 'Wavelength' $
  ELSE BEGIN
    xtitle = 'Wavelength index'
    lambda = findgen(Nlambda)
  ENDELSE


   ytitlei = 'I profile'


  window, TITLE='Spectral profile', /FREE
  profileWindow = !D.WINDOW

  Nspec = 1 ;only one spectrum to show
  nshow = 1
  WHILE TRUE DO BEGIN
    wset, originalWindow
    cursor, i, j, wait, /DEVICE, DOWN=down

   ;------------  right click closes spectra
    IF (!MOUSE.BUTTON EQ RIGHT_BUTTON) THEN BEGIN
      wdelete, profileWindow
      wdelete, originalwindow
      return
    ENDIF

;------------  left click makes eps file -----------------------------------
    IF (!MOUSE.BUTTON EQ LEFT_BUTTON) THEN BEGIN
     print,'created view_ibis.eps'
     set_plot,'PS'
     !p.charsize=0.7
     xpage = 18 & ypage = 10
     device,filename='view_ibis.eps',xoffset=0,yoffset=0,/encaps,$
            xsize=xpage,ysize=ypage,/color,bits_per_pixel=16
                corrfact = ypage/xpage ;korrrekturfaktor fuer seitengroesse, da norm koordinaten ungleich in x und y

     sizex= 0.22 ;set manually for size of image (in normalized coord size)
     ;draw x at observed position
     tempimg=fltarr(nx,ny,3)
     tempimg[*,*,0] = stokes[*,*,imgind]
     tempimg[*,*,1] = stokes[*,*,imgind]
     tempimg[*,*,2] = stokes[*,*,imgind]
     tempimg[i-5:i+5,j,0] = max(tempimg) &     tempimg[i, j-5:j+5,0] = max(tempimg)
     tempimg[i-5:i+5,j,1] = min(tempimg) &     tempimg[i, j-5:j+5,1] = min(tempimg)
     tempimg[i-5:i+5,j,2] = min(tempimg) &     tempimg[i, j-5:j+5,2] = min(tempimg)
     tvscl,tempimg,0.01,0.1,xsize=sizex,/norm,true=3

     plot, lambda, stokes[i,j,*,0], $
           XTITLE=xtitle, YTITLE=ytitlei, /YS,position=[0.32,0.58,0.6,0.95],/norm ,/xs,/noer,psym=-4,$
           charthick=2,xthick=2,ythick=2,thick=2
      if keyword_set(xl) then xline,xl,lines=1

    ;plot Q/I
      if not keyword_set(equal) then begin 
        plot, lambda, stokes[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,charthick=2,$
        position=[0.7,0.58,0.98,0.95],/norm,/noer,/xs,xthick=2,ythick=2,thick=2
       endif else begin
        xm = max(abs(stokes[i,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda, stokes[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,charthick=2,$
        position=[0.7,0.58,0.98,0.95],/norm,/noer,yrange=yr,/xs,xthick=2,ythick=2,thick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl) then xline,xl,lines=1

     ;plot U/I
       if not keyword_set(equal) then begin 
        plot, lambda, stokes[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,charthick=2,$
        position=[0.32,0.1,0.6,0.47],/norm,/noer,/xs,xthick=2,ythick=2,thick=2
       endif else begin
        xm = max(abs(stokes[i,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda, stokes[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,charthick=2,$
        position=[0.32,0.1,0.6,0.47],/norm,/noer,yrange=yr,/xs,xthick=2,ythick=2,thick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl) then xline,xl,lines=1

      ;plot V/I
       if not keyword_set(equal) then begin 
        plot, lambda, stokes[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,charthick=2,$
        position=[0.7,0.1,0.98,0.47],/norm,/noer,/xs,xthick=2,ythick=2,thick=2
       endif else begin
        xm = max(abs(stokes[i,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda, stokes[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,charthick=2,$
        position=[0.7,0.1,0.98,0.47],/norm,/noer,yrange=yr,/xs,xthick=2,ythick=2,thick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl) then xline,xl,lines=1
  
     !p.charsize=1.
      xyouts, 0.05, 0.01, "[" + string(i, FORMAT='(I3)') + ", " + $
        string(j MOD Ny, FORMAT='(I3)') + "]", /NORMAL,charthick=2

     device,/close
     set_plot,'x'      
    ENDIF
;------------------- end eps file -----------------------------------------------------

;------- plotting spectra --------------
   ;if cursor inside image
    IF (i GE 0 AND i LT Nx  AND j GE 0 AND j LT Ny) THEN BEGIN
      wset, profileWindow

    ;plot I
      if ~keyword_set(irange) then $
      plot, lambda, stokes[i,j,*,0],  XTITLE=xtitle, YTITLE=ytitlei, $
      /YSTYLE,position=[0.1,0.58,0.45,0.95],/norm ,/xs, psym=-4 else $ ;YRANGE=[dmin, dmax], 
      plot, lambda, stokes[i,j,*,0],  XTITLE=xtitle, YTITLE=ytitlei, $
      /YSTYLE,position=[0.1,0.58,0.45,0.95],/norm ,/xs ,yrange=[irange[0],irange[1]],psym=-4

      if keyword_set(xl) then xline,xl,lines=1

    ;plot Q/I
      if ~keyword_set(qrange) then begin
         if not keyword_set(equal) then begin 
            plot, lambda, stokes[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
                  position=[0.55,0.58,0.9,0.95],/norm,/noer  ,/xs
         endif else begin
            xm = max(abs(stokes[i,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
            plot, lambda, stokes[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
                  position=[0.55,0.58,0.9,0.95],/norm,/noer,yrange=yr ,/xs
         endelse
      endif else begin
         plot, lambda, stokes[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
                  position=[0.55,0.58,0.9,0.95],/norm,/noer,yrange=[qrange[0],qrange[1]] ,/xs
      endelse

      yline,0,lines=1  
      if keyword_set(xl) then xline,xl,lines=1

     ;plot U/I
     if ~keyword_set(urange) then begin
        if not keyword_set(equal) then begin 
           plot, lambda, stokes[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
                 position=[0.1,0.1,0.45,0.47],/norm,/noer ,/xs
        endif else begin
           xm = max(abs(stokes[i,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
           plot, lambda, stokes[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
                 position=[0.1,0.1,0.45,0.47],/norm,/noer,yrange=yr ,/xs
        endelse
     endif else begin
        plot, lambda, stokes[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
              position=[0.1,0.1,0.45,0.47],/norm,/noer,yrange=[urange[0],urange[1]] ,/xs
     endelse
     
     yline,0,lines=1  
     if keyword_set(xl) then xline,xl,lines=1

      ;plot V/I
     if ~keyword_set(vrange) then begin
        if not keyword_set(equal) then begin 
           plot, lambda, stokes[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
                 position=[0.55,0.1,0.9,0.47],/norm,/noer  ,/xs
        endif else begin
           xm = max(abs(stokes[i,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
           plot, lambda, stokes[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
                 position=[0.55,0.1,0.9,0.47],/norm,/noer,yrange=yr ,/xs
        endelse
     endif else begin
          plot, lambda, stokes[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
                 position=[0.55,0.1,0.9,0.47],/norm,/noer,yrange=[vrange[0],vrange[1]] ,/xs
     endelse

     yline,0,lines=1  
     if keyword_set(xl) then xline,xl,lines=1
  

      xyouts, 0.05, 0.01, "[" + string(i, FORMAT='(I3)') + ", " + $
       string(j MOD Ny, FORMAT='(I3)') + "]", /NORMAL
    ENDIF
  ENDWHILE


end
