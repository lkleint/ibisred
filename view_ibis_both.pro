;lk 12.5.11
;purpose: input stokesout [x,y,lambda,4] and display I and V/I for
;photosphere and chromosphere
;including division to get Q/I,U/I,V/I

; keywords: lambda1, lambda2 = vector of lambda for x axis
;           imgind = for example [1,2] = indices of images to display, default = middle ones
;           nofrac = do not calculate fractional polarization states
;           equal = equal scaling for plots, i.e. +- max value of pol. state
;           xl1, xl2 = xline at certain wavelength (for example at line center)
;           vv =  show v instead of i for overview plots
;           all = show I, Q, U, V not just I and V

;right click closes the windows, left click creates ps file with current spectra

; requires: xline.pro and yline.pro (which draw vertical / horizontal lines)

pro view_ibis_both,data6302,data8542,lambda1=lambda1,lambda2=lambda2,imgind=imgind,$
              nofrac=nofrac,equal=equal,xl1=xl1,xl2=xl2,vv=vv, all=all

;----------------- variables -------------------------------------------------------------
 dimension = size(data6302)
  IF (dimension[0] NE 4) THEN BEGIN
    print, "VIEWCUBE: data needs to be 4-D spectral data cube"
    return
  ENDIF

  Nx = dimension[1]
  Ny = dimension[2]
  Nlambda1 = dimension[3]
  dmax = max(data6302, MIN=dmin)
  TRUE = 1
  WAIT = 2
  LEFT_BUTTON   = 1
  MIDDLE_BUTTON = 2
  RIGHT_BUTTON  = 4

  dimension2 = size(data8542)
  Nlambda2 = dimension2[3] ;number of wl steps is usually different
  dmax2 = max(data8542, MIN=dmin2)

  ;test for imgind format
  if keyword_set(imgind) then begin
  if (size(imgind))[1] ne 2 then message,'imgind needs to be vector with 2 elements'
  endif

  ;stokes... contains the images to display
  stokes6302 = data6302 
  stokes8542 = data8542



;--- make fractional polarization states -----------------------------------------------

if not keyword_set(nofrac) then begin
  stokes6302[*,*,*,3] = data6302[*,*,*,3]/data6302[*,*,*,0]
  stokes6302[*,*,*,1] = data6302[*,*,*,1]/data6302[*,*,*,0]
  stokes6302[*,*,*,2] = data6302[*,*,*,2]/data6302[*,*,*,0]
  stokes8542[*,*,*,3] = data8542[*,*,*,3]/data8542[*,*,*,0]
  stokes8542[*,*,*,1] = data8542[*,*,*,1]/data8542[*,*,*,0]
  stokes8542[*,*,*,2] = data8542[*,*,*,2]/data8542[*,*,*,0]
  ytitleq = 'Q/I'
  ytitleu = 'U/I'
  ytitlev = 'V/I'
endif else begin
  ytitleq = 'Q'
  ytitleu = 'U'
  ytitlev = 'V'
endelse


;---- create window for reference images -----------------------------------------------
 window, /FREE,xsize=2*nx,ysize=ny
 originalWindow = !D.WINDOW


;------- display reference image -------------------------------------------------------
if not keyword_set(imgind) then imgind = [nlambda1/2,nlambda2/2]
if not keyword_set(vv) then tvscl, data6302[*, *, imgind[0]] else $
                    tvscl,stokes6302[*, *, imgind[0],3]
if not keyword_set(vv) then tvscl,data8542[*, *, imgind[1]],nx,0 else $
                    tvscl,stokes8542[*, *, imgind[1],3],nx,0


;------ set up plotting for profiles ---------------------------------------------------
 
  ;lambda scale will either contain real values or indices [1,2,3,...]
  IF (keyword_set(LAMBDA1)) THEN $
   xtitle = 'Wavelength' $
  ELSE BEGIN
    xtitle = 'Wavelength index'
    lambda1 = findgen(Nlambda1)
  ENDELSE
  IF not keyword_set(LAMBDA2) THEN lambda2 = findgen(Nlambda2)

   ytitlei = 'I profile'


  window, TITLE='Spectral profile', /FREE  ;user can resize the window manually
  profileWindow = !D.WINDOW

  Nspec = 1 ;only one spectrum to show
  nshow = 1

  WHILE TRUE DO BEGIN
    wset, originalWindow
    cursor, i, j, wait, /DEVICE, DOWN=down

;------------  right click closes spectra ---------------------------------------------
    IF (!MOUSE.BUTTON EQ RIGHT_BUTTON) THEN BEGIN
      wdelete, profileWindow
      wdelete, originalwindow
      return
    ENDIF

;------------  left click makes eps file ----------------------------------------------

    IF (!MOUSE.BUTTON EQ LEFT_BUTTON) THEN BEGIN
     print,'created view_ibis_both.eps' 
     !p.charsize=0.55
     set_plot,'PS'
     xpage = 25 & ypage = 10
     device,filename='view_ibis_both.eps',xoffset=0,yoffset=0,/encaps,/portrait,$
            xsize=xpage,ysize=ypage,/color,bits_per_pixel=16
                corrfact = ypage/xpage ;korrrekturfaktor fuer seitengroesse, da norm koordinaten ungleich in x und y

     sizex= 0.1 ;set manually for size of image (in normalized coord size)
 
     i =  i mod nx ;if the right image is clicked on

    ;draw x at observed position 6302
     tempimg=fltarr(nx,ny,3)
     tempimg[*,*,0] = data6302[*,*,imgind[0]]
     tempimg[*,*,1] = data6302[*,*,imgind[0]]
     tempimg[*,*,2] = data6302[*,*,imgind[0]]
     tempimg[i-5:i+5,j,0] = max(tempimg) &     tempimg[i, j-5:j+5,0] = max(tempimg)
     tempimg[i-5:i+5,j,1] = min(tempimg) &     tempimg[i, j-5:j+5,1] = min(tempimg)
     tempimg[i-5:i+5,j,2] = min(tempimg) &     tempimg[i, j-5:j+5,2] = min(tempimg)
     tvscl,tempimg,0.01,0.5,xsize=sizex,/norm,true=3

    ;draw x at observed position 8542
     tempimg=fltarr(nx,ny,3)
     tempimg[*,*,0] = data8542[*,*,imgind[1]]
     tempimg[*,*,1] = data8542[*,*,imgind[1]]
     tempimg[*,*,2] = data8542[*,*,imgind[1]]
     tempimg[i-5:i+5,j,0] = max(tempimg) &     tempimg[i, j-5:j+5,0] = max(tempimg)
     tempimg[i-5:i+5,j,1] = min(tempimg) &     tempimg[i, j-5:j+5,1] = min(tempimg)
     tempimg[i-5:i+5,j,2] = min(tempimg) &     tempimg[i, j-5:j+5,2] = min(tempimg)
     tvscl,tempimg,0.01,0.00,xsize=sizex,/norm,true=3

    ;define positions for plots
     ipos = [0.15,0.6,0.31,0.95]
     qpos = [0.37,0.6,0.54,0.95]
     upos = [0.60,0.6,0.77,0.95]
     vpos = [0.83,0.6,0.99,0.95]
     ycorr = [0.,0.5,0.,0.5]

    ;-------- plot upper row (6302) ------------------------
    ;plot I
     plot, lambda1, stokes6302[i,j,*,0], $
       XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,position=ipos,/norm ,/xs,/noer,thick=2,xthick=2,ythick=2
      if keyword_set(xl1) then xline,xl1,lines=1
      oplot, lambda1, stokes6302[i,j,*,0],psym=4,symsize=0.5

    ;plot Q/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes6302[i,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1
  
   ;plot U/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes6302[i,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1
 
  ;plot V/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes6302[i,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1

    ;-------- plot lower row (8542) ------------------------
     ;plot I
     plot, lambda2, stokes8542[i,j,*,0], $
       XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,position=ipos-ycorr,/norm ,/xs,/noer,thick=2,xthick=2,ythick=2 
      if keyword_set(xl2) then xline,xl2,lines=1
     oplot, lambda2, stokes8542[i,j,*,0],psym=4,symsize=0.5

    ;plot Q/I
      if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos-ycorr,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes8542[i,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos-ycorr,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1
 
    ;plot U/I
      if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos-ycorr,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes8542[i,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos-ycorr,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1

     ;plot V/I
      if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos-ycorr,/norm,/noer  ,/xs,thick=2,xthick=2,ythick=2
       endif else begin
        xm = max(abs(stokes8542[i,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos-ycorr,/norm,/noer,yrange=yr ,/xs,thick=2,xthick=2,ythick=2
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1
 
    ;pixel position at lower right corner
       xyouts, 0.95, 0.01, "[" + string(i mod nx, FORMAT='(I3)') + ", " + $
       string(j MOD Ny, FORMAT='(I3)') + "]", /NORMAL

 
 
     device,/close
     set_plot,'x'  
     !p.charsize=1.0    
    ENDIF
;------------------- end eps file -----------------------------------------------------



;------- plotting spectra -------------------------------------------------------------

    ;if cursor inside image
    IF (i GE 0 AND i LT 2*Nx  AND j GE 0 AND j LT Ny) THEN BEGIN
    wset, profileWindow

   ;------------------------- only plot I and V/I -----------------
    if not keyword_set(all) then begin 

    ;plot I 6302
      plot, lambda1, stokes6302[i mod nx,j,*,0], $
       XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,position=[0.1,0.58,0.45,0.95],/norm ,/xs ;YRANGE=[dmin, dmax], 
      if keyword_set(xl1) then xline,xl1,lines=1

    ;plot V/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=[0.55,0.58,0.9,0.95],/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes6302[i mod nx,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=[0.55,0.58,0.9,0.95],/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1


     ;plot I 8542
        plot, lambda2, stokes8542[i mod nx,j,*,0], XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,$
        position=[0.1,0.1,0.45,0.47],/norm,/noer ,/xs
        if keyword_set(xl2) then xline,xl2,lines=1

      ;plot V/I
       if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=[0.55,0.1,0.9,0.47],/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes8542[i mod nx,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=[0.55,0.1,0.9,0.47],/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1
  

      xyouts, 0.05, 0.01, "[" + string(i mod nx, FORMAT='(I3)') + ", " + $
       string(j MOD Ny, FORMAT='(I3)') + "]", /NORMAL

      endif else begin  
      ;---------------------- keyword all --------------------

      ipos = [0.05,0.58,0.24,0.95]
      qpos = [0.3,0.58,0.49,0.95]
      upos = [0.55,0.58,0.74,0.95]
      vpos = [0.80,0.58,0.99,0.95]

      ycorr = [0.,.48,0.,0.48]  ;correction for bottom plots

      ;plot I 6302
      plot, lambda1, stokes6302[i mod nx,j,*,0], $
       XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,position=ipos,/norm ,/xs ;YRANGE=[dmin, dmax], 
      if keyword_set(xl1) then xline,xl1,lines=1


      ;plot Q/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i mod nx,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes6302[i mod nx,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i mod nx,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1
 
      ;plot U/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i mod nx,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes6302[i mod nx,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i mod nx,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1
   
    ;plot V/I
      if not keyword_set(equal) then begin 
        plot, lambda1, stokes6302[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes6302[i mod nx,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda1, stokes6302[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl1) then xline,xl1,lines=1


     ;plot I 8542
        plot, lambda2, stokes8542[i mod nx,j,*,0], XTITLE=xtitle, YTITLE=ytitlei, /YSTYLE,$
        position=ipos-ycorr,/norm,/noer ,/xs
        if keyword_set(xl2) then xline,xl2,lines=1

       ;plot Q/I
       if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i mod nx,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos-ycorr,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes8542[i mod nx,j,*,1])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i mod nx,j,*,1], XTITLE=xtitle, YTITLE=ytitleq, /YSTYLE,$
        position=qpos-ycorr,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1
  
      ;plot U/I
       if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i mod nx,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos-ycorr,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes8542[i mod nx,j,*,2])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i mod nx,j,*,2], XTITLE=xtitle, YTITLE=ytitleu, /YSTYLE,$
        position=upos-ycorr,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1

     ;plot V/I
       if not keyword_set(equal) then begin 
        plot, lambda2, stokes8542[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos-ycorr,/norm,/noer  ,/xs
       endif else begin
        xm = max(abs(stokes8542[i mod nx,j,*,3])) & yr = [-xm-0.05*xm,xm+0.05*xm]
        plot, lambda2, stokes8542[i mod nx,j,*,3], XTITLE=xtitle, YTITLE=ytitlev, /YSTYLE,$
        position=vpos-ycorr,/norm,/noer,yrange=yr ,/xs
       endelse
      yline,0,lines=1  
      if keyword_set(xl2) then xline,xl2,lines=1
  

       xyouts, 0.05, 0.01, "[" + string(i mod nx, FORMAT='(I3)') + ", " + $
       string(j MOD Ny, FORMAT='(I3)') + "]", /NORMAL

 

      endelse

    ENDIF
  ENDWHILE


end
