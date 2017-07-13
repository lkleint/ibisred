;create overview plots of all calibrated 8542 data

;OK total linear polarization: exclude < 1% of avg intensity (not Q/I,
;otherwise wings too bad). then total(abs(linpol))

;OK total cicular polarization: exclude < 1% of avg I, then
;total(abs(circpol))

;OK averages of continuum polarization in quiet sun (should be 0)
; plot,avg(stokesout[200:250,*,1,0-3]) for i,q,u,v

;OK images of stokes plots in some pixels where signal is > 3%


;OK images of continuum images of stokesout
;OK images of line wing and center of stokesout

;execute from main IBIS reduction directory

;---------
;### to do: create analysis directory if it does not exist already


;---------------------- variables -----------------
;set variables
x1 = 50.    ;FOV to display
x2 = 350   ;right edge of FOV in px
y1 = 50.
y2 = 900.

c1 = 200.  ;x pixel between which the continuum is averaged
c2 = 250.

;
xpaper=20.
ypaper=28.

;positions of plots
p1x = .08
p2x = .38
p3x = .68
p1y = .65
dx = .16
dy = dx * (y2-y1+1.)/(x2-x1+1.) * xpaper/ypaper

p2y = .5 ;for cont pol
dpx = .18
dpy = .1
p4x = .33
p5x = .56
p6x = .79
!p.charsize=.8

;for images
p3y = .1
dximg = .15
dyimg = dximg * (y2-y1+1.)/(x2-x1+1.) * xpaper/ypaper

;for page 2
p4y = .25

;----- find files -----------------------------------

files = file_search('*/8542*tatb*sav',count=nfiles)

FOR jj=0,nfiles-1 DO BEGIN

fname1 = strmid(files[jj],37,15,/rev)
fname2 = strmid(files[jj],21,10,/rev)
if strpos(files[jj],'results') ne -1 then fname1 = strmid(files[jj],37,11,/rev)
if strpos(files[jj],'results') ne -1 then x1=100
if strpos(files[jj],'results') ne -1 then x2=350

;------------------------- plotting --------------------
set_plot,'PS'
device,encaps=0,filename='analysis/overv_'+fname1+'_'+fname2+'.eps',xsize=xpaper,ysize=ypaper,/color,xoffset=0,yoffset=0,bits=8

restore,files[jj]
wave = nb_expos.wave[uniq(nb_expos.wave)]
wave = wave[sort(wave)]

;total polarizations
plot_image,total(abs(stokesout[x1:x2,y1:y2,*,1])>.01,3),ticklen=-.02,position=[p1x,p1y,p1x+dx,p1y+dy],title='total Q',min=-50,/norm
plot_image,total(abs(stokesout[x1:x2,y1:y2,*,2])>.01,3),ticklen=-.02,position=[p2x,p1y,p2x+dx,p1y+dy],title='total U',/noer,min=-50,/norm
plot_image,total(abs(stokesout[x1:x2,y1:y2,*,3])>.01,3),ticklen=-.02,position=[p3x,p1y,p3x+dx,p1y+dy],title='total V',/noer,min=-200,/norm

;average continuum polarization
tek_color
plot,avg(stokesout[c1:c2,y1:y2,0,0],0),xtitle='y pixel',title='Continuum I',position=[p1x,p2y,p1x+dpx,p2y+dpy],/noer,$
     xthick=2,ythick=2,thick=2,charthick=2,/xs,yrange=[min(stokesout[c1:c2,y1:y2,0:2,0]),max(stokesout[c1:c2,y1:y2,0:2,0])]
oplot,avg(stokesout[c1:c2,y1:y2,1,0],0),color=2
oplot,avg(stokesout[c1:c2,y1:y2,2,0],0),color=4
al_legend,['lambda='+string(wave[0],format='(F7.2)'),'lambda='+string(wave[1],format='(F7.2)'),'lambda='+string(wave[2],format='(F7.2)')],color=[0,2,4],lines=[0,0,0],box=0,position=[0,min(stokesout[c1:c2,y1:y2,0:2,0])-200]


plot,avg(stokesout[c1:c2,y1:y2,0,1],0),xtitle='y pixel',title='Continuum Q',position=[p4x,p2y,p4x+dpx,p2y+dpy],/noer,$
     xthick=2,ythick=2,thick=2,charthick=2,/xs,yrange=[min(stokesout[c1:c2,y1:y2,0:2,1]),max(stokesout[c1:c2,y1:y2,0:2,1])]
oplot,avg(stokesout[c1:c2,y1:y2,1,1],0),color=2
oplot,avg(stokesout[c1:c2,y1:y2,2,1],0),color=4
yline,0,/lines

plot,avg(stokesout[c1:c2,y1:y2,0,2],0),xtitle='y pixel',title='Continuum U',position=[p5x,p2y,p5x+dpx,p2y+dpy],/noer,$
     xthick=2,ythick=2,thick=2,charthick=2,/xs,yrange=[min(stokesout[c1:c2,y1:y2,0:2,2]),max(stokesout[c1:c2,y1:y2,0:2,2])]
oplot,avg(stokesout[c1:c2,y1:y2,1,2],0),color=2
oplot,avg(stokesout[c1:c2,y1:y2,2,2],0),color=4
yline,0,/lines

plot,avg(stokesout[c1:c2,y1:y2,0,3],0),xtitle='y pixel',title='Continuum V',position=[p6x,p2y,p6x+dpx,p2y+dpy],/noer,$
     xthick=2,ythick=2,thick=2,charthick=2,/xs,yrange=[min(stokesout[c1:c2,y1:y2,0:2,3]),max(stokesout[c1:c2,y1:y2,0:2,3])]
oplot,avg(stokesout[c1:c2,y1:y2,1,3],0),color=2
oplot,avg(stokesout[c1:c2,y1:y2,2,3],0),color=4
yline,0,/lines


loadct,0
;stokes I,Q,U,V continuum
plot_image,stokesout[x1:x2,y1:y2,1,0],ticklen=-.02,position=[p1x,p3y,p1x+dximg,p3y+dyimg],/noer,title='Continuum images'
plot_image,stokesout[x1:x2,y1:y2,1,1],ticklen=-.02,position=[p4x,p3y,p4x+dximg,p3y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,1,2],ticklen=-.02,position=[p5x,p3y,p5x+dximg,p3y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,1,3],ticklen=-.02,position=[p6x,p3y,p6x+dximg,p3y+dyimg],/noer

;stokes I,Q,U,V wing
plot_image,stokesout[x1:x2,y1:y2,8,0],ticklen=-.02,position=[p1x,p1y,p1x+dximg,p1y+dyimg],title='Line wing images'
plot_image,stokesout[x1:x2,y1:y2,8,1],ticklen=-.02,position=[p4x,p1y,p4x+dximg,p1y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,8,2],ticklen=-.02,position=[p5x,p1y,p5x+dximg,p1y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,8,3],ticklen=-.02,position=[p6x,p1y,p6x+dximg,p1y+dyimg],/noer

;stokes I,Q,U,V core
plot_image,stokesout[x1:x2,y1:y2,11,0],ticklen=-.02,position=[p1x,p4y,p1x+dximg,p4y+dyimg],title='Line core images',/noer
plot_image,stokesout[x1:x2,y1:y2,11,1],ticklen=-.02,position=[p4x,p4y,p4x+dximg,p4y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,11,2],ticklen=-.02,position=[p5x,p4y,p5x+dximg,p4y+dyimg],/noer
plot_image,stokesout[x1:x2,y1:y2,11,3],ticklen=-.02,position=[p6x,p4y,p6x+dximg,p4y+dyimg],/noer

;Stokes plots
tmp = max(total(abs(stokesout[x1:x2,y1:y2,*,3])>.01,3),pos)
pp = array_indices(stokesout[x1:x2,y1:y2,*,3],pos)
plot,stokesout[pp[0]+x1,pp[1]+y1,*,0],title='I at '+string(pp[0]+x1,format='(I3)')+', '+string(pp[1]+y1,format='(I3)'),/noer,position=[p1x,p3y,p1x+dpx,p3y+dpy],/xs
plot,stokesout[pp[0]+x1,pp[1]+y1,*,1],title='Q',/noer,position=[p4x,p3y,p4x+dpx,p3y+dpy],/xs
yline,0,/lines
plot,stokesout[pp[0]+x1,pp[1]+y1,*,2],title='U',/noer,position=[p5x,p3y,p5x+dpx,p3y+dpy],/xs
yline,0,/lines
plot,stokesout[pp[0]+x1,pp[1]+y1,*,3],title='V',/noer,position=[p6x,p3y,p6x+dpx,p3y+dpy],/xs
yline,0,/lines

xyouts,.1,.05,files[jj]+',   DATE_OBS: '+nb_expos.date_obs[0],/norm,charsize=.8,charthick=2

device,/close
set_plot,'x'

print,'Done: ',files[jj],',   DATE_OBS: '+nb_expos.date_obs[0]
ENDFOR


END
