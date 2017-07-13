pro make_stokesmovie,stokesout,qs,nb_expos,framerate=framerate,filename=filename
;creates movie of all stokes states
;stokesout is regular vector



sz = size(stokesout)

fullarr = fltarr((sz[1]-30)*4,sz[2]-100,sz[3])

lim=200
tmp = max(abs(stokesout[15:sz[1]-16,50:sz[2]-51,*,0]))
fullarr[0,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,0],left=1,right=tmp)  ;I
;tmp = max(abs(stokesout[15:sz[1]-16,50:sz[2]-51,*,1]))
;fullarr[sz[1]-30,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,1],left=-1*tmp,right=tmp);Q
fullarr[sz[1]-30,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,1],left=-1*lim,right=lim);Q
;tmp = max(abs(stokesout[15:sz[1]-16,50:sz[2]-51,*,2]))
;fullarr[(sz[1]-30)*2,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,2],left=-1*tmp,right=tmp)  ;U
fullarr[(sz[1]-30)*2,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,2],left=-1*lim,right=lim)  ;U
;tmp = max(abs(stokesout[15:sz[1]-16,50:sz[2]-51,*,3]))
;fullarr[(sz[1]-30)*3,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,3],left=-1*tmp,right=tmp)  ;V
fullarr[(sz[1]-30)*3,0,0] = scaleimg(stokesout[15:sz[1]-16,50:sz[2]-51,*,3],left=-1*lim,right=lim)  ;V

wl = diffelement(nb_expos.wave,/names,/double)
sz2 = size(fullarr)
fullarr2 = fullarr
!p.font=0
!p.charsize=1.2
device,set_font="-adobe-helvetica-bold-o-normal--0-0-100-100-p-0-iso8859-1"
window,xsize=sz2[1],ysize=sz2[2]
FOR i=0,sz2[3]-1 do begin
tv,fullarr[*,*,i]
plot,wl,avg(avg(stokesout[qs[0]:qs[1],qs[2]:qs[3],*,0],0),0),xthick=3,ythick=3,thick=3,charthick=3,position=[.05,.85,.22,.95],$
     /norm,/noer,/xs,color=0,xticks=2,xtickv=[6302.3,6302.5,6302.7],xminor=2,psym=-4
xline,wl[i],/lines,thick=3,color=0
a=tvrd()
fullarr2[*,*,i]=a
ENDFOR

if ~keyword_set(filename) then filename='stokesmovie.mp4'
  video = idlffvideowrite(filename)
  tvlct,rr,gg,bb,/get
  if ~keyword_set(framerate) then framerate = 10
  framedims = [sz2[1],sz2[2]]
  stream = video.addvideostream(framedims[0], framedims[1], framerate, BIT_RATE=8e8)
  vidarr = fullarr2
  frame = bytarr(3,framedims[0],framedims[1])
  FOR i=0,sz[3]-1 do begin
      frame[0,*,*] = rr[vidarr[*,*,i]]
      frame[1,*,*] = gg[vidarr[*,*,i]]
      frame[2,*,*] = bb[vidarr[*,*,i]]
      timestamp = video.put(stream,frame)
  ENDFOR
  video.cleanup
  
;------- plot showing the trend in the continuum ------------
set_plot,'PS'
device,filename='stokes_cont_combbeam.eps',/encaps,xsize=20,ysize=20
!p.multi=[0,1,4]
plot,avg(stokesout[200:300,*,1,0],0),/xs,/ys,title='I (comb beam, fixed offset+ret)',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stokesout[200:300,*,1,1],0),/xs,/ys,title='Q',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stokesout[200:300,*,1,2],0),/xs,/ys,title='U',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stokesout[200:300,*,1,3],0),/xs,/ys,title='V',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
!p.multi=0
device,/close
set_plot,'x'
  


;------- plot showing the trend in the continuum L beam ------------
set_plot,'PS'
device,filename='stokes_cont_lbeam_mod.eps',/encaps,xsize=20,ysize=20
!p.multi=[0,1,4]
plot,avg(stksoutl[200:300,*,1,0],0),/xs,/ys,title='I (l beam, fixed offset+ret, mod matrix by hand)',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutl[200:300,*,1,1],0),/xs,/ys,title='Q',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutl[200:300,*,1,2],0),/xs,/ys,title='U',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutl[200:300,*,1,3],0),/xs,/ys,title='V',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
!p.multi=0
device,/close
set_plot,'x'

set_plot,'PS'
device,filename='stokes_cont_rbeam.eps',/encaps,xsize=20,ysize=20
!p.multi=[0,1,4]
plot,avg(stksoutr[200:300,*,1,0],0),/xs,/ys,title='I (r beam, fixed offset+ret)',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutr[200:300,*,1,1],0),/xs,/ys,title='Q',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutr[200:300,*,1,2],0),/xs,/ys,title='U',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
plot,avg(stksoutr[200:300,*,1,3],0),/xs,/ys,title='V',xrange=[50,sz[2]-50],xthick=2,ythick=2,charthick=2,thick=2,charsize=1.2
!p.multi=0
device,/close
set_plot,'x'


end
