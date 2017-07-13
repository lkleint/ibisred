PRO plot_responsematrix
;makes diagnostics plots of response matrix

restore,'01.ta.20140329.6302.X.idl',/ve

set_plot,'PS'
device,filename='respmat_left2.eps',/encaps,xsize=20,ysize=25
!p.charsize=1.3
!p.multi=[0,1,6]
b=0 & a=0
plot,xmat[a,b,*,1],/xs,/ys,yrange=[0,2],thick=2,title='6302 Response matrix, left, [0,0] to [0,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=1
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[1,0] to [1,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=2
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[2,0] to [2,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=3
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[3,0] to [3,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b

plot,wp_aoffset[0,*,1],title='waveplate offset',/xs,/ys
plot,wp_ret[0,*,1],title='Retardance',/xs,/ys
!p.multi=0
device,/close
set_plot,'x'

restore,'01.tb.20140329.6302.X.idl',/ve


set_plot,'PS'
device,filename='respmat_right2.eps',/encaps,xsize=20,ysize=25
!p.charsize=1.3
!p.multi=[0,1,6]
b=0 & a=0
plot,xmat[a,b,*,1],/xs,/ys,yrange=[0,2],thick=2,title='6302 Response matrix, right, [0,0] to [0,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=1
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[1,0] to [1,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=2
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[2,0] to [2,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b
b=0 & a=3
plot,xmat[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[3,0] to [3,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmat[a,b,*,1],lines=b

plot,wp_aoffset[0,*,1],title='waveplate offset',/xs,/ys
plot,wp_ret[0,*,1],title='Retardance',/xs,/ys
!p.multi=0
device,/close
set_plot,'x'



;----- from within program

set_plot,'PS'
device,filename='respmat_left2.eps',/encaps,xsize=20,ysize=25
!p.charsize=1.3
!p.multi=[0,1,6]
b=0 & a=0
plot,xmatl_fit[a,b,*,1],/xs,/ys,yrange=[0,2],thick=2,title='6302 Response matrix, left, [0,0] to [0,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmatl_fit[a,b,*,1],lines=b
b=0 & a=1
plot,xmatl_fit[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[1,0] to [1,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmatl_fit[a,b,*,1],lines=b
b=0 & a=2
plot,xmatl_fit[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[2,0] to [2,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmatl_fit[a,b,*,1],lines=b
b=0 & a=3
plot,xmatl_fit[a,b,*,1],/xs,/ys,yrange=[-1,1],thick=2,title='[3,0] to [3,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xmatl_fit[a,b,*,1],lines=b

plot,wp_aoffset[0,*,1],title='waveplate offset',/xs,/ys
plot,wp_ret[0,*,1],title='Retardance',/xs,/ys
!p.multi=0
device,/close
set_plot,'x'




END
