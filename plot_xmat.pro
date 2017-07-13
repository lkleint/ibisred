;plot the X matrix
;(quicklook program for diagnostics)
;Nov 2014, LK


PRO plot_xmat,xx


!p.multi=[0,1,4]
b=0 & a=0
plot,xx[a,b,*],/xs,/ys,yrange=[0,2],thick=2,title='Response matrix, left, [0,0] to [0,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xx[a,b,*],lines=b
b=0 & a=1
plot,xx[a,b,*],/xs,/ys,yrange=[-1.5,1.5],thick=2,title='[1,0] to [1,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xx[a,b,*],lines=b
b=0 & a=2
plot,xx[a,b,*],/xs,/ys,yrange=[-1.5,1.5],thick=2,title='[2,0] to [2,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xx[a,b,*],lines=b
b=0 & a=3
plot,xx[a,b,*],/xs,/ys,yrange=[-1.5,1.5],thick=2,title='[3,0] to [3,5]',xthick=2,ythick=2,charthick=2
for b=1,5 do oplot,xx[a,b,*],lines=b

!p.multi=0



END
