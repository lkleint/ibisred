;creates some plots about the polcal
;used to test why X matrix is so bad


;get a "clear" image
fn1 = file_search('/Volumes/work4/dst_2014/reduc/20140329/temporary/polcal_330/6302*.sav')
restore,fn1[0],/ve


;image of fractions of intensity for the different polarization states
;for the first wavelength
set_plot,'PS'
device,filename='polcal_lbeam_fractions_330.eps',/encaps,xsize=20,ysize=25,/color
!p.multi=[0,1,5]
!p.charsize=1.3
tek_color
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,1],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/1',xthick=2,ythick=2,thick=2,charthick=2,title='avg over x=[200:400], black=line wing, blue=line core',$
     xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,25],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,2],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/2',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,26],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,3],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/3',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,27],0),color=4
tmp =avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,4],0) 
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/4',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,28],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,5],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/5',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,29],0),color=4
!p.multi=0
device,/close
set_plot,'x'





;get a "clear" image
fn1 = file_search('/Volumes/work4/dst_2014/reduc/20140329/temporary/polcal_60/6302*.sav')
restore,fn1[0],/ve


;image of fractions of intensity for the different polarization states
;for the first wavelength
set_plot,'PS'
device,filename='polcal_lbeam_fractions_060.eps',/encaps,xsize=20,ysize=25,/color
!p.multi=[0,1,5]
!p.charsize=1.3
tek_color
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,1],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/1',xthick=2,ythick=2,thick=2,charthick=2,title='avg over x=[200:400], black=line wing, blue=line core',$
     xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,25],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,2],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/2',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,26],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,3],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/3',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,27],0),color=4
tmp =avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,4],0) 
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/4',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,28],0),color=4
tmp = avg(nb_data[200:400,*,0],0)/avg(nb_data[200:400,*,5],0)
plot,tmp,/xs,/ys,xtitle='y px',ytitle='image 0/5',xthick=2,ythick=2,thick=2,charthick=2,xrange=[50,950],yrange=[.95*min(tmp[50:950]),1.05*max(tmp[50:950])]
oplot,avg(nb_data[200:400,*,24],0)/avg(nb_data[200:400,*,29],0),color=4
!p.multi=0
device,/close
set_plot,'x'


