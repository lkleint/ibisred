;may 2011 lk
;oct 2011 lk, added window keyword for window index
;13.4.17, removed xrange=[0,511] from contour to fully show single beam

;display 2 windows with blueshift maps calculated by ibis_blueshift
pro view_blueshift,bfile,windex=windex

restore,bfile,/ve

if not keyword_set(windex) then windex=1


window,windex,xsize=512,ysize=512
offpolma = offset.poly_fit*1000. ;in mA
polmin = min(offpolma,max=polmax)
lvl = (polmax-polmin)/7.*findgen(8)+polmin*1.0 ;levels to display
contour,offpolma,/xs,/ys,/follow,title='blueshift [mA], poly_fit:'+bfile,$
  nlevels = 6, levels = lvl,c_labels=fltarr(8)+1


window,windex+1,xsize=512,ysize=512
offcogma = offset.cog_fit*1000. ;in mA
;cogmin = min(offcogma,max=cogmax)
;lvl = (cogmax-cogmin)/7.*findgen(8)+cogmin*1.0 ;levels to display
;probably smarter to use same levels for both plots...
contour,offcogma,/xs,/ys,/follow,title='blueshift [mA], cog:'+bfile,$
  nlevels = 6, levels = lvl,c_labels=fltarr(8)+1

end
