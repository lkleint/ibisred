;test condition of X matrix

restore,'01.ta.20140329.6302.X.idl',/ve
xmatl = xmat[*,*,*,1:*]
wlobs = wlobs[1:*]

restore,'01.tb.20140329.6302.X.idl',/ve
xmatr = xmat[*,*,*,1:*]


xmatl_fit = smooth(xmatl,[1,1,25,1],/edge_truncate)
xmatr_fit = smooth(xmatr,[1,1,25,1],/edge_truncate)


   ny = yb-ya+1
   nwl = (size(xmat))[4]


;get pseudo-inverse with SVD
      Xinv_right =fltarr(6,4,ny,nwl)
      Xinv_left = fltarr(6,4,ny,nwl)

print,'inverting X'
     ;invert matrices
 j=1
condl = fltarr(ny)
condr = fltarr(ny)
      for i=0,ny-1 do begin
          svdc,xmatl_fit[*,*,i,j],w,u,v
     	  nw=n_elements(w)
	  tol=1e-6
 	  ww	=fltarr(nw,nw)
    	  z=where(abs(w/max(abs(w))) lt tol) 
    	  if(z(0) ne -1) then for j=0,n_elements(z)-1 do ww(z(j),z(j))=0.
          z=where(abs(w/max(abs(w))) ge tol) 
         if(z(0) ne -1) then for j=0,n_elements(z)-1 do ww(z(j),z(j))=1./w(z(j))
    	invmat=v##ww##transpose(u)
           Xinv_left[*,*,i,j] =invmat
	   condl[i] = max(w)/min(w)
       endfor
  
     for i=0,ny-1 do begin
          svdc,xmatr_fit[*,*,i,j],w,u,v
     	  nw=n_elements(w)
	  tol=1e-6
 	  ww	=fltarr(nw,nw)
    	  z=where(abs(w/max(abs(w))) lt tol) 
    	  if(z(0) ne -1) then for j=0,n_elements(z)-1 do ww(z(j),z(j))=0.
          z=where(abs(w/max(abs(w))) ge tol) 
         if(z(0) ne -1) then for j=0,n_elements(z)-1 do ww(z(j),z(j))=1./w(z(j))
    	invmat=v##ww##transpose(u)
           Xinv_right[*,*,i,j] =invmat
	   condr[i] = max(w)/min(w)
       endfor
  

set_plot,'PS'
device,filename='condition_respmat.eps',/encaps,xsize=15,ysize=10
 plot,condl,/xs,xtitle='pixel in y-direction',title='Condition of 6302 resp. matrix (max(EV)/min(EV))',yrange=[0,20],/ys,xthick=2,ythick=2,thick=2,charthick=2
oplot,condr,lines=2,thick=2
al_legend,['left beam', 'right beam'],box=0,lines=[0,2],thick=[2,2]
device,/close
set_plot,'x'



