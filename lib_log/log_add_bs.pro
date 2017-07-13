pro log_add_bs,path1,lambda,offset,npoints 
;add a figure with blueshifts in the log

     files = file_search(path1+$
    '/log/blueshift'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles)
    levels = findgen(20)*20.-200. ;predefined levels for contours
 
     set_plot,'PS'
      fblue = path1+'/log/blueshift'+string(lambda, FORMAT='(I4)')+$
           string(nfiles,format='(I1)')+'.eps'
     device,filename=fblue
     device,/encaps,bits_per_pixel=16,xsize=10,ysize=10
     !p.charsize=0.7
     tvscl,offset.cog,0,5.48,xsize=4.01,/cent
     contour,offset.cog_fit*1000.,/follow,levels=levels,title='COG fit [mA]',$
         thick=2,xthick=2,ythick=2,/noer,position=[0.55,0.55,0.95,0.95],$
         /norm,c_charsize=0.5
     tvscl,offset.poly,0,0.47,xsize=4.01,/cent
     contour,offset.poly_fit*1000.,/follow,levels=levels,$
         title='POLY fit [mA]',thick=2,xthick=2,$
         ythick=2,/noer,position=[0.55,0.05,0.95,0.45],/norm,c_charsize=0.5
     xyouts,0.02,0.47,'parabola with '+string(npoints,format='(I1)')+' wl points at each px',/norm
     xyouts,0.02,0.97,'cog at each px',/norm
     !p.charsize=0.7 &  device,/close
     set_plot,'x'

     newtext=strarr(9)
     newtext[0] = 'wavelength: '+string(lambda)+', Fig.~\ref{bs'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'} \\'
     newtext[1] = '\begin{figure}[htb]'
     newtext[2] = '\begin{center}'
     newtext[3] = '\includegraphics[width=10cm]{blueshift'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I1)')+'}'
     newtext[4] = '\caption{Blueshifts: COG (top), POLY (bottom), contours are 20 mA, '+$
             string(lambda,format='(I4)')+'}'
     newtext[5] = '\label{bs'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
     newtext[6] = '\end{center}'
     newtext[7] = '\end{figure}'
     newtext[8] = ' '

     remove_underscore,newtext
     log_write,path1,newtext
     

end   
   
