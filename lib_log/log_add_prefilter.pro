pro log_add_prefilter,path1,lambda,wl,sscan,prefilt_waves,prefilt_trans,xlam,atlas,$
   sswl,prefilt_wl,wscale_flat,prefmult,flatprof


;--- create eps with spectral scan, lamp scan and corrected profiles --- 
     files = file_search(path1+$
    '/log/pref'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles)
 
     set_plot,'PS'
      fblue = path1+'/log/pref'+string(lambda, FORMAT='(I4)')+$
           string(nfiles,format='(I02)')+'.eps'
     device,filename=fblue
     device,/encaps,bits_per_pixel=16,xsize=15,ysize=10,/color
     !p.charsize=0.7
     tek_color
  
     plot,wl,sscan,title='spectral scan and prefilter from lamp scan',/xs,$
     position=[0.05,0.6,0.95,0.95],psym=-4,xthick=2,ythick=2,thick=2,/ys,$
       yrange=[0,1.1],xtitle='relative wavelength'
     oplot,prefilt_waves,prefilt_trans,lines=1,thick=2,color=2

     plot,xlam,atlas,/xs,position=[0.05,0.1,0.95,0.45],title='corrected IBIS profiles',$
     xrange=[min(sswl),max(sswl)],yrange=[0,1.1],/ys,/noer,xthick=2,ythick=2,thick=2,$
     xtitle='absolute wavelength'
    ;interpolate prefilter for wscale_flat
    prefforflat = INTERPOL(prefilt_trans,prefilt_wl,wscale_flat, /QUADRATIC )
    oplot,wscale_flat,flatprof/prefforflat*prefmult,color=4,thick=2
    ;interpolate prefilter for sswl
    prefforss = INTERPOL(prefilt_trans,prefilt_wl,sswl, /QUADRATIC )
    oplot,sswl,sscan/prefforss*prefmult,color=2,thick=2
    al_legend,['FTS atlas','corrected spectral scan','corrected flat'],color=[0,2,4],$
     lines=[0,0,0],pspacing=1.5
    loadct,0

     !p.charsize=1.0 &  device,/close
     set_plot,'x'
     loadct,0

;------ create string for .tex----------------------------
newtext = strarr(9) ;new entries for log
tstr = ''

     newtext[0] = 'prefilter, wavelength: '+string(lambda,format='(I4)')+', Fig.~\ref{pref'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'} \\'
     newtext[1] = ' '
     newtext[2] = '\begin{figure}[htb]'
     newtext[3] = '\begin{center}'
     newtext[4] = '\includegraphics[width=0.9\textwidth]{pref'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'}'
     newtext[5] = '\caption{\textit{Top}: Measured imaging spectral scan (solid) and prefilter from lamp tuning (dotted). Different optical paths lead to an arbitrary wavelength scale. \textit{Bottom}: Corrected profiles vs FTS atlas. '+$
             string(lambda,format='(I4)')+'}'
     newtext[6] = '\label{pref'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
     newtext[7] = '\end{center}'
     newtext[8] = '\end{figure}'
 
;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


log_write,path1,newtext

end
