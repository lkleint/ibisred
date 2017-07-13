pro log_add_pccurve,path1,lambda,region,nbpolcal,savefile,sampleimg,pc_llevel_med
;adds variables that were set to log file
;2.dec.13: added float for fov, otherwise division = 0
;nbpolcal is [112 (4 tablepos, 28 seq), 4 stokes, 2 beams]

fov=region
;create eps of averaged polcal region
     files = file_search(path1+$
    '/log/pcreg'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles)
 
     set_plot,'PS'
      fblue = path1+'/log/pcreg'+string(lambda, FORMAT='(I4)')+$
           string(nfiles,format='(I02)')+'.eps'
     device,filename=fblue
     device,/encaps,bits_per_pixel=8,xsize=20,ysize=10,/color
     !p.charsize=0.7
     tvscl,sampleimg,xsize=10,/cent
     tek_color

     plots,float([fov[0],fov[2],fov[2],fov[0],fov[0]])/(size(sampleimg))[1]/2.,$
           float([fov[1],fov[1],fov[3],fov[3],fov[1]])/(size(sampleimg))[2],color=2,thick=2,/norm
     plots,float(fov[4]+[fov[0],fov[2],fov[2],fov[0],fov[0]])/(size(sampleimg))[1]/2.,$
           float([fov[1],fov[1],fov[3],fov[3],fov[1]])/(size(sampleimg))[2],color=2,thick=2,/norm

     plot,pc_llevel_med,/xs,/ys,thick=2,ythick=2,xthick=2,$
           position=[0.55,0.83,0.97,0.95],/norm,/noer,title='light level (avg over lambda)',$
           xtitle='cal sequence (at each tablepos)';position=[0.55,0.55,0.74,0.95]
     xline,[28,56,84]
     plot,nbpolcal[*,1,0],/xs,/ys,thick=2,ythick=2,xthick=2,$
           position=[0.55,0.55,0.97,0.7],/norm,/noer,title='Q/I light curve'
     oplot,nbpolcal[*,1,1],lines=1,thick=2,color=120;[0.78,0.55,0.97,0.95]
     xline,[28,56,84]
     plot,nbpolcal[*,2,0],/xs,/ys,thick=2,ythick=2,xthick=2,$
           position=[0.55,0.30,0.97,0.45],/norm,/noer,title='U/I light curve'
     oplot,nbpolcal[*,2,1],lines=1,thick=2,color=120;[0.55,0.05,0.74,0.45]
     xline,[28,56,84]
     plot,nbpolcal[*,3,0],/xs,/ys,thick=2,ythick=2,xthick=2,$
           position=[0.55,0.05,0.97,0.2],/norm,/noer,title='V/I light curve'
     oplot,nbpolcal[*,3,1],lines=1,thick=2,color=120
     xline,[28,56,84]
 
     !p.charsize=1.0 &  device,/close
     set_plot,'x'
     loadct,0


newtext = strarr(10) ;new entries for log
tstr = ''

     newtext[0] = 'ibis_get_polcalcurve.pro for wavelength: '+$
               string(lambda,format='(I4)')+', Fig.~\ref{pcreg'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'} \\'
     newtext[1] = 'savefile: '+savefile
     newtext[2] = '\begin{figure}[htb]'
     newtext[3] = '\begin{center}'
     newtext[4] = '\includegraphics[width=0.9\textwidth]{pcreg'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'}'
     newtext[5] = '\caption{\textit{Left}: Area used for polcal averaging. \textit{Right}: Light level and polarized light curves (left beam=black, right beam=red). '+$
             string(lambda,format='(I4)')+'}'
     newtext[6] = '\label{pcreg'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
     newtext[7] = '\end{center}'
     newtext[8] = '\end{figure}'
     newtext[9] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext

log_write,path1,newtext

end
