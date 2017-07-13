pro log_add_qscont,path1,fov,wlimg,avgprof,cregion,lambda,scan_file


;create eps of chosen quiet sun region

     files = file_search(path1+$
    '/log/qsreg'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles)
 
     set_plot,'PS'
      fblue = path1+'/log/qsreg'+string(lambda, FORMAT='(I4)')+$
           string(nfiles,format='(I02)')+'.eps'
     device,filename=fblue
     xpaper = 10 & ypaper=7
     device,/encaps,bits_per_pixel=16,xsize=xpaper,ysize=ypaper,/color
     !p.charsize=0.7
     sz = size(wlimg)
     yimg = ypaper & ximg = yimg/(float(sz[2])/sz[1])
     tvscl,wlimg,ysize=yimg,/cent
     loadct,3
     plots,[fov[0],fov[2],fov[2],fov[0],fov[0]]/sz[1]*ximg/xpaper,$
           [fov[1],fov[1],fov[3],fov[3],fov[1]]/sz[2]*yimg/ypaper,color=120,thick=2,/norm
 
     plot,avgprof,/xs,/ys,thick=2,ythick=2,xthick=2,yrange=[0,max(avgprof)*1.1],$
           position=[0.43,0.15,0.93,0.7],/norm,/noer,title='average profile and chosen continuum'
     xline,cregion,thick=3,color=120
        
     !p.charsize=1.0 &  device,/close
     set_plot,'x'
     loadct,0


newtext = strarr(12+n_elements(scan_file)) ;new entries for log
tstr = ''

     newtext[0] = 'calibration for wavelength: '+$
               string(lambda,format='(I4)')+', Fig.~\ref{qsreg'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'} \\'
     newtext[1] = ''
     newtext[2] = '\begin{figure}[htb]'
     newtext[3] = '\begin{center}'
     newtext[4] = '\includegraphics[width=0.5\textwidth]{qsreg'+$
             string(lambda,format='(I4)')+$
             string(nfiles,format='(I02)')+'}'
     newtext[5] = '\caption{\textit{Left}: Chosen quiet Sun. \textit{Right}: Chosen continuum, '+$
             string(lambda,format='(I4)')+'}'
     newtext[6] = '\label{qsreg'+string(lambda,format='(I4)')+string(nfiles,format='(I02)')+'}'
     newtext[7] = '\end{center}'
     newtext[8] = '\end{figure}'
     newtext[9] = ' '
     newtext[10] = 'Calibrated files: \\'
     newtext[11] = scan_file+' \\' 
     newtext[n_elements(newtext)-1] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


log_write,path1,newtext

end
