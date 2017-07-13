pro log_add_wldcff,path1,wlffile,wldfile
;adds variables that were set to log file
time=systime()

;check how many darks already done:
files = file_search(path1+'/log/wldark*.eps',count=nfiles)

restore,wldfile
set_plot,'PS'
fdark = path1+'/log/wldark'+string(nfiles,format='(I1)')+'.eps'
device,filename=fdark
device,/encaps,bits_per_pixel=16,xsize=5,ysize=5
tvscl,dark,xsize=5,/cent
device,/close
set_plot,'x'

restore,wlffile
set_plot,'PS'
fflat = path1+'/log/wlflat'+string(nfiles,format='(I1)')+'.eps'
device,filename=fflat
ll=avg(flat)
tvscl,flat>ll,xsize=5,/cent
device,/close
set_plot,'x'


newtext = strarr(12) ;new entries for log

newtext[0] = '\textbf{WL dark and flat: ibis_wl_avgdcff.pro}\\'
newtext[1] = string(time)+'\\'
newtext[2] = 'WL dark. Min: \textit{'+string(min(dark))+$
               '}, Max: \textit{'+string(max(dark))+'}\\'
newtext[3] = 'WL flat. Min: \textit{'+string(min(flat))+$
               '}, Max: \textit{'+string(max(flat))+'}\\'
newtext[4] = '\begin{figure}[htb]'
newtext[5] = '\begin{center}'
newtext[6] = '\includegraphics[width=5cm]{wldark'+string(nfiles,format='(I1)')+'}'
newtext[7] = '\includegraphics[width=5cm]{wlflat'+string(nfiles,format='(I1)')+'}'
newtext[8] = '\caption{WL dark and flat}'
newtext[9] = '\end{center}'
newtext[10] = '\end{figure}'
newtext[11] = ' '


;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


print,'writing variables into log'
log_write,path1,newtext

end
