pro log_add_autocal,path1,tstamps
;adds variables that were set to log file
time=systime()

newtext = strarr(4) ;new entries for log
tstr = ''

newtext[0] = '\textbf{Automatic polcal initialized: collect_ibis_polcal.pro}\\'
newtext[1] = string(time)+'\\'
for i=0,n_elements(tstamps)-1 do tstr=tstr+tstamps[i]+' '
newtext[2] = 'Timestamps (330/15/60/105): \textit{'+tstr+'}\\'
newtext[3] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


log_write,path1,newtext

end
