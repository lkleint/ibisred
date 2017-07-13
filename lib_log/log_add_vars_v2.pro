pro log_add_vars_v2,path1,dt,oripath,wpath,polpath,la,flagncam
;adds variables that were set to log file
time=systime()

newtext = strarr(10) ;11 new entries for log


newtext[0] = '\textbf{variables set in ibis_variables.txt}\\'
newtext[1] = string(time)+'\\'
newtext[2] = 'Observing date: \textit{'+dt+'}\\'
newtext[3] = 'Path to original data: \textit{ '+oripath+'}\\'
newtext[4] = 'Path to save data: \textit{'+path1+'}\\'
newtext[5] = 'Path to original wl data: \textit{ '+wpath+'}\\'
newtext[6] = 'Path to polcal (if taken on diff date): \textit{'+polpath+'}\\'

tstr=''
for i=0,n_elements(la)-1 do tstr=tstr+la[i]+' '
newtext[7] = 'selected wavelengths: \textit{ '+tstr+'}\\'

newtext[8] = 'Camera (0=old,1=new): \textit{ '+string(flagncam)+'}\\'
newtext[9] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


print,'writing variables into log'
log_write,path1,newtext

end
