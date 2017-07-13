pro log_add_align,path1,dir
;adds variables that were set to log file
time=systime()

newtext = strarr(3) ;new entries for log

newtext[0] = '\textbf{Alignment: align_ibis.pro}\\'
newtext[1] = string(time)+'\\'
newtext[2] = 'chosen grid file: '+dir+' \\'
;no space in the end because wl follow directly from align_ibis

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext

;print,'writing variables into log'
log_write,path1,newtext

end
