pro log_add_wldb,path1,wldbfile,tpdbfile
;adds variables that were set to log file
time=systime()

newtext = strarr(5) ;new entries for log

newtext[0] = '\textbf{ibis_wl_db_new.pro}\\'
newtext[1] = string(time)+'\\'
newtext[2] = 'WL db file: \textit{'+wldbfile+'}\\'
newtext[3] = 'WL telescope file: \textit{'+tpdbfile+'}\\'
newtext[4] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


print,'writing variables into log'
log_write,path1,newtext

end
