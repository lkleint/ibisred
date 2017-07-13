pro log_add_paths,path1,wdfile,wffile,dcdir,ffdir,datadir,griddir,tardir
;adds variables that were set to log file
time=systime()

newtext = strarr(10) ;new entries for log
tstr = ''

newtext[0] = '\textbf{paths initialized}\\'
newtext[1] = string(time)+'\\'
for i=0,n_elements(wdfile)-1 do tstr=tstr+wdfile[i]+' '
newtext[2] = 'WL dark file: \textit{'+tstr+'}\\'
tstr = ''
for i=0,n_elements(wffile)-1 do tstr=tstr+wffile[i]+' '
newtext[3] = 'WL flat file: \textit{ '+tstr+'}\\'
tstr = ''
for i=0,n_elements(dcdir)-1 do tstr=tstr+dcdir[i]+' '
newtext[4] = 'Darks: \textit{'+tstr+'}\\'
tstr = ''
for i=0,n_elements(ffdir)-1 do tstr=tstr+ffdir[i]+' '
newtext[5] = 'Flats: \textit{ '+tstr+'}\\'
tstr = ''
for i=0,n_elements(datadir)-1 do tstr=tstr+datadir[i]+' '
newtext[6] = 'Data: \textit{ '+tstr+'}\\'
tstr = ''
for i=0,n_elements(griddir)-1 do tstr=tstr+griddir[i]+' '
newtext[7] = 'Grids: \textit{'+tstr+'}\\'
tstr = ''
for i=0,n_elements(tardir)-1 do tstr=tstr+tardir[i]+' '
newtext[8] = 'Targets: \textit{'+tstr+'}\\'
newtext[9] = ' '

;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


print,'writing variables into log'
log_write,path1,newtext

end
