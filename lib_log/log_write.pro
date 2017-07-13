;open the log file and add things before \end{document}
pro log_write,path1,newtext

nlines = file_lines(path1+'/log/log_datared.tex')
openr,2,path1+'/log/log_datared.tex'

;----- get current content of file --------
content = strarr(nlines)
txt=''
for i=0,nlines-1 do begin
readf,2,txt
content[i] = txt
endfor
close,2

;---- find \end{document}
tmp = where(strmid(content,0,14) eq '\end{document}')

;---- add more things to file ----
openw,2,path1+'/log/log_datared.tex'

for i=0,tmp[0]-1 do begin
printf,2,content[i]
endfor

;add more stuff
for i=0,n_elements(newtext)-1 do begin
printf,2,newtext[i]
endfor

;---- close file -------
printf,2,content[tmp]
close,2


end
