;each step of the ibis data reduction is saved in a log file (tex)
;which can be compiled to make a pdf.

;this program tests if the .tex already exists and if not, it creates
;its structure

;lk 27.4.11
pro log_create,path1,overwrite=overwrite

close,/all
if file_test(path1+'/log/log_datared.tex') and not keyword_set(overwrite) then begin
print,'log file already present in /log/'
return
endif

;create log in separate directory
spawn,'mkdir '+path1+'/log/'

openw,2,path1+'/log/log_datared.tex'

printf,2,'\documentclass[dvips,12pt]{article}'
printf,2,'\title{IBIS reduction log}'
printf,2,'\author{ibis\_v5}'
printf,2,'\date{\today}'
printf,2,'\usepackage{graphicx}'
printf,2,'\usepackage[english]{babel}'
printf,2,'\usepackage[margin=0.8in]{geometry}'
printf,2,'\usepackage{pslatex}'
printf,2,'\setlength\parindent{0pt}'
printf,2,' '
printf,2,'\begin{document}'
printf,2,'\maketitle'
printf,2,' '
printf,2,'\section{Executed commands}'
printf,2,' '
printf,2,'\end{document}'
  
close,2

print,'started log file in log/log_datared.tex'

end
