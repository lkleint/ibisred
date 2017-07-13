PRO YLINE,yvalue,_extra=extra
;draws a horizontal line with y=yvalue
;keywords are accepted, example: yline,50,lines=2
;LK 8.8.07
on_error,2
;message,'usage: yline,yvalue,[keywords];  purpose: draws a horizontal line at yvalue'
if n_elements(yvalue) lt 1 then print,'YLINE usage: yline,yvalue,[keywords];  purpose: draws a horizontal line at yvalue'
nlines = n_elements(yvalue)
for i=0,nlines-1 do begin
xlo=!x.crange[0]
xhi=!x.crange[1]

oplot,[xlo,xhi],[yvalue[i],yvalue[i]],_extra=extra
endfor
END
