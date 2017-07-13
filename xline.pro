PRO XLINE,xvalue,_extra=extra
;draws a vertical line with x=xvalue
;keywords are accepted, example: xline,50,lines=2
;LK 8.8.07
on_error,2
;message,'usage: xline,xvalue,[keywords];  purpose: draws a vertical line at xvalue'

if n_elements(xvalue) lt 1 then print,'XLINE usage: xline,xvalue,[keywords];  purpose: draws a vertical line at xvalue'

nlines = n_elements(xvalue)
for i=0,nlines-1 do begin
ylo=!y.crange[0]
yhi=!y.crange[1]

oplot,[xvalue[i],xvalue[i]],[ylo,yhi],_extra=extra
endfor

END
