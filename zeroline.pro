PRO ZEROLINE,_extra=extra
;draws a line with y=0
;keywords are accepted, example zeroline,lines=2
;LK 8.8.07
on_error,2

xlo=!x.crange[0]
xhi=!x.crange[1]

oplot,[xlo,xhi],[0,0],_extra=extra

END
