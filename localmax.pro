FUNCTION localmax,xvec,yvec,show=show
;finds all local maxima in a plot (xvec,yvec) 
;useful for finding the location of fabry perot transmission peaks
;works up to 250 maxima so far, if you want more, change the number in
;the structure!
;/show prints the maxima

sz = size(yvec)
ydiff =  yvec[1:sz[1]-1] - yvec[0:sz[1]-2] 

;all the places where ydiff changes its sign to a negative number are
;places of local maxima

;make a structure to store these places
locmax = {xpos:dblarr(250),ypos:dblarr(250)}
counter = 0

;go through ydiff and find maxima
for i=0,sz[1]-3 do begin
if ydiff[i] gt 0 and ydiff[i+1] lt 0 then begin
locmax.xpos[counter] = xvec[i+1]
locmax.ypos[counter] = yvec[i+1]
counter = counter+1
endif 
endfor

;shorten locmax to actual length
temp = {xpos:dblarr(counter),ypos:dblarr(counter)}
temp.xpos = locmax.xpos[0:counter-1]
temp.ypos = locmax.ypos[0:counter-1]
locmax = temp

;print the maxima?
if keyword_set(show) then print,locmax.xpos

return,locmax

END
