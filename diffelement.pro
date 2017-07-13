FUNCTION DIFFELEMENT,array,names=names,double=double
;gives back the value of different elements in the array
;24.9.07 LK
;9.2.11 added name keyword to get the value of the different elements
;10.10.11 added keyword double, use it only in case you are sure that array
;is not string array. will return double value, not string as usual

on_error,2
array=reform(array)
dims=size(array)

if dims[0] ne 1 then MESSAGE,'array is not 1-dimensional'

sarray = array[sort(array)] ;sort the array
counter=1

;--- go through array and count different elements ---
for i=0,dims[1]-2 do begin
if sarray[i] ne sarray[i+1] then counter=counter+1 
;if two consecutive elements are not equal then set the counter to c+1
endfor

;--- save different elements in variable 'names' ---
if keyword_set(names) then begin
ctr = 0
names=strarr(counter) ;use string array as it always works
if keyword_set(double) then names=dblarr(counter)
for i=0,dims[1]-2 do begin
if sarray[i] ne sarray[i+1] then begin
if not keyword_set(double) then names[ctr] = string(sarray[i]) else $
      names[ctr] = double(sarray[i])
ctr=ctr+1
endif
endfor
names[counter-1]=sarray[i]
endif

if not keyword_set(names) then return,counter else return,names

END
