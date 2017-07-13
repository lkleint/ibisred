;converts 2011-10-13T16:18:47.228 into seconds taking into account
;hour, min and s
;input can be array
;lk, 6 dec 2011


function ibis_date_to_s,date

;--- test if correct input format
if strmid(date[0],10,1) ne 'T' then begin
 print,'wrong input format'
 return,-1
endif

nr = n_elements(date)
allsec = dblarr(nr)

for i=0,nr-1 do begin
;--- separate entries
year = strmid(date[i],0,4)
month = strmid(date[i],5,2)
day = strmid(date[i],8,2)
hour = strmid(date[i],11,2)
min = strmid(date[i],14,2)
sec = strmid(date[i],17,strlen(date[i])-17) ;because msec are variable in length


;--- get seconds (double format)
allsec[i] = double(sec) + double(min)*60 + double(hour)*3600.
endfor


return,allsec
end
