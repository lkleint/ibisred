;workaround to fix gain issues at telluric line
;LK June 2016

restore,'6302_gain.sav',/ve

sz=size(wcgain)
lpt = 19  ;wavelength index on left that still looks ok
rpt = 22  ;wavelength index on right that still looks ok
;anything in between will be interpolated linearly

wl = diffelement(info_flat_short.wave,/double,/names)

badind = (findgen(sz[3]))[lpt+1:rpt-1]
goodind = rem_elem(findgen(sz[3]),badind)

;tmp = interpol(wcgain[300,300,goodind,0],wl[goodind],wl)
wcgainip = wcgain

FOR i=0,sz[1]-1 do begin
   if i mod 50 eq 0 then print,i,' of ',sz[1]-1
  FOR j=0,sz[2]-1 do begin
     FOR k=0,sz[4]-1 do begin
        ip = interpol(wcgain[i,j,goodind,k],wl[goodind],wl)
        wcgainip[i,j,*,k] = ip
     ENDFOR
  ENDFOR
ENDFOR

wcgain = wcgainip
save,wcgain,info_flat_short,filename='6302_gain.sav',/ve
