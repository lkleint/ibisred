PRO ibis_adapt_polstates,nbdwc,nbout,s_file,info_nb
;balance intensities of both beams separately
;### many things are hardwired!!!

sz = size(nbdwc)
if sz[3] mod 6 eq 0 then npol=6 else npol=5 ;for h-alpha
beams = sz[4]

srt = bsort(info_nb.wave[0:*:npol])
nbout = reform(nbdwc,sz[1],sz[2],npol,sz[3]/npol,beams)

;mask = ibis_mask(nbout[*,*,0,0,0],cut=5)
;indL = where(mask[*,*] eq 1)
;mask2 = ibis_mask(nbout[*,*,0,0,1],cut=5)
;indR = where(mask2[*,*] eq 1)

;ipxL = array_indices(mask[0:sz[1]/2.-1,*],indL)
;ipxR = array_indices(mask2[0:sz[1]/2.-1,*],indR)

;get center of masks -NO, select QS
;cogxL = round(avg(ipxL[0,*]) )
;cogxR = roundavg(ipxR[0,*])+sz[1]/2. )
;cogy = roundavg(ipxL[1,*]) )
;rgx = roundstddev(ipxL[0,*]))
;rgy = roundstddev(ipxL[1,*]))

;QS
x1=250
x2=300
y1=450
y2=550

wl1=1  ;wavelengths to average over, take continuum
wl2=2

int = fltarr(npol,beams)


FOR i=0,npol-1 do begin  ;for all polstates
   ;take wavelengths 1&2
   FOR bb=0,beams-1 do begin
  ;   int = avg(nbdwc[cogxL-rgx:cogxL+rgx,cogy-rgy:cogy+rgy,i
      int[i,bb] = avg(nbout[x1:x2,y1:y2,i,[srt[wl1],srt[wl2]],bb])
      fac = int[i,bb] / int[0,bb]
      nbout[*,*,i,*,bb] = nbout[*,*,i,*,bb]/fac
   ENDFOR
ENDFOR

srt = bsort(info_nb.wave)
tmp =reform(nbout,sz[1],sz[2],sz[3],sz[4])
plot,avg(avg(tmp[x1:x2,y1:y2,srt,0],0),0),title=s_file,psym=-4,/xs
a=tvrd()
tm = strcompress(systime(),/rem)
if ~file_test('analysis/') then spawn,'mkdir analysis'
write_jpeg,'analysis/'+tm+'.jpg',bytscl(a)

nbout=tmp
END
