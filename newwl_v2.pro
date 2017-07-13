function newwl_v2,img,f1,f2,r,sfl,rotindex,off,wl_lcut,wl=wl,nb=nb
;28.2.11 modified LK
;mod 3.1.14: alignment done for NB to match wl
;mod 13.4.2017: for single beam, off is 0. and not [x,y], use this to
;determine single beam alignment

;------------------------- DUAL BEAM ----------------------------------

if n_elements(off) eq 2 then begin ;DUAL beam

   wr=float(img)
   s = size(wr)
;factor = round(s[1]/500.)  ;alignment is done on 500x500, correct for factor here
;3.1.14: alignment done on original size now

;wl images: rotate and cut
   if keyword_set(wl) then begin
      tmp = rotate(wr,rotindex)
;  tmp2 = tmp[wl_lcut*factor:wl_lcut*factor+s[1]/2-1,*]
      tmp2 = tmp[wl_lcut:wl_lcut+s[1]/2-1,*]
      return,tmp2
   endif

;nb images: get L and R beam
   if keyword_set(nb) then begin
      tmp = congrid(wr,s[1]*f1,s[2]*f2,cubic=-0.5)
      tmp2 = rot(tmp,-r,cubic=-0.5)
      tmp2 = ibis_set(tmp2,s[1],s[2])
      
      lbeam   = interpolate(tmp2[0:s[1]/2-1,0:s[2]-1], findgen(s[1]/2)-sfl[0], findgen(s[2])-sfl[1], $
                            /GRID, CUBIC=-0.5)
      rbeam = ibis_set(interpolate(tmp2[s[1]/2:*,*],findgen(s[1]/2)-off[0]-sfl[0], findgen(s[2])-off[1]-sfl[1], /GRID, CUBIC=-0.5),s[1]/2,s[2])
      arr=[[[lbeam]],[[rbeam]]]
      return,arr
   endif

endif else begin
;------------------------- SINGLE BEAM ----------------------------------
   wr=float(img)
   s = size(wr)

;wl images: rotate and cut
   if keyword_set(wl) then begin
      tmp = rotate(wr,rotindex)
      tmp2 = tmp[wl_lcut:s[1]-1,*] ;wl_lcut is zero for single beam
      return,tmp2
   endif

;nb images:
   if keyword_set(nb) then begin
      tmp = congrid(wr,s[1]*f1,s[2]*f2,cubic=-0.5)
      tmp2 = rot(tmp,-r,cubic=-0.5)
      tmp2 = ibis_set(tmp2,s[1],s[2])
      
     ; lbeam   = interpolate(tmp2[0:s[1]/2-1,0:s[2]-1], findgen(s[1]/2)-sfl[0], findgen(s[2])-sfl[1], $
     ;                       /GRID, CUBIC=-0.5)
     ; rbeam = ibis_set(interpolate(tmp2[s[1]/2:*,*],findgen(s[1]/2)-off[0]-sfl[0], findgen(s[2])-off[1]-sfl[1], /GRID, CUBIC=-0.5),s[1]/2,s[2])
     ; arr=[[[lbeam]],[[rbeam]]]
      return,tmp2
   endif



endelse


return,tmp
end
