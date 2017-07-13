;LK 20.7.2016
;correct for intensity gradients in polarized continuum images
;
;restores files, surface fits continuum (of first wavelength point or cind)
;and subtracts this from all data. Saves new file with "polcont" in filename.
;The edges of the IBIS FOV are currently hardwired below.
;This routine was applied to 6302 data for X1 2014-03-29

;cind = continuum index (index 0 by default)

pro ibis_corr_cont,files,cind=cind

   if ~file_test(files[0]) then message,'files not found. USAGE: ibis_corr_cont,files'
   nfiles = n_elements(files)
   if ~keyword_set(cind) then cind=0

;---- IBIS FOV ----
   x1 = 4  ;left edge of FOV
   x2 = 5  ;pixels to subtract from right edge
   y1 = 50 ;bottom pixels to cut
   y2 = 40 ;top pixels to cut

FOR i=0,nfiles-1 do begin

   restore,files[i],/ve
   sz = size(stokesout)

   ;surface fitting, use 3rd degree
   qfit = sfit(stokesout[x1:sz[1]-x2,y1:sz[2]-y2,cind,1],3)
   ufit = sfit(stokesout[x1:sz[1]-x2,y1:sz[2]-y2,cind,2],3)
   vfit = sfit(stokesout[x1:sz[1]-x2,y1:sz[2]-y2,cind,3],3)

   ;variable that contains new FOV edges wrt to old array
   cut = [x1,sz[1]-x2,y1,sz[2]-y2]

;anim_lk,reform(stokesout[4:sz[1]-5,50:sz[2]-40,*,1]-cmreplicate(qfit,sz[3]))
;anim_lk,reform(stokesout[4:sz[1]-5,50:sz[2]-40,*,2]-cmreplicate(ufit,sz[3]))
;anim_lk,reform(stokesout[4:sz[1]-5,50:sz[2]-40,*,3]-cmreplicate(vfit,sz[3]))

    ;make new array and save data
    stokesoutold = stokesout
    stokesout = fltarr(cut[1]-cut[0]+1,cut[3]-cut[2]+1,sz[3],4)
    stokesout[*,*,*,0] = reform(stokesoutold[x1:sz[1]-x2,y1:sz[2]-y2,*,0])
    stokesout[*,*,*,1] = reform(stokesoutold[x1:sz[1]-x2,y1:sz[2]-y2,*,1]-cmreplicate(qfit,sz[3]))
    stokesout[*,*,*,2] = reform(stokesoutold[x1:sz[1]-x2,y1:sz[2]-y2,*,2]-cmreplicate(ufit,sz[3]))
    stokesout[*,*,*,3] = reform(stokesoutold[x1:sz[1]-x2,y1:sz[2]-y2,*,3]-cmreplicate(vfit,sz[3]))

    simage = simage[x1:sz[1]-x2,y1:sz[2]-y2]

    fname = strmid(files[i],strlen(files[i]),11,/rev)+'pc.polcont.sav'
    info='Polarized continuum subtracted from Q,U,V. cut gives coordinates wrt default reduc.'

    ;not saving mwld and TP to save space
    print,'Saving: ',fname
    save,stokesout,cut,simage,wl_obs,nb_expos,info,filename=fname

ENDFOR
 



end
