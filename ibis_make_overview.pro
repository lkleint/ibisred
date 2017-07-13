;create overview files of reduced data for quicklook
;13.12.13 LK
;run from main IBIS directory (reduc/20110924/)


files = file_search('*/*pc.tatb.sav',count=nf)

if file_test('analysis/') ne 1 then spawn,'mkdir analysis'

;find number of subdirectories and wavelengths
;assuming that files are structured as results_xxx/wl_....sav
wl = strarr(nf)
res = strarr(nf)

for i=0,nf-1 do begin
wl[i] = strmid(files[i],21,4,/rev)
res[i] = strmid(files[i],0,11)  ;### change for newer reduction!
endfor

print,'---------------------------------------------------------------------------------------'
print,'result directories with data: ',diffelement(res,/names)
print,'wavelengths reduced:',diffelement(wl,/names)
print,'---------------------------------------------------------------------------------------'


nwl = diffelement(wl)
wlobs = diffelement(wl,/names)
ndir = diffelement(res)
dirs = diffelement(res,/names)

;---- create overview documents in analysis subdirectory ---
for ww=0.,nwl-1,1. do begin  ;for all wavelengths;
;for ww=1.,1. do begin  ;for all wavelengths
print,'Wavelength: ',ww,' of ',nwl-1
print,'---------------------------------------------------------------------------------------'
nextres:
   for kk=0.,ndir-1 do begin ;for all results_kk

      tmpa = where(strpos(files,dirs[kk]) ne -1)
      tmpb = where(strpos(files[tmpa],wlobs[ww]) ne -1)
      ;files[tmp[tmp2]] are the files of current wl and directory
      if tmpb[0] eq -1 then goto,nextres

      FOR ll=0,n_elements(tmpb)-1 do begin
         restore,files[tmpa[tmpb[ll]]],/ve
         sz=size(stokesout)
         nmod = diffelement(nb_expos.stokes)<6

         col = 5  ;5 images per row
         row = (sz[3]/2+3)/col+1
         dx = 10;space between images
         dy = 50 ;vertical space between images
 
         ;make jpeg, not eps because of file size
         set_plot,'z'
         xpaper = col*sz[1]+dx*(col+1)
         ypaper = row*sz[2]+dy*(row+2)
         device,set_resolution=[xpaper,ypaper],decomposed=0,set_pixel_depth=24
         !p.background=255 & !p.color=0
         imgx =sz[1]             ;size of images
         imgy = sz[2]
         !p.charsize=1.2
         startx = 10
         posx = startx & posy=ypaper - sz[2] - dy*2
         ;ypaper-1.1*imgx*sz[2]/float(sz[1]) ;starting location of tvscl image

         th = 2 ;thickness
;plot stokes I (every other image)
         for j=0,sz[3]/2-1 do begin
            plot_image,stokesout[*,*,j*2,0],position=[posx,posy,posx+imgx,posy+imgy],xthick=th,ythick=th,thick=th,charthick=th,$
                       title=nb_expos.date_obs[j*2*nmod],/noer,/dev
            xyouts,posx+imgx/2.,posy+dy,strcompress(string(nb_expos.wave[j*2*nmod],format='(F7.2)'),/remove),charthick=2,/dev,charsize=3,align=.5
        ;      print,posx,posy
            posx = posx + imgx + dx
            if posx gt xpaper-imgx then begin
               posx = startx
               posy = posy-sz[2] - dy
            endif
         endfor

;plot stokes Q
         test = fltarr(sz[3])
         for jj=0,sz[3]-1 do test[jj] = total(abs(stokesout[*,*,jj,1]/stokesout[*,*,jj,0]))
         tmp = where(max(test) eq test)
         plot_image,stokesout[*,*,tmp,1]/stokesout[*,*,tmp,0],position=[posx,posy,posx+imgx,posy+imgy],xthick=th,ythick=th,thick=th,$
                    charthick=th,title=nb_expos.date_obs[tmp*nmod],/noer,/dev
         xyouts,posx+imgx/2.,posy+dy,'Q/I at '+strcompress(string(nb_expos.wave[tmp],format='(F8.3)'),/remove),charthick=2,/dev,charsize=3,align=.5
         posx = posx + imgx+ dx
         if posx gt xpaper-imgx then begin
            posx = startx
            posy = posy-sz[2]-dy
         endif

;plot stokes U
         test = fltarr(sz[3])
         for jj=0,sz[3]-1 do test[jj] = total(abs(stokesout[*,*,jj,2]/stokesout[*,*,jj,0]))
         tmp = where(max(test) eq test)
         plot_image,stokesout[*,*,tmp,2]/stokesout[*,*,tmp,0],position=[posx,posy,posx+imgx,posy+imgy],$
                    title=nb_expos.date_obs[tmp*nmod],xthick=th,ythick=th,thick=th,charthick=th,/noer,/dev
         xyouts,posx+imgx/2.,posy+dy,'U/I at '+strcompress(string(nb_expos.wave[tmp*nmod],format='(F8.3)'),/remove),charthick=2,/dev,charsize=3,align=.5
         posx = posx + imgx+ dx
         if posx gt xpaper-imgx then begin
            posx = startx
            posy = posy-sz[2]-dy
         endif

;plot stokes V
         test = fltarr(sz[3])
         for jj=0,sz[3]-1 do test[jj] = total(abs(stokesout[*,*,jj,3]/stokesout[*,*,jj,0]))
         tmp = where(max(test) eq test)
         plot_image,stokesout[*,*,tmp,3]/stokesout[*,*,tmp,0],position=[posx,posy,posx+imgx,posy+imgy],xthick=th,ythick=th,thick=th,$
                    charthick=th,title=nb_expos.date_obs[tmp*nmod],/noer,/dev
         xyouts,posx+imgx/2.,posy+dy,'V/I at '+strcompress(string(nb_expos.wave[tmp*nmod],format='(F8.3)'),/remove),charthick=2,/dev,charsize=3,align=.5
         posx = posx + imgx+ dx
         if posx gt xpaper-imgx then begin
            posx = startx
            posy = posy-float(sz[2])-dy
         endif

         plot_image,simage,position=[posx,posy,posx+imgx,posy+imgy],xthick=th,ythick=th,thick=th,charthick=th,/noer,/dev
         xyouts,xpaper/2.,ypaper-50,wlobs[ww]+'   '+nb_expos.date_obs[0]+', '+files[tmpa[tmpb[ll]]],align=.5,charsize=3.,/dev,charthick=2

         a=tvrd()
         write_jpeg,'./analysis/overview_'+dirs[kk]+'_nb'+string(ll,format='(I03)')+'_'+wlobs[ww]+'.jpg',a,quality=100
         erase

      ENDFOR


   endfor


endfor



set_plot,'x'

;spawn,'open ./analysis/overview6302_'+string(i,format='(I02)')+'.eps'

END
