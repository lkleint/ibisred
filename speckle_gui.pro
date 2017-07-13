;----------------------------------------------------
;        GUI for speckle input
;----------------------------------------------------

;There is a GUI written by Fred, which uses GraphApp
;commands. I couldn't get it to compile on Mac OSX.
;This GUI does exactly the same (creating the 3 files init_file.dat,
;init_method.dat and init_props.dat), but has additional options to
;display the data.

;v1, LK, Apr 22, 2013
;mod ASD, Feb 20, 2014 -> better display options
;LK: mpicommand now changeable by user

;IDL> speckle_gui
;### todo: update phase and niter in tab3/4 when changed

;this procedure controls the writing of the dat files and updating variables
;PRO Draw_Resize_TLB_Resize, event
;   Widget_Control, event.top, Get_UValue=info, /No_Copy
;   Widget_Control, info.drawID, Draw_XSize=event.x, Draw_YSize=event.y
;   WSet, info.wid
;   TVLCT, info.r, info.g, info.b
; ;  plot_image,simage
;   Widget_Control, event.top, Set_UValue=info, /No_Copy
;END

pro speckle_gui_event,ev
widget_control, ev.top, get_uvalue=arr
widget_control, ev.id, get_uvalue=nr
common simg, currfile

if (nr eq 'arr.aoflag' || nr eq 'arr.aostf' || nr eq 'arr.aolock' || $
    nr eq 'arr.phase' || nr eq 'arr.wiener' || nr eq 'arr.lowpass') then $
   widget_control, ev.id, get_value=test

case nr of
;tab1
 'arr.inputpath':  *arr.inputpath = ev.value
 'arr.startnum':   *arr.startnum = ev.value
 'arr.endnum':     *arr.endnum = ev.value
 'arr.savepath':   *arr.savepath = ev.value
 'arr.noisefile':  *arr.noisefile = ev.value

;tab2
 'arr.ximg':       *arr.ximg = ev.value
 'arr.yimg':       *arr.yimg = ev.value
 'arr.nimgburst':  *arr.nimgburst = ev.value
 'arr.hdroffset':  *arr.hdroffset = ev.value
 'arr.xarcsecpx':  *arr.xarcsecpx = ev.value
 'arr.yarcsecpx':  *arr.yarcsecpx = ev.value
 'arr.dtel':       *arr.dtel = ev.value
 'arr.obswl':      *arr.obswl = ev.value
 'arr.aoflag':     *arr.aoflag = test      ;yes=0, no=1, for cw_bgroup use test
 'arr.aostf':      *arr.aostf = test
 'arr.aolock':     *arr.aolock = test
 'arr.xlock':      *arr.xlock = ev.value
 'arr.ylock':      *arr.ylock = ev.value

;tab3 and 4
  'arr.subfield':    *arr.subfield = ev.value ;3 and 4
  'arr.subfield2':   *arr.subfield2 = ev.value ;3 and 4
  'arr.phase':       *arr.phase = test
  'arr.fixedrad':    *arr.fixedrad = ev.value
  'arr.niter':       *arr.niter = ev.value ;3 and 4
  'arr.niter2':      *arr.niter2 = ev.value ;3 and 4
  'arr.apod':        *arr.apod = ev.value ;3 and 4
  'arr.apod2':        *arr.apod2 = ev.value ;3 and 4
  'arr.amplimit':    *arr.amplimit = ev.value
  'arr.wiener':      *arr.wiener = test
  'arr.lowpass':     *arr.lowpass = test ;3 and 4
  'arr.phaselimit':  *arr.phaselimit = ev.value
  'arr.uinx':        *arr.uinx = ev.value
  'arr.veclengths':  *arr.veclengths = ev.value
  'arr.weighting':   *arr.weighting = ev.value
  'arr.autosetsnr':  *arr.autosetsnr = ev.value

;tab 5
  'arr.time':        *arr.time = ev.value
  'arr.mpicommand':  *arr.mpicommand = ev.value
  'simage': begin
     loadct,0
     path = strmid(*arr.savepath,0,strpos(*arr.savepath,'/',/reverse_search)+1)
     sFile = DIALOG_PICKFILE(PATH=path, /multiple,$
            TITLE='Select speckle-reconstructed image',  FILTER='*.final')
     n2x = *arr.ximg &      n2y = *arr.yimg
     nsfiles = n_elements(sfile)
     for i=0,nsfiles-1 do begin
       if file_test(sfile[i]) eq -1 then return  ;this may be true if user chooses existing directory
       if strpos(sfile[i],'final') eq -1 then return
       speckle = read_rec(sfile[i], n2x, n2y)
       speckle =  swap_endian(speckle,/swap_if_big_endian)
       index = WHERE( finite(speckle, /NAN) EQ 1, COMPLEMENT = index_valid)
       IF index[0] NE -1 THEN speckle[index] = AVG( speckle[index_valid] )
       sz=size(speckle)
       if sz[1] lt sz[2] then speckle=rotate(speckle,1)
      ; sz=size(speckle)
      ; if sz[1] gt 512 then speckle=congrid(speckle,sz[1]/2,sz[2]/2)
       erase
       plot_image,speckle,min=0
;       tvscl,speckle
       xyouts,.05,.95,sfile[i],/norm
       path = strmid(*arr.savepath,0,strpos(*arr.savepath,'/',/reverse_search)+1)
       allfiles = file_search(path+'*.final',count=nsfiles)
       *arr.indeximage = where(sfile[i] eq allfiles) ;save current file
       wait,*arr.time
     endfor
    end
  
  'previmage' : begin
     loadct,0
     path = strmid(*arr.savepath,0,strpos(*arr.savepath,'/',/reverse_search)+1)
     sFile = file_search(path+'*.final',count=nsfiles)
     n2x = *arr.ximg
     n2y = *arr.yimg
     *arr.indeximage=(*arr.indeximage-1)<(nsfiles-1)>0 
     speckle = read_rec(sfile[(*arr.indeximage)<(nsfiles-1)], n2x, n2y)
     speckle =  swap_endian(speckle,/swap_if_big_endian)
     index = WHERE( finite(speckle, /NAN) EQ 1, COMPLEMENT = index_valid)
     IF index[0] NE -1 THEN speckle[index] = AVG( speckle[index_valid] )
      sz=size(speckle)
     if sz[1] lt sz[2] then speckle=rotate(speckle,1)
     erase
     plot_image,speckle,min=0
     xyouts,.05,.95,sfile[(*arr.indeximage)<(nsfiles-1)],/norm
   end 
    
  'nextimage' : begin
     loadct,0
     path = strmid(*arr.savepath,0,strpos(*arr.savepath,'/',/reverse_search)+1)
     sFile = file_search(path+'*.final',count=nsfiles)
     n2x = *arr.ximg
     n2y = *arr.yimg
     *arr.indeximage=(*arr.indeximage+1)<(nsfiles-1)>0
     speckle = read_rec(sfile[(*arr.indeximage)<(nsfiles-1)], n2x, n2y)
     speckle =  swap_endian(speckle,/swap_if_big_endian)
     index = WHERE( finite(speckle, /NAN) EQ 1, COMPLEMENT = index_valid)
     IF index[0] NE -1 THEN speckle[index] = AVG( speckle[index_valid] )
     sz=size(speckle)
     if sz[1] lt sz[2] then speckle=rotate(speckle,1)
     erase
     plot_image,speckle,min=0
     xyouts,.05,.95,sfile[(*arr.indeximage)<(nsfiles-1)],/norm
   end 
    


 '1': ;do nothing when tab changes

 '50': begin ;save init_file
  openw,2,'init_file.dat'
  printf,2,*arr.inputpath
  printf,2,*arr.startnum
  printf,2,*arr.endnum
  printf,2,*arr.savepath
  printf,2,*arr.noisefile
  printf,2,''
  close,2
  print,'init_file.dat written'
  save,arr,filename='speckle_params.sav'
  end

 '51': begin ;save init_props
  openw,2,'init_props.dat'
  printf,2,*arr.ximg
  printf,2,*arr.yimg
  printf,2,*arr.nimgburst
  printf,2,*arr.hdroffset
  printf,2,*arr.xarcsecpx
  printf,2,*arr.yarcsecpx
  printf,2,*arr.dtel
  printf,2,*arr.obswl
  IF (*arr.aoflag eq 1) then begin ;AO not used
   for jj=0,1 do printf,2,'-1'
   printf,2,'0'
  ENDIF else begin ;AO used
    if *arr.aolock eq 0 then begin  ;auto lock
     for jj=0,1 do printf,2,'-1'
     if *arr.aostf eq 0 then printf,2,'1' else printf,2,'2' ;2=stf checked
    endif else begin ;manual lock coordinates
     printf,2,strcompress(string(*arr.xlock>0<*arr.ximg),/remove) ;automatically correct stupid entries
     printf,2,strcompress(string(*arr.ylock>0<*arr.yimg),/remove)
     if *arr.aostf eq 0 then printf,2,'1' else printf,2,'2' ;2=stf checked
    endelse
  ENDELSE
  close,2
  print,'init_props.dat written'
  save,arr,filename='speckle_params.sav'
  end

 '52': begin ;save init_method, Knox-Thomspon
  openw,2,'init_method.dat'
  printf,2,'0'
  printf,2,*arr.subfield
  if *arr.phase eq 0 then printf,2,*arr.fixedrad else printf,2,'0'
  printf,2,*arr.niter
  printf,2,*arr.apod
  printf,2,*arr.amplimit
  if *arr.wiener eq 0 && *arr.lowpass eq 0 then printf,2,'0'  ;no filter
  if *arr.wiener eq 1 && *arr.lowpass eq 1 then printf,2,'1'  ;both filters
  if *arr.wiener eq 1 && *arr.lowpass eq 0 then printf,2,'2'  ;wiener
  if *arr.wiener eq 0 && *arr.lowpass eq 1 then printf,2,'3'  ;lowpass
  close,2
  print,'init_method.dat written (KT)'
  save,arr,filename='speckle_params.sav'
  end

 '53': begin ;save init_method, triple correl
  openw,2,'init_method.dat'
  printf,2,'1'
  printf,2,*arr.subfield2
  printf,2,*arr.phaselimit
  printf,2,*arr.uinx
  printf,2,*arr.veclengths
  printf,2,*arr.niter2
  printf,2,*arr.autosetsnr
  printf,2,*arr.weighting
  printf,2,*arr.apod2  
  if *arr.wiener eq 0 && *arr.lowpass eq 0 then printf,2,'0'  ;no filter
  if *arr.wiener eq 1 && *arr.lowpass eq 1 then printf,2,'1'  ;both filters
  if *arr.wiener eq 1 && *arr.lowpass eq 0 then printf,2,'2'  ;wiener
  if *arr.wiener eq 0 && *arr.lowpass eq 1 then printf,2,'3'  ;lowpass
  close,2
  print,'init_method.dat written (TC)'
  save,arr,filename='speckle_params.sav'
  end

'90': begin
  print,'Running speckle reconstruction'
  spawn,*arr.mpicommand
  end


'99': begin
     save,arr,filename='speckle_params.sav'
     widget_control,ev.top,/destroy ;close widget
      end

else: print, 'nothing going on yet'

endcase

;help, ev,/str


end


pro speckle_gui,restart=restart, group_leader=groupleader

;---- stuff for modal widget (non-blocking) --
;Catch, theError
;    IF theError NE 0 THEN BEGIN
;       Catch, /Cancel
;       ok = Dialog_Message(!Error_State.Msg)
;       IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
;       cancel = 1
;       RETURN;, 'done' ;modified lk
;    ENDIF

; Provide a group leader if not supplied with one. This
; is required for modal operation of widgets. Set a flag
; for destroying the group leader widget before returning.
 
; IF N_Elements(groupleader) EQ 0 THEN BEGIN
;    groupleader = Widget_Base(Map=0)
;    Widget_Control, groupleader, /Realize
;    destroy_groupleader = 1
; ENDIF ELSE destroy_groupleader = 0


;---- main window definition
scrsz=get_screen_size()
xs=scrsz(0) & ys=scrsz(1)

if (xs ge 700 and ys ge 500) then begin
guiwindow = widget_base(title='Speckle input files, v1',/column,/align_center,$
  x_scroll_size=700,y_scroll_size=500, /scroll);,TLB_Size_Events=1) 
endif else begin
if xs lt 700 then xwind = xs else xwind = 700
if ys lt 500 then ywind = ys else ywind = 500

guiwindow = widget_base(title='Speckle input files, v1',/column,/align_center,$
  x_scroll_size=xwind,y_scroll_size=ywind*.9,/scroll)
endelse
;--- end main window


;create pointers to store values and parameters
inputpath = ptr_new('/path/ibis_wl_20111010.6563')
startnum = ptr_new('000') 
endnum = ptr_new('001') 
savepath = ptr_new('/path/sp.20111010.6563')
noisefile = ptr_new('Filename')
ximg = ptr_new('971')
yimg = ptr_new('420')        ;px
nimgburst = ptr_new('126')   ;n images
hdroffset = ptr_new('0')
xarcsecpx = ptr_new('0.085') ;arcsec
yarcsecpx = ptr_new('0.085') ;arcsec
dtel      = ptr_new('760')     ;mm
obswl     = ptr_new('790')    ;nm
aoflag    = ptr_new(0)
aostf     = ptr_new(0)
aolock    = ptr_new(0)      ;0=auto, 1=manual
xlock     = ptr_new('-1')
ylock     = ptr_new('-1')
subfield  = ptr_new('5')    ;arcsec
subfield2  = ptr_new('5')    ;arcsec
phase     = ptr_new(0)         ;0=fixed, 1=seeing limited
fixedrad  = ptr_new('5')
niter     = ptr_new('30')
niter2     = ptr_new('30')
apod      = ptr_new('30')
apod2      = ptr_new('30')
amplimit  = ptr_new('98')
wiener    = ptr_new(0)
lowpass   = ptr_new(0)
phaselimit= ptr_new('100')
uinx      = ptr_new('10')
veclengths= ptr_new('10')
autosetsnr= ptr_new('80')
weighting = ptr_new('1.2')
time = ptr_new('1.')  ;wait time for speckle animation
indeximage = ptr_new(0) ; Index for speckle images
mpicommand = ptr_new('nohup /opt/local/bin/openmpirun -np 8 /Users/kleintl/sanhome/ibis/kisip/entry')
;on macbook: /usr/local/mpich/bin/mpirun -np 4 -host localhost /Users/kleintl/Data/work/kisip/kisip/entry

arr = {inputpath:inputpath, startnum:startnum, endnum:endnum, savepath:savepath, $
  noisefile:noisefile, ximg:ximg, yimg:yimg, nimgburst:nimgburst, hdroffset:hdroffset, $
  xarcsecpx:xarcsecpx, yarcsecpx:yarcsecpx, dtel:dtel, obswl:obswl, aoflag:aoflag, $
  aostf:aostf, aolock:aolock, xlock:xlock, ylock:ylock, subfield:subfield, phase:phase, $
  fixedrad:fixedrad, niter:niter, apod:apod, amplimit:amplimit, wiener:wiener,  $
  lowpass:lowpass, phaselimit:phaselimit, uinx:uinx, veclengths:veclengths,$
  autosetsnr:autosetsnr, weighting:weighting, subfield2:subfield2, niter2:niter2, $
  apod2:apod2,time:time, indeximage:indeximage, mpicommand:mpicommand $
}

if file_test('speckle_params.sav') then restore,'speckle_params.sav'


;--------- variable section ---------------
 xsizetext = 280.
 xsizefield = 50.
 xsizetext2 = 180.
 xsizeform = 10.
 linelength = 95.
 wind1 = widget_base(guiwindow,/row)

;=================================================================
; TAB 1: init_file
;=================================================================
 tab1 = widget_tab(wind1,location=0,xsize=xwind,uvalue='1')

 tab1base = WIDGET_BASE(tab1, TITLE='init_file.dat', /column) 
 label=widget_label(tab1base,value='This tab specifies the file parameters.',/align_left)

 ;path to speckle files:
 tab1base2 = widget_base(tab1base,/row)
 label = Widget_Label(tab1base2, Value="Path to speckle files (omit .000):",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab1base2, XSize=xsizefield, Value=*arr.inputpath,uvalue='arr.inputpath',/all_events,title='')

 ;start number
 tab1base3 = widget_base(tab1base,/row)
 label = Widget_Label(tab1base3, Value="Start at number:",scr_xsize=xsizetext,/align_left)
 textID3 = cw_field(tab1base3, XSize=xsizeform, Value=*arr.startnum,uvalue='arr.startnum',/all_events,title='')

 ;end number
 tab1base4 = widget_base(tab1base,/row)
 label = Widget_Label(tab1base4, Value="End at number:",scr_xsize=xsizetext,/align_left)
 textID4 = cw_field(tab1base4, XSize=xsizeform, Value=*arr.endnum,uvalue='arr.endnum',/all_events,title='')

 ;path to save files:
 tab1base5 = widget_base(tab1base,/row)
 label = Widget_Label(tab1base5, Value="Save as (omit .000):",scr_xsize=xsizetext,/align_left)
 textID5 = cw_field(tab1base5, XSize=xsizefield, Value=*arr.savepath,uvalue='arr.savepath',/all_events,title='')

 ;path to save files:
 tab1base6 = widget_base(tab1base,/row)
 label = Widget_Label(tab1base6, Value="Noise file (if available):",scr_xsize=xsizetext,/align_left)
 textID6 = cw_field(tab1base6, XSize=xsizefield, Value=*arr.noisefile,uvalue='arr.noisefile',/all_events,title='')

 horizline = widget_base(tab1base,/row)
 tmp = widget_label(horizline,value=strjoin(replicate(' ',linelength)), frame=1,scr_ysize=2)

 ;save button
 tab1base99 = widget_base(tab1base,/row)
 buttont1 = widget_button(tab1base99, value=$
      ColorButtonBitmap(' save init_file.dat',  FGCOLOR='WHITE', BGCOLOR='BLUE'), uvalue='50', scr_xsize=130)
 buttont1b = widget_button(tab1base99, value='    close    ', uvalue='99', scr_xsize=100)

 loadct,0
;=================================================================
; TAB 2
;=================================================================

  tab2base = WIDGET_BASE(tab1, TITLE='init_props.dat', /COLUMN) 
  label=widget_label(tab2base,value='This tab sets the properties of the images and telescope.',/align_left)

 ;x size of image:
 tab2base2 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base2, Value="x-size [px]:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base2, XSize=xsizeform, Value=*arr.ximg,uvalue='arr.ximg',/all_events,title='')

 ;y size of image
 tab2base3 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base3, Value="y-size [px]:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base3, XSize=xsizeform, Value=*arr.yimg,uvalue='arr.yimg',/all_events,title='')

 ;images per burst
 tab2base4 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base4, Value="images per burst:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base4, XSize=xsizeform, Value=*arr.nimgburst,uvalue='arr.nimgburst',/all_events,title='')

;header offset
; tab2base5 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base4, Value="  header offset:",scr_xsize=xsizetext2*.8,/align_left)
 textID2 = cw_field(tab2base4, XSize=xsizeform, Value=*arr.hdroffset,uvalue='arr.hdroffset',/all_events,title='')

;arcsec/px in x
 tab2base6 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base6, Value="arcsec/px in x:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base6, XSize=xsizeform, Value=*arr.xarcsecpx,uvalue='arr.xarcsecpx',/all_events,title='')

;arcsec/px in y
; tab2base7 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base6, Value="  arcsec/px in y:",scr_xsize=xsizetext2*.8,/align_left)
 textID2 = cw_field(tab2base6, XSize=xsizeform, Value=*arr.yarcsecpx,uvalue='arr.yarcsecpx',/all_events,title='')

;telescope diameter
 tab2base8 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base8, Value="telescope diameter [mm]:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base8, XSize=xsizeform, Value=*arr.dtel,uvalue='arr.dtel',/all_events,title='')

;wavelength observed
 tab2base9 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base9, Value="observed wavelength [nm]:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab2base9, XSize=xsizeform, Value=*arr.obswl,uvalue='arr.obswl',/all_events,title='')

;adaptive optics used? 
 tab2base10 = widget_base(tab2base,/row)
 label=widget_label(tab2base10,value='AO used?:',/align_left,scr_xsize=xsizetext2)
 button2c = cw_bgroup(tab2base10,['yes','no'],/exclusive,uvalue='arr.aoflag', /row, set_value=*arr.aoflag,/no_release)
 button4b = cw_bgroup(tab2base10,[' '],/nonexc,uvalue ='arr.aostf', set_value=*arr.aostf,label_left='     Check to use AO specific STFs:')

 ;AO lock
 tab2base11 = widget_base(tab2base,/row)
 label = Widget_Label(tab2base11, Value="AO lock structure location:",scr_xsize=xsizetext2,/align_left)
 button2 = cw_bgroup(tab2base11,['auto','manual [px]:'],/exclusive,uvalue='arr.aolock', /row, $
            set_value=*arr.aolock,/no_release)
 label = Widget_Label(tab2base11, Value="x:",scr_xsize=10,/align_left)
 textID2 = cw_field(tab2base11, XSize=xsizeform, Value=*arr.xlock,uvalue='arr.xlock',/all_events,title='')
 label = Widget_Label(tab2base11, Value="y:",scr_xsize=10,/align_left)
 textID2 = cw_field(tab2base11, XSize=xsizeform, Value=*arr.ylock,uvalue='arr.ylock',/all_events,title='')

 horizline = widget_base(tab2base,/row)
 tmp = widget_label(horizline,value=strjoin(replicate(' ',linelength)), frame=1,scr_ysize=2)

 ;save button
 tab2base99 = widget_base(tab2base,/row)
 buttont2 = widget_button(tab2base99, /align_right, value=$
      ColorButtonBitmap(' save init_props.dat',  FGCOLOR='white', BGCOLOR='blue'), uvalue='51', scr_xsize=130)
 buttont2b = widget_button(tab2base99, value='    close    ', uvalue='99', scr_xsize=100)

 
;=================================================================
; TAB 3
;=================================================================

  tab3base = WIDGET_BASE(tab1, TITLE='Knox-Thompson', /COLUMN) 
  label=widget_label(tab3base,value='Saving this tab overwrites init_method.dat with KT parameters.',/align_left)

;subfield size for reconstruction
 tab3base1 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base1, Value="Subfield size for reconstruction [arcsec]:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab3base1, XSize=xsizeform, Value=*arr.subfield,uvalue='arr.subfield',/all_events,title='')

;phase integration rad
 tab3base2 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base2, Value="Phase integration/iteration radius [px]:",scr_xsize=xsizetext,/align_left)
 button2 = cw_bgroup(tab3base2,['fixed:    ','seeing-limited (careful!)'],/exclusive,uvalue='arr.phase', /row,$
           set_value=*arr.phase,/no_release)

;box for radius limit
 tab3base3 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base3, Value=" ",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab3base3, XSize=xsizeform, Value=*arr.fixedrad,uvalue='arr.fixedrad',/all_events,title='')

;max iter
 tab3base4 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base4, Value="Maximum number of iterations:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab3base4, XSize=xsizeform, Value=*arr.niter,uvalue='arr.niter',/all_events,title='')

;phase rec apodization
 tab3base4 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base4, Value="Phase rec. apodization: [%]",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab3base4, XSize=xsizeform, Value=*arr.apod,uvalue='arr.apod',/all_events,title='')

;amplitude rec. limit
 tab3base5 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base5, Value="Amplitude rec. limit: [% of diffr. limit]",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab3base5, XSize=xsizeform, Value=*arr.amplimit,uvalue='arr.amplimit',/all_events,title='')

;Noise filtering
 tab3base6 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base6, Value="Noise filtering:",scr_xsize=xsizetext,/align_left)
 button44 = cw_bgroup(tab3base6,['Apply Wiener Noise Filter'],/nonexc,uvalue ='arr.wiener', set_value=*arr.wiener,/return_id)

 tab3base7 = widget_base(tab3base,/row)
 label = Widget_Label(tab3base7, Value=" ",scr_xsize=xsizetext,/align_left)
 button45 = cw_bgroup(tab3base7,['Apply Adaptive Low-pass Filter'],/nonexc,uvalue ='arr.lowpass', set_value=*arr.lowpass)


 horizline = widget_base(tab3base,/row)
 tmp = widget_label(horizline, value=strjoin(replicate(' ',linelength)),frame=1,scr_ysize=2)

 ;save button
 tab3base99 = widget_base(tab3base,/row)
 buttont3 = widget_button(tab3base99, /align_right, value=$
      ColorButtonBitmap(' save init_method.dat', FGCOLOR='white', BGCOLOR='blue'), uvalue='52', scr_xsize=140)
 buttont3b = widget_button(tab3base99, value='    close    ', uvalue='99', scr_xsize=100)


;=================================================================
; TAB 4
;=================================================================

  tab4base = WIDGET_BASE(tab1, TITLE='Triple Correlation', /COLUMN) 
  label=widget_label(tab4base,value='Saving this tab overwrites init_method.dat with TC parameters.',/align_left)

;subfield size for reconstruction
 tab4base1 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base1, Value="Subfield size for reconstruction [arcsec]:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base1, XSize=xsizeform, Value=*arr.subfield2,uvalue='arr.subfield2',/all_events,title='')

;title
 tab4base2 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base2, Value="Bispectrum parameters (in % of diffr. limit):",scr_xsize=600,/align_left)

;phase rec
 tab4base3 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base3, Value="Phase rec. limit:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base3, XSize=xsizeform, Value=*arr.phaselimit,uvalue='arr.phaselimit',/all_events,title='')

;u in x-direction
 tab4base4 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base4, Value="u in x-direction:",scr_xsize=xsizetext*.7,/align_left)
 textID2 = cw_field(tab4base4, XSize=xsizeform, Value=*arr.uinx,uvalue='arr.uinx',/all_events,title='')

;|u| etc.
 ;tab4base5 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base4, Value="   |u|, |v| and |u+v|:",scr_xsize=xsizetext2,/align_left)
 textID2 = cw_field(tab4base4, XSize=xsizeform, Value=*arr.veclengths,uvalue='arr.veclengths',/all_events,title='')

;max iter
 tab4base6 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base6, Value="Maximum number of iterations:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base6, XSize=xsizeform, Value=*arr.niter2,uvalue='arr.niter2',/all_events,title='')

;autoset snr
 tab4base7 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base7, Value="Autoset SNR thresh. to keep %age of bispectr.:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base7, XSize=xsizeform, Value=*arr.autosetsnr,uvalue='arr.autosetsnr',/all_events,title='')

;weighting exponent
 tab4base8 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base8, Value="Weighting exponent:",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base8, XSize=xsizeform, Value=*arr.weighting,uvalue='arr.weighting',/all_events,title='')

;phase rec apodization
 tab4base9 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base9, Value="Phase rec. apodization: [%]",scr_xsize=xsizetext,/align_left)
 textID2 = cw_field(tab4base9, XSize=xsizeform, Value=*arr.apod2,uvalue='arr.apod2',/all_events,title='')

;Noise filtering
 tab4base10 = widget_base(tab4base,/row)
 label = Widget_Label(tab4base10, Value="Noise filtering:",scr_xsize=xsizetext*.7,/align_left)
 button44 = cw_bgroup(tab4base10,['Apply Wiener Noise Filter'],/nonexc,uvalue ='arr.wiener', set_value=*arr.wiener)

; tab4base10 = widget_base(tab4base,/row)
; label = Widget_Label(tab4base10, Value=" ",scr_xsize=xsizetext,/align_left)
 button45 = cw_bgroup(tab4base10,['Apply Adaptive Low-pass Filter'],/nonexc,uvalue ='arr.lowpass', set_value=*arr.lowpass)



 horizline = widget_base(tab4base,/row)
 tmp = widget_label(horizline, value=strjoin(replicate(' ',linelength)), frame=1,scr_ysize=2)

 ;save button
 tab4base99 = widget_base(tab4base,/row)
 buttont4 = widget_button(tab4base99, /align_right, value=$
      ColorButtonBitmap(' save init_method.dat', FGCOLOR='white', BGCOLOR='blue'), uvalue='53', scr_xsize=140)

 buttont4b = widget_button(tab4base99, value='    close    ', uvalue='99', scr_xsize=100)


;=================================================================
; TAB 5
;=================================================================

  tab5base = WIDGET_BASE(tab1, TITLE='Preview images', /COLUMN) 
 label=widget_label(tab5base,value='Here you can preview one or more speckle-reconstructed images',/align_left)
  draw = WIDGET_DRAW(tab5base, XSIZE = 680, YSIZE = 340) 


  tab5base1 = widget_base(tab5base,/row)
  label=widget_label(tab5base1,value='Enter time [s] for animation steps:',/align_left)
  textID2 = cw_field(tab5base1, XSize=xsizeform, Value=*arr.time,uvalue='arr.time',/all_events,title='')


  ;run speckle
  tab5base2 = widget_base(tab5base,/row)
  textID3 = cw_field(tab5base2,value=*arr.mpicommand, uvalue='arr.mpicommand',/all_events,title='')
;  label=widget_label(tab5base2,value='$nohup /opt/local/bin/openmpirun -np 8 /Users/kleintl/sanhome/ibis/kisip/entry',/align_left)
  buttont5c = widget_button(tab5base2, value='    RUN speckle    ', uvalue='90', scr_xsize=100)

 ;save button
  tab5base99 = widget_base(tab5base,/row)
  buttont5a = widget_button(tab5base99, /align_right, value=$
      ColorButtonBitmap(' select image', FGCOLOR='white', BGCOLOR='blue'), uvalue='simage')
  buttont5a1 = widget_button(tab5base99, /align_right, value=$
      ColorButtonBitmap(' <- ', FGCOLOR='white', BGCOLOR='blue'), uvalue='previmage')
  buttont5a2 = widget_button(tab5base99, /align_right, value=$
      ColorButtonBitmap(' -> ', FGCOLOR='white', BGCOLOR='blue'), uvalue='nextimage')
  buttont5b = widget_button(tab5base99, value='    close    ', uvalue='99', scr_xsize=100)


WIDGET_CONTROL, /REALIZE, guiwindow, set_uvalue=arr
XMANAGER, 'speckle_gui', guiwindow, /no_block;,Event_Handler='Draw_Resize_TLB_Resize'

end
