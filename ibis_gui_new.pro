;--------------------------------------
;     GUI FOR IBIS DATA REDUCTION
;--------------------------------------
;version for new camera only. cont from v5 of previous GUI
;3.1.2014 LK
;6.3.2014, new button to correct for atmospheric refraction (can be up
;to 15 px during the morning/evening)
;19.6.15 major change to apply prefilter profile during flatfielding

;improvements:
;faster finding of paths
;database to handle different file parameters
;alignment wrt to speckle image (=time sequences should be aligned)
;better polcal based on response matrices


PRO ibis_gui_new_event,ev
widget_control, ev.top, get_uvalue = info
widget_control, ev.id, get_uvalue=nr

;------------------- set variables -----------------------------------------------
if nr eq 1 then begin ;create paths

	if file_test('ibis_paths.txt') then begin
	close,/all
	openr,1,'ibis_paths.txt'
	date = '' & path = '' & path1='' & wpath='' & telparpath='' & lfile='' & la='' & polpath=''
	readf,1,date & *info.date=date     ;yyyymmdd
	readf,1,path & *info.path=path     ;path to original data
        readf,1,path1 & *info.path1=path1  ;path to save data
	readf,1,wpath & *info.wpath=wpath  ;path to original wl data
        readf,1,polpath & *info.polpath=polpath ;path to polcal (only needs to be set if polcal taken on different date
    	readf,1,la & *info.la=la ;wavelength
    	;print,la
	close,1 
	endif else begin ;set to some values which user can change manually (new values will be saved then)
	*info.date = '20111014'
	*info.path = '/Users/kleintl/sanhome/ibis/14oct11/ibis/'
	*info.path1 = '/Users/kleintl/sanhome/ibis/reduc/20111014test/'
	*info.wpath = '/Users/kleintl/sanhome/ibis/14oct11/whitelight/'
        *info.polpath=''
	*info.la = ['8542 6302']
	endelse
;---- end variables test -------------------------------------

;call separate GUI to select paths:
tmp = TextBox_lk2(Group_Leader=ev.top, Cancel=cancelled, info=info)

;initialize log file
log_create,*info.path1

;test if new camera. set flagncam to 1 if yes.
*info.flagncam=1 

;add variables to log
log_add_vars_v2,*info.path1,*info.date,*info.path,*info.wpath,*info.polpath,*info.la,*info.flagncam

print,'-------- main variables set -------------'

@ibis_reduce.pro
print,'read ibis_reduce.pro'
print,'-------- paths and variables initialized -------------'

;add info to log
log_add_paths,*info.path1,*info.wdfile,*info.wffile,*info.dcdir,*info.ffdir,*info.datadir,*info.griddir,*info.tardir

tmp = widget_info(ev.top,/geometry)
if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
widget_control,ev.top,/destroy  ;update ffdir and datadir
ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=tmp.xoffset,yoffset=tmp.yoffset ;call program


endif

;------- polarimetry / spectroscopy ---
if nr eq 22 then begin
	(*info.button2b_state) = ev.value
        ;add selection in log
        datetime = systime()
        if (*info.button2b_state) eq 0 then txt='polarimetry' else txt='spectroscopy'
        newtext = strarr(3)
        newtext[0] = '\textbf{selected: '+txt+'}\\'
        newtext[1] = string(datetime)+'\\'
        newtext[2] = ' '
        log_write,*info.path1,newtext
;	if *button2b_state eq 1 then print,'spectro' else print,'pola'

endif

;------- dual / single beam ----
if nr eq 23 then begin 
	(*info.button2c_state) = ev.value
        ;add selection in log
        datetime = systime()
        if (*info.button2c_state) eq 0 then txt='dual beam' else txt='single beam'
        newtext = strarr(3)
        newtext[0] = '\textbf{selected: '+txt+'}\\'
        newtext[1] = string(datetime)+'\\'
        newtext[2] = ' '
        log_write,*info.path1,newtext

endif

;------- bridges yes/no ----
if nr eq 24 then begin 
	(*info.button2d_state) = ev.value
        ;add selection in log
        datetime = systime()
        if (*info.button2d_state) eq 0 then txt='parallel execution' else txt='serial execution'
        newtext = strarr(3)
        newtext[0] = '\textbf{selected: '+txt+'}\\'
        newtext[1] = string(datetime)+'\\'
        newtext[2] = ' '
        log_write,*info.path1,newtext
endif

;----------------------------------------- white light data -----------------------------------
if nr eq 3 then begin ;wl database
;TEST: do not do anything and see what will fail later
;	fname = '*.fits'
;	wl_sid = ibis_wl_db_new(*info.wpath, fname, savefile1 = *info.wldbfile, savefile2 = *info.tpdbfile,$
;                 telpath=*info.telparpath,flagncam=*info.flagncam)
;        ;only kept for compatibility, todo: delete and check dependences
;        log_add_wldb,*info.path1,*info.wldbfile,*info.tpdbfile
;
;	print,'-------- wl database created -------------------'
endif

;--- not so elegant test if button is selected
if nr eq 44 then begin
	if *info.button4b_state eq 0 then *info.button4b_state = 1 else *info.button4b_state = 0
endif

if nr eq 4 then begin ;wl dark and flat
	if *info.button4b_state eq 1 then $
	ibis_wl_avgdcff, *info.wffile, *info.wdfile, *info.wlffile, *info.wldfile,/overwrite, flagncam=*info.flagncam else $;box set
	ibis_wl_avgdcff, *info.wffile, *info.wdfile, *info.wlffile, *info.wldfile, flagncam=*info.flagncam

        log_add_wldcff,*info.path1,*info.wlffile,*info.wldfile
	print,'-------- wl mean dark & flat done ------------------------------'
endif


;----------------------- alignment of nb and wl -------------------------------------------------
if nr eq 5 then begin           ;alignment
;no need to change anything here. WL aligned wrt to NB, one just would
;need to shift WL wrt to speckle in the final cal.
	align_ibis_v2,*info.spath,*info.wpath,*info.cpath,*info.wldfile,*info.wlffile,*info.griddir,*info.path1,$
                      flagncam=*info.flagncam,singlebeam=*info.button2b_state ;1=single, 0=dual
        ;v4: path1 added to be able to write into log from routine     
	print,'-------- images aligned and parameters saved -------------------'
endif

 

;---------------------- dark and flat nb ---------------------------------------------------------
if nr eq 65 then begin
	if *info.button6a_state eq 0 then *info.button6a_state = 1 else *info.button6a_state = 0
endif


if nr eq 66 then begin ;test if checkbox for flat set
	widget_control,ev.id,get_value=flatbox
	;print,flatbox
	;nfl = ptr_new(n_elements(ffdir)) ;number of flatfield directories
	for i=0,n_elements(*info.ffdir)-1 do begin
	 if flatbox[i] eq 0 then (*info.ffptr)[i] = 0 else  (*info.ffptr)[i] = 1 ;change pointer if box set/unset
	endfor
endif


if nr eq 6 then begin ;dark and flat
      	if *info.button6a_state eq 1 then $  ;assumes flagncam=1
	ibis_dark_avg_nb, *info.spath, *info.cpath, *info.path1,/overwrite else $;new cam, dcdir automatic
	ibis_dark_avg_nb, *info.spath, *info.cpath, *info.path1


 	;recreate ffdir according to selection
	ind = where((*info.ffptr) eq 1)
        if ind[0] eq -1 then begin
          print,'choose a flat' & return ;user forgot to select a flat
	endif
        ffdir2 = (*info.ffdir)[ind]  ;new, shorter ffdir
	nmax = fltarr(n_elements(ffdir2))

 	if *info.button6a_state eq 1 then $
	ibis_flat_avg_nb,*info.spath,*info.cpath,ffdir2,*info.path1,/overwrite else $  ;new cam
	ibis_flat_avg_nb,*info.spath,*info.cpath,ffdir2,*info.path1
   
print,'-------- nb mean dark & flats done -----------------------------'
endif

;-------------------------------------- blueshift -----------------------------------
if nr eq 7 then begin ;blueshift
	for oloop = 0, n_elements(*info.la)-1 do begin 
	lambda = (*info.la)[oloop]
        print,'blueshift calculation for: ',lambda
	ffile = *info.cpath + lambda + '_flat.sav'
	dfile = *info.cpath + lambda + '_dark.sav'
	bfile = *info.cpath + lambda + '_blueshift.sav'
	wlffile = *info.cpath + 'flat_' +*info.date+ '.sav'
	wldfile = *info.cpath + 'dark_' +*info.date+ '.sav'
	
	wrange = [35,59] ;will be set in program, it doesn't matter what numbers are set here 
                         ;it is important to select only 1 line (for example in 6301 & 6302 region)
	npoints = 3 ;specifies how many wl points around line core are used for parabolic fit
	
        if (file_test(ffile))[0] ne 1 then begin
           print,'Flat does not exist!'
           return
        endif
      
        ;call offset_map with different parameters depending on single/dual and pol
        if *info.button2b_state eq 0 and *info.button2c_state eq 0 then $
	offset_map = ibis_blueshift(ffile, dfile, wrange, npoints, aps, *info.path1,SAVEFILE = bfile, $
                     /POL, /DUAL,flagncam=*info.flagncam)
    
        if *info.button2b_state eq 0 and *info.button2c_state eq 1 then $
	offset_map = ibis_blueshift(ffile, dfile, wrange, npoints, aps, *info.path1,SAVEFILE = bfile, $
                     /POL, /SINGLE,flagncam=*info.flagncam)
   
        if *info.button2b_state eq 1 and *info.button2c_state eq 1 then $
	offset_map = ibis_blueshift(ffile, dfile, wrange, npoints, aps, *info.path1,SAVEFILE = bfile, $
                     flagncam=*info.flagncam)

        if *info.button2b_state eq 1 and *info.button2c_state eq 0 then print,'single beam required for spectroscopy !!! Try again...'
         ;stokes does not need to be set but is found from flat info array
	;(uses ibis_get_aperture)
         ;logging
         restore,bfile
         log_add_bs,*info.path1,lambda,offset,npoints
   


	endfor
print,'-------- blueshift calculated for each wl -----------------------------'
endif

;----------------------- display blueshift ---------------------------------------------------
if nr eq 77 then begin
	for oloop = 0, n_elements(*info.la)-1 do begin 
		lambda = (*info.la)[oloop]
		bfile = *info.cpath + lambda + '_blueshift.sav'
                if (file_test(bfile))[0] ne 1 then begin
                   print,'Blueshift does not exist!'
                   return
                endif
  		view_blueshift,bfile,windex=oloop*2.+1 ;windex is window index
	endfor
print,'-------- blueshift displayed -----------------------------'
endif


;-------------------------------------- nb gain --------------------------------------------
if nr eq 88 then begin ;cog or poly fit?
	(*info.button8b_state) = ev.value
;cog is more stable for broad lines (H-a) and fringes
endif


if nr eq 8 then begin 
        ;log
        time=systime()
        newtext = strarr(2) ;new entries for log
        newtext[0] = '\textbf{NB Gain: ibis\_gain.pro}\\'
        newtext[1] = string(time)+'\\'
        log_write,*info.path1,newtext

	for oloop = 0, n_elements(*info.la)-1 do begin 
	lambda = (*info.la)[oloop]
	ffile = *info.cpath + lambda + '_flat.sav'
	dfile = *info.cpath + lambda + '_dark.sav'
	gfile = *info.cpath + lambda + '_gain.sav'
	fpfile = *info.cpath + lambda + '_flatprof.sav'
	bfile = *info.cpath + lambda + '_blueshift.sav'
        restore,bfile

      ;cog is better for broad lines (like Ha)
        if *info.button8b_state eq 0 then offset = offset.poly_fit else $
             offset = offset.cog_fit
        if *info.button8b_state eq 0 then tmp=' using POLY fit' else $
             tmp=' using COG fit'
        print,tmp        

	smooth = 0 ;better than 3 (only has effect if wl spacing is large)
        print,'--- calculating gain for: ',lambda

        ;add to log
        newtext = strarr(1) ;new entries for log
        newtext[0] = 'gain calculated for: '+string(lambda,format='(I4)')+tmp+' \\' 
        log_write,*info.path1,newtext

        ;call ibis_gain with different parameters depending on pol/spectroscopy, single/dual
        if *info.button2b_state eq 0 and *info.button2c_state eq 0 then $
	ibis_gain, ffile, dfile, offset, aps, gfile, fpfile, /POL, SMOOTH = smooth, /DUAL, flagncam=*info.flagncam;, STOKES=6

        if *info.button2b_state eq 0 and *info.button2c_state eq 1 then $
	ibis_gain, ffile, dfile, offset, aps, gfile, fpfile, /POL, SMOOTH = smooth, /SINGLE, flagncam=*info.flagncam
 
        if *info.button2b_state eq 1 and *info.button2c_state eq 1 then $
	ibis_gain, ffile, dfile, offset, aps, gfile, fpfile, SMOOTH = smooth, flagncam=*info.flagncam
 
        if *info.button2b_state eq 1 and *info.button2c_state eq 0 then print,'single beam required for spectroscopy !!! Try again...'
    
	endfor
 
        newtext = strarr(1) ;new entries for log
        newtext[0] = ' '
        log_write,*info.path1,newtext

print,'-------- nb gain table created -----------------------------'
endif


;-------------------------------------- speckle --------------------------------------------

;limb data or not?
if nr eq 98 then begin 
     if *info.button9b_state eq 0 then *info.button9b_state = 1 else *info.button9b_state = 0
endif

if nr eq 95 then begin 
     speckle_gui
endif


if nr eq 9 then begin 
  	if file_test(*info.speckle_dir) ne 1 then spawn,'mkdir '+*info.speckle_dir
        ibis_reduce_speckle_ncam, *info.wpath, *info.speckle_dir, *info.date, *info.wldfile, *info.wlffile,*info.path1, savefile=*info.sdbfile
;new cam does not have option for number of images per burst
;one speckle file per scan per filter will be saved
;### todo: make choice of files
;to redo database:
;         ibis_reduce_speckle_dbonly_ncam, *info.wpath, *info.speckle_dir, *info.date, *info.wldfile, *info.wlffile, savefile=*info.sdbfile

print,'-------- speckle files prepared. run fortran code manually! -----------------------------'
endif


if nr eq 10 then begin 
	;bin = wl_sid.bin ;## todo, now smaller
        bin = fltarr(4)
        openr,2,'speckle_dims.txt' ;contains x and y size of wl image
        for i=0,3 do begin
        readf,2,tmp
        bin[i] = tmp
        endfor
        close,2

	ibis_prepare_speckle, *info.speckle_dir, *info.speckle_dir, *info.wlffile,*info.path1, BIN = bin
print,'-------- conversion from fortran to idl format done -----------------------------'
endif


;------------------------------ destretch speckle ----------------------------------------------
if nr eq 101 then begin 
ibis_detr_speckle,*info.speckle_dir,*info.sdbfile,*info.path1,flagncam=*info.flagncam
;to temporally align all speckle images. The different nb wavelengths
;should thus only have a spatial offset. this procedure overwrites the
;existing sav images with the destretched version.

print,'-------- speckle destretched, .sav overwritten -----------------------------'
end


;------------------------------ combine wl/nb --------------------------------------------------
if nr eq 112 then begin  ;keyword overwrite
     if *info.button11c_state eq 0 then *info.button11c_state = 1 else *info.button11c_state = 0
endif
if nr eq 113 then begin ;keyword movie
     if *info.button11d_state eq 0 then *info.button11d_state = 1 else *info.button11d_state = 0
endif


if nr eq 111 then begin ;test if checkbox for datadir set
	widget_control,ev.id,get_value=datbox
;	print,datbox
	ndat = n_elements(*info.datadir) ;number of flatfield directories
 
nomoreff:
	for i=0,ndat-1 do begin
	if datbox[i] eq 0 then (*info.datptr)[i] = 0 else  (*info.datptr)[i] = 1 ;change pointer if box set/unset
	endfor
;  print,*datptr

endif



if nr eq 11 then begin 
        ind = where((*info.datptr) eq 1)
        if ind[0] eq -1 then begin
           print,'choose which directories to combine!'
           return
        endif


	*info.datadir2 = (*info.datadir)[ind]  ;new, shorter datadir
;	print,datadir2
        currupath = (*info.upath)[ind]   
;assign directories _000 , _001 etc. always to the same timestamp

 ;log
        time=systime()
        newtext = strarr(2+n_elements(*info.datadir2)) ;new entries for log
        if *info.flagncam ne 1 then newtext[0] = '\textbf{Combining NB and WL: ibis_combine_lk.pro}\\'
        if *info.flagncam eq 1 then newtext[0] = '\textbf{Combining NB and WL: ibis_combine_ncam.pro}\\'
        newtext[1] = string(time)+'\\'
        newtext[2] = '\textit{'+string(*info.datadir2)+'} $\rightarrow$ \textit{'+string(currupath)+'} \\'
        ;----- check for underscores (need to be \_ in tex) ----
        remove_underscore,newtext
        log_write,*info.path1,newtext


	for ii=0,n_elements(*info.datadir2)-1 do begin
	output_dir = currupath[ii] + '/' 
;
	input_dir = *info.spath +'/'
	afilepath = *info.cpath
	region = [15,40,230,46]
	region = [68,59,195,120] ;small region for crosscorrelation of last shift
	;not sure how well this works for 8542...only used with keywords atm or daydrift
	;### make automatic!!!
        lfile ='' ;used to be linearity file, no longer necessary


        ibis_combine_ncam_v2, input_dir, output_dir, afilepath, (*info.datadir2)[ii],$
             movie=*info.button11d_state, overwrite=*info.button11c_state,info=info,$
             singlebeam=*info.button2b_state ;1=single, 0=dual

        endfor ;all *info.datadir2
 
        ;log
        time=systime()
        newtext = strarr(3) ;new entries for log
        if *info.flagncam ne 1 then newtext[0] = '\textbf{Assigning speckle: assign\_speckle2nb.pro}\\'
        if *info.flagncam eq 1 then newtext[0] = '\textbf{Assigning speckle: assign\_speckle2nb\_ncam.pro}\\'
        newtext[1] = string(time)+'\\'
        newtext[2] = ' '
        log_write,*info.path1,newtext  

	for oloop = 0, n_elements(*info.la)-1 do begin 
	lambda = (*info.la)[oloop]
       	        *info.snbdbfile = *info.cpath + 'nb_speckle_files_'+lambda+'_000.sav'
		nb_speckle_db = assign_speckle2nb_ncam_v2(*info.upath, lambda, *info.sdbfile, *info.speckle_dir, SAVEFILE = *info.snbdbfile)
	endfor

;### todo: speckle assignment for new camera (currently only option is
;1 file per filter per filename (s000,8542-> 1 speckle recon. with ni images)

print,'-------- wl and nb combined, speckle assigned -----------------------------'
endif


;----------------- correct for atmospheric dispersion ----------------
if nr eq 114 then begin
     ind = where((*info.datptr) eq 1)
        if ind[0] eq -1 then begin
           print,'choose which directories to combine!'
           return
        endif

	*info.datadir2 = (*info.datadir)[ind]  ;new, shorter datadir
        currupath = (*info.upath)[ind]   


        ;log
        time=systime()
        newtext = strarr(2+n_elements(*info.datadir2)) ;new entries for log
        if *info.flagncam eq 1 then newtext[0] = '\textbf{Shifting NB on WL: ibis_shiftatmosdisp.pro}\\'
        newtext[1] = string(time)+'\\'
        newtext[2] = '\textit{'+string(*info.datadir2)+'} $\rightarrow$ \textit{'+string(currupath)+'} \\'
        remove_underscore,newtext
        log_write,*info.path1,newtext

        ;do shifting:
	for ii=0,n_elements(*info.datadir2)-1 do begin
	output_dir = currupath[ii] + '/' 
        ibis_shiftatmosdisp, output_dir,/oneperdir
        ;option oneperdir only asks user about one file per directory
        ;and applies the same shifts for all other files
        endfor


print,'-------- atmospheric dispersion corrected -----------------------------'
endif


;----------------------- reduce ibis ----------------------------------------------------------
;limb data or not?
if nr eq 122 then begin 
	if *info.button12b_state eq 0 then *info.button12b_state = 1 else *info.button12b_state = 0
endif

if nr eq 123 then begin ;speckle/avg/seq
	(*info.button12c_state) = ev.value
;	print,*button12c_state
endif

if nr eq 12 then begin

;read kernel size (same for all wavelengths).
	widget_control,*info.text12c,get_value=*info.kerneltxt  ;number of images for speckle
        tmp = strsplit(*info.kerneltxt,'[,]',/extr) ;get kernel values
        kerarr = fix(tmp) ;now integer array
        nkerarr = n_elements(kerarr)

;test which reconstruction method to use
        if *info.button12c_state eq 0 then meth = 'SPECKLE'
        if *info.button12c_state eq 1 then meth = 'AVG'
        if *info.button12c_state eq 2 then meth = 'SEQ'
   
        if *info.button2d_state eq 0 then flag_bridge=1 else flag_bridge=0

;check if poly or cog
        if *info.button8b_state eq 0 then tmp=' using poly fit' else $
                tmp=' using cog fit'
       
;if datadir not defined, define it..(may happen if program is re-run
;and 'combine all' step is skipped)
;help,*info.datadir2
;print,n_elements(*info.datadir2)

     IF (n_elements(*info.datadir2) eq 0) then begin ;if *info.datadir2 undefined
        if total(*info.datptr) eq 0 then begin
        print,'choose which directories to reduce!'
        return
        endif
        ind = where((*info.datptr) eq 1)
        *info.datadir2 = (*info.datadir)[ind]  ;new, shorter 
     ENDIF

       ind = where((*info.datptr) eq 1) 
     IF ind[0] eq -1 then begin
       print,'choose which directories to reduce!'
       return
     ENDIF
     *info.datadir2 = (*info.datadir)[ind]  ;new, shorter datadir
     currpath = (*info.rpath)[ind] ;always use same directory for same timestamp
     currupath = (*info.upath)[ind]


     if (*info.datadir2)[0] eq 'temp' then begin ;if datadir2 wrongly defined
     print,'choose which directories to reduce!'
     return
     endif

     ;logging
        time=systime()
        newtext = strarr(3+n_elements(*info.datadir2)) ;new entries for log
        newtext[0] = '\textbf{IBIS data reduced: ibis_calibrate.pro}\\'
        newtext[1] = string(time)+'\\'
        tstr = ''
        for i=0,n_elements(kerarr)-1 do tstr=tstr+string(kerarr[i])+' '
        newtext[2] = 'Method: \textit{'+meth+'} with kernel: \textit{'+tstr+'}.'+tmp+' \\'
        newtext[3] = '\textit{'+string(*info.datadir2)+'} $\rightarrow$ \textit{'+string(currpath)+'} \\'
   
       ;----- check for underscores (need to be \_ in tex) ----
       remove_underscore,newtext 
       log_write,*info.path1,newtext
      ;end logging


    ;----------------------- for each wavelength --------------------
    FOR oloop =0, n_elements(*info.la)-1 do begin
	lambda=(*info.la)[oloop]
	afile=file_search(*info.cpath,lambda+'_align_params.sav')
	bfile=file_search(*info.cpath,lambda+'_blueshift.sav')
	dfile=file_search(*info.cpath,lambda+'_dark.sav')
	gfile=file_search(*info.cpath,lambda+'_gain.sav')
	if (afile eq '' or bfile eq '' or dfile eq '' or gfile eq '') then message,'At least one file undefined!'
	nb_speckle_files = file_search(*info.cpath + '/nb_speckle_files_'+lambda+'*.sav')

        ;log: add wavelength
        newtext = strarr(1)
        newtext[0] = 'wavelength: '+string(lambda,format='(I4)')+' \\ '
        log_write,*info.path1,newtext	

	FOR kloop = 0, n_elements(currupath)-1 do begin ;for all chosen data series
		*info.snbdbfile = *info.cpath + 'nb_speckle_files_'+lambda+'_00'+strtrim(string(kloop),1)+'.sav'
		print, currupath[kloop]	
		scan_file = file_search(currupath[kloop] + '/' + lambda + '_nb*.sav',count=n)
		wl_file= file_search(currupath[kloop] + '/' + lambda + '_bb*.sav')
		out_dir = currpath[kloop] + '/'
 
       ;if a line was omitted for certain timestamps, then no files are found for some wavelengths
                if n eq 0 then begin
                print,'*** no data found for:',lambda,' in ',currupath[kloop]
                goto,notthisdir
	        endif
	
	;-- define structure for destretch parameters
		dstr_idl = create_struct('method', strarr(1), 'detrend', intarr(1), $
			'kernel', intarr(nkerarr), 'region', intarr(2,2), $
			'avg_img', intarr(1))

        ;write timestamp into directory for easier access (/scans)
         ;       openw, 2, currupath[kloop]+'/timestamp.txt'
         ;       printf, 2, *info.datadir2[kloop]
         ;       close,2
   
        ;write timestamp into directory for easier access (/results)
          ;      openw, 2, currpath[kloop]+'/timestamp.txt'
          ;      printf, 2, *info.datadir2[kloop]
          ;      close,2
	
       ;-- txt file with rigid aligment region
       ;-- v4 new: because the reduction is done in the order wl1,datadir1,datadir2,...,wl2,
       ;datadir1,datadir2, ..., it is smarter to define one region for each datadir
       ;this way, the alignment can be chosen to be the same for all wavelenghts in 
       ;a given directory
             currind = where((*info.datadir2)[kloop] eq *info.datadir)
  
            ;case: file doesn't exist -> create empty file
	        if file_test('rigidalign.sav') ne 1 then begin 
             	dstr_idl.region = [[20,45],[235,465]]   ;; qs area in FOV PGJ 20 May 08, i.e.x0=20,x1=235
                ralign = fltarr(2,2,n_elements(*info.datadir))  ;create varible with correct size (one alignment per datadir)
                save,filename='rigidalign.sav',ralign,/ve

            ;case: file exists -> test if align for curent dir exists                
                endif else begin
    	        restore,'rigidalign.sav'
                   if ralign[0,0,currind] eq 0 then begin   ;region for this datastamp not defined yet
                 	dstr_idl.region = [[20,45],[235,465]]   ;; 
                        ralign[*,*,currind] = dstr_idl.region
                  endif else begin
            	        dstr_idl.region = reform(ralign[*,*,currind] )
                  endelse
                endelse
  
		restore,wl_file[0]
		region = dstr_idl.region ;short name
		;LK: added 110304. Find region interactively and only once for dataset
                window,xsize=(size(mwld))[1],ysize=(size(mwld))[2]
		tvscl,mwld[*,*,0]

		loadct,3
		plots,[region[0,0],region[0,1],region[0,1],region[0,0],region[0,0]],$
                  [region[1,0],region[1,0],region[1,1],region[1,1],region[1,0]],color=120,thick=2,/dev
		loadct,0
		print,'the preselected region for rigid alignment region (WL->speckle) is drawn on the image'
		print,'press (a) to accept, (c) to change'
		if get_kbrd() eq 'c' then begin
		print,'select lower left then upper right'
		xy,/dev,count=2,pos=pos
                ;if user selects top/right first...
                if pos[1,0] gt pos[1,1] then begin
                   tmp = pos[1,1]
                   pos[1,1] = pos[1,0]
                   pos[1,0] = tmp
                endif
                if pos[0,0] gt pos[0,1] then begin
                   tmp = pos[0,1]
                   pos[0,1] = pos[0,0]
                   pos[0,0] = tmp
                endif

		dstr_idl.region = [[pos[0,0],pos[1,0]],[pos[0,1],pos[1,1]]]
                region = pos
         	plots,[region[0,0],region[0,1],region[0,1],region[0,0],region[0,0]],$
                 [region[1,0],region[1,0],region[1,1],region[1,1],region[1,0]],color=120,thick=2,/dev
		endif
            ;save parameters for rigid alignment
                ralign[*,*,currind] = region
                save,ralign,filename='rigidalign.sav'
                alimg = tvrd(/true)

		;do not choose limb! the mask of speckle and nb data is different and an offset would occur 
		restore, bfile, /verb


;;;		offset = offset.poly_fit 
                if *info.button8b_state eq 0 then offset = offset.poly_fit else offset = offset.cog_fit
                 print,tmp ;cog/poly

		print,'**********************************************'
		print,'Blueshift map :', bfile
		print,'Scans in directory :', currupath[kloop]
         	if *info.flagncam ne 1 then		print,'Speckle connection :', nb_speckle_files[kloop] else $
               	print,'Speckle connection :', nb_speckle_files[0]
		print,'**********************************************'
		
   		
        	dstr_idl.method = meth    
		dstr_idl.detrend = 0                    ;; 0 : no detrend, 1,2,3 ... : order of polynomial fitted to detrend.
;### lk: not sure what is best for detrend

;		dstr_idl.kernel = [0,64,32,16] ;limb data, does not work, needs 0 for rigid aligment
		dstr_idl.kernel = kerarr ;
		dstr_idl.avg_img = 3                    ;; running mean in sequence mode. 

;------ test if this wl and timestamp already reduced
                outfilename = (STRMID(scan_file[0], (STRSPLIT(scan_file[0], '/'))))[N_ELEMENTS(STRSPLIT(scan_file[0], '/'))-1]

                currofl = out_dir + '/' + outfilename
                if file_test(currofl) then begin
                print,'At least one file with this timestamp and wavelength already reduced. Overwrite (y,n, def=n)?' 
                ans = ' '
                read,ans
                if ans eq 'y' then goto,cont_calib else goto,notthisdir
                endif
        	cont_calib:

        ;--------- logging
        ;create eps for alignment region
        files = file_search(*info.path1+$
        '/log/align'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles)  
        set_plot,'PS'
        alignbox = *info.path1+'/log/align'+string(lambda, FORMAT='(I4)')+$
            string(nfiles,format='(I03)')+'.eps'
        device,filename=alignbox,/color
        device,/encaps,bits_per_pixel=16,xsize=5,ysize=10
        tv,alimg,/true,xsize=5,/cent
        device,/close
        set_plot,'x'


        newtext = strarr(11) ;new entries for log              
                                ; region, including picture
        newtext[0] = 'current dir: \textit{'+string((*info.datadir2)[kloop])+'}, reducing \textit{'+string(n)+'} files. \\'
        newtext[1] = 'alignment region (x0,x1,y0,y1): \textit{'$
                         +string(region[0,0],region[0,1],region[1,0],region[1,1])+'}, Fig.~\ref{align'+$
                         string(lambda,format='(I4)')+string(nfiles,format='(I03)')+'} \\'
        newtext[2] = '\begin{figure}[htb]'
        newtext[3] = '\begin{center}'
        newtext[4] = '\includegraphics[width=5cm]{align'+string(lambda,format='(I4)')+$
                      string(nfiles,format='(I03)')+'}'
        ;continues below...

	
		;; create structure for speckle option.
		;;-------------------------------------
		
		speckle_info = create_struct('datapath', strarr(1), 'database', strarr(1))
		speckle_info.datapath = *info.speckle_dir
		if *info.flagncam ne 1 then speckle_info.database = nb_speckle_files[kloop] else $
                        speckle_info.database = nb_speckle_files[0]
                ;for new cam, only one speckle file (000) is present

		IF *info.button12b_state eq 1 then begin ;limb
 
                if *info.button2b_state eq 0 and *info.button2c_state eq 0 then $ ;pol dual
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
                         /POL, TPFILE = *info.tpdbfile, /DUAL, /limb, flagncam=*info.flagncam,flag_bridge =flag_bridge 
                        ;set limb to avoid shifting

                if *info.button2b_state eq 0 and *info.button2c_state eq 1 then $ ;pol single
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
                         /POL, TPFILE = tpdbfile, /single, /limb, flagncam=flagncam,flag_bridge=flag_bridge 

                if *button2b_state eq 1 and *button2c_state eq 1 then $ ;spectroscopy
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
                         TPFILE = *info.tpdbfile, /limb, flagncam=*info.flagncam,flag_bridge=flag_bridge

                if *info.button2b_state eq 1 and *info.button2c_state eq 0 then print,$
                  'single beam required for spectroscopy !!! Try again...'
    
		ENDIF else begin ;no limb
   
                if *info.button2b_state eq 0 and *info.button2c_state eq 0 then $ ;pol dual
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
			 /POL, TPFILE = *info.tpdbfile, /DUAL, flagncam=*info.flagncam,flag_bridge=flag_bridge

                if *info.button2b_state eq 0 and *info.button2c_state eq 1 then $ ;pol single
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
			 /POL, TPFILE = *info.tpdbfile, /single, flagncam=*info.flagncam,flag_bridge=flag_bridge
 
                if *info.button2b_state eq 1 and *info.button2c_state eq 1 then $ ;pol dual
		for i=0,n-1 do ibis_calibrate, out_dir, gfile, dfile, scan_file[i], wl_file[i], afile,$
                         offset, aps, DSTR_IDL = dstr_idl, SPECKLE = speckle_info, $  
			 TPFILE = *info.tpdbfile, flagncam=*info.flagncam,flag_bridge=flag_bridge

                if *info.button2b_state eq 1 and *info.button2c_state eq 0 then print,$
                  'single beam required for spectroscopy !!! Try again...'
    
		ENDELSE

 
       ;add sample image to log
        print,'writing log' 
        samplefile = file_search(out_dir+'/'+string(lambda,format='(I04)')+'_nb???.sav')
        restore,samplefile[0] ;restores nbdwc[x,y,stokes,lambda,2]
   
        files = file_search(*info.path1+$
        '/log/sample'+string(lambda, FORMAT='(I4)')+'*.eps',count=nfiles2)  
        set_plot,'PS'
        alignbox = *info.path1+'/log/sample'+string(lambda, FORMAT='(I4)')+$
            string(nfiles2,format='(I03)')+'.eps'
        device,filename=alignbox,/color
        device,/encaps,bits_per_pixel=16,xsize=10,ysize=10
if (size(nbdwc))[0] ne 3 then begin
        nwavelngths = (size(nbdwc))[4] 
        tvscl,nbdwc[*,*,0,nwavelngths/4,0],0,0,xsize=5,/cent ;line wing
        tvscl,nbdwc[*,*,0,nwavelngths/2,0],5,0,xsize=5,/cent ;line core
endif else begin
        nwavelngths = (size(nbdwc))[3] ;single beam
        tvscl,nbdwc[*,*,nwavelngths/4,0],0,0,xsize=5,/cent ;line wing
        tvscl,nbdwc[*,*,nwavelngths/2,0],5,0,xsize=5,/cent ;line core
endelse
        device,/close
        set_plot,'x'
  

        newtext[5] = '\includegraphics[width=10cm]{sample'+string(lambda,format='(I4)')+$
                      string(nfiles2,format='(I03)')+'}' 
        newtext[6] = '\caption{Alignment region, line wing, line core, '+$
                      string((*info.datadir2)[kloop])+', '+string(lambda,format='(I4)')+'}'
        newtext[7] = '\label{align'+string(lambda,format='(I4)')+string(nfiles,format='(I03)')+'}'
        newtext[8] = '\end{center}'
        newtext[9] = '\end{figure}'
        newtext[10] = ' '
        remove_underscore,newtext
        log_write,*info.path1,newtext
 
                notthisdir:
          ENDFOR   ;end chosen dataseries
   ENDFOR ;end wavelengths
print,'-------- ibis date reduced, spectroscopy done. -----------------------------'
endif


;--------------------------- polcal ------------------------------------------------------------
if nr eq 133 then begin ;determine if polcal auto or manual
;this is called twice when the button is clicked
        if ev.value ne *info.button13b_state then begin
           (*info.button13b_state) = ev.value
           tmp = widget_info(ev.top,/geometry)
           if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
           widget_control,ev.top,/destroy                                            
           ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=tmp.xoffset,yoffset=tmp.yoffset ;call program
        endif   
     ;cal has different lines for auto/manual option
endif


if nr eq 134 then begin ;choose automatic polcal directory 1
        winfo = widget_info(ev.top,/geometry)
        tmp2 = (*info.polvals)[0]
	tmp = buttonbox_lk_v2(Group_Leader=ev.top, Cancel=cancelled,info=info)
 	if tmp eq '' then (*info.polvals)[0] = tmp2 else $;somebody clicked on 'done' -> keep current dir
	(*info.polvals)[0] = tmp
        if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
        ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=winfo.xoffset,yoffset=winfo.yoffset;call program
 endif

if nr eq 135 then begin ;choose automatic polcal directory 2
       winfo = widget_info(ev.top,/geometry)
       tmp2 = (*info.polvals)[1]
       tmp = buttonbox_lk_v2(Group_Leader=ev.top, Cancel=cancelled,info=info)
       if tmp eq '' then (*info.polvals)[1] = tmp2 else $ ;somebody clicked on 'done' -> keep current dir
          (*info.polvals)[1] = tmp
       if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
       ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=winfo.xoffset,yoffset=winfo.yoffset ;call program
endif

if nr eq 136 then begin ;choose automatic polcal directory 3
        winfo = widget_info(ev.top,/geometry)
        tmp2 = (*info.polvals)[2]
	tmp = buttonbox_lk_v2(Group_Leader=ev.top, Cancel=cancelled,info=info)
	if tmp eq '' then (*info.polvals)[2] = tmp2 else $;somebody clicked on 'done' -> keep current dir
	(*info.polvals)[2] = tmp
        if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
        ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=winfo.xoffset,yoffset=winfo.yoffset ;call program
endif

if nr eq 137 then begin ;choose automatic polcal directory 4
        winfo = widget_info(ev.top,/geometry)
        tmp2 = (*info.polvals)[3]
	tmp = buttonbox_lk_v2(Group_Leader=ev.top, Cancel=cancelled,info=info)
	if tmp eq '' then (*info.polvals)[3] = tmp2 else $;somebody clicked on 'done' -> keep current dir
	(*info.polvals)[3] = tmp
        if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
        ibis_gui_new,file='info_'+*info.date+'.sav',xoffset=winfo.xoffset,yoffset=winfo.yoffset ;call program

endif



if nr eq 13 then begin
;; create directories where polarization measurement scans are gathered in 
;; four different directories corresponding to measurements at the four 
;; different table positions. Dont change the order!!
print,'initializing polcal'
      ;  npolcal = round(n_elements(*info.poldir)/28.)<4 ;max 4 table positions
      ;above line is wrong if fewer than 4 positions and polcal aborted
        tmp = where(*info.polvals ne 'N/A')
        npolcal = n_elements(tmp)


	p1path = *info.spath1 + 'polcal_330/'
	p2path = *info.spath1 + 'polcal_15/'
	p3path = *info.spath1 + 'polcal_60/'
	p4path = *info.spath1 + 'polcal_105/'
	
	spawn,'mkdir ' + p1path
	spawn,'mkdir ' + p2path
	spawn,'mkdir ' + p3path
	spawn,'mkdir ' + p4path
	
	;; collext measurements at 4 table postions!!
	;; this has to be done 4 times for the 4 table positions 330/15/60/105.
	;; order is crucial!!! the observers usually go 330/15/60/105!!!

	if (*info.button13b_state) eq 0 then begin;automatic cal
	nmax = 28
	pol='Automatic'

        ;### implement test if correct polcal is chosen
        ;### a check should be done if the user chose suitable values for poldir
        ;  possible crash here if ind is not correct 

	input_dir = *info.polpath
        ind = where(strpos(*info.poldir,(*info.polvals)[0]) ne -1)
	collect_ibis_polcal, input_dir, p1path, (*info.poldir)[ind:ind+27], NMAX = 28,/auto,flagncam=*info.flagncam

        if npolcal ge 2 then begin
	input_dir = *info.polpath
        ind = where(strpos(*info.poldir,(*info.polvals)[1]) ne -1)
	collect_ibis_polcal, input_dir, p2path, (*info.poldir)[ind:ind+27], NMAX = 28,/auto,flagncam=*info.flagncam
        endif

        if npolcal ge 3 then begin
	input_dir = *info.polpath
        ind = where(strpos(*info.poldir,(*info.polvals)[2]) ne -1)
	collect_ibis_polcal, input_dir, p3path, (*info.poldir)[ind:ind+27], NMAX = 28,/auto,flagncam=*info.flagncam
        endif

        if npolcal ge 4 then begin
	input_dir = *info.polpath
        ind = where(strpos(*info.poldir,(*info.polvals)[3]) ne -1)
	collect_ibis_polcal, input_dir, p4path, (*info.poldir)[ind:ind+27], NMAX = 28,/auto,flagncam=*info.flagncam
        endif
        
        tstamps = [(*info.polvals)[0],(*info.polvals)[1],(*info.polvals)[2],(*info.polvals)[3]]
        log_add_autocal,*info.path1,tstamps

	endif else begin ;pol manual
;this may not work (pointers may need to be updated) - never used anyway

	 for i=0,3 do begin  ;read text for exclude
	 widget_control,text13[i],get_value=excltext
	 (*info.exclcal)[i] = excltext ;update in pointer
	 endfor
 
         ;possibility of aborted polcals
         ;##this is just a quick fix, it could be done better by displaying 
         ;all available poldirs and user would assign table positions to them
         if n_elements(*info.poldir) ne 4 then begin
         print,'WARNING: number of polcal directories ne 4!'
         print,'choose directories to exclude (0 1 ...):'
         print,*info.poldir & ans=''
         read,ans
         ;## todo:add error checking
         ans = fix(get_words(ans))
         good_indices = ibis_good_frame_index(n_elements(*info.poldir),ans)
         *info.poldir = (*info.poldir)[good_indices]
         endif
  
        ;log file entry
           tstamps = (*info.poldir)[0:3]
           excl = fix(*info.exclcal)
           log_add_mancal,*info.path1,tstamps,excl

	
         ;if exclusions needed, set variable, otherwise simply execute program
		input_dir = polpath
        	if (*exclcal)[0] eq '' then begin  
		collect_ibis_polcal, input_dir, p1path, (*info.poldir)[0], NMAX = 28,flagncam=*info.flagncam
		endif else begin
		nrs = fix(strsplit((*info.exclcal)[0],' ',/extract)) ;array containing series to exclude
		nmaxval = 28+n_elements(nrs) ;new nmax value
                collect_ibis_polcal, input_dir, p1path, (*info.poldir)[0], NMAX = nmaxval, $
                                   exclude=nrs,flagncam=*info.flagncam ;lk changed
 		endelse
                input_dir = polpath ;overwrite the path modified by 'pass by reference' 

		if (*info.exclcal)[1] eq '' then begin  
		collect_ibis_polcal, input_dir, p2path, (*info.poldir)[1], NMAX = 28,flagncam=*info.flagncam
		endif else begin
		nrs = fix(strsplit((*exclcal)[1],' ',/extract)) ;array containing series to exclude
		nmaxval = 28+n_elements(nrs) ;new nmax value
                collect_ibis_polcal, input_dir, p2path,  (*info.poldir)[1], NMAX = nmaxval, $
                                   exclude=nrs, flagncam=*info.flagncam
 		endelse
                input_dir = polpath ;overwrite the path modified by 'pass by reference' 

		if (*info.exclcal)[2] eq '' then begin  
		collect_ibis_polcal, input_dir, p3path,  (*info.poldir)[2], NMAX = 28,flagncam=*info.flagncam
		endif else begin
		nrs = fix(strsplit((*exclcal)[2],' ',/extract)) ;array containing series to exclude
		nmaxval = 28+n_elements(nrs) ;new nmax value
                collect_ibis_polcal, input_dir, p3path,  (*info.poldir)[2], NMAX = nmaxval,$
                                  exclude=nrs ,flagncam=*info.flagncam
 		endelse
                input_dir = polpath ;overwrite the path modified by 'pass by reference' 

		if (*info.exclcal)[3] eq '' then begin  
		collect_ibis_polcal, input_dir, p4path,  (*info.poldir)[3], NMAX = 28, flagncam=*info.flagncam
		endif else begin
		nrs = fix(strsplit((*info.exclcal)[3],' ',/extract)) ;array containing series to exclude
		nmaxval = 28+n_elements(nrs) ;new nmax value
                collect_ibis_polcal, input_dir, p4path,  (*info.poldir)[3], NMAX = nmaxval, $
                                  exclude=nrs, flagncam=*info.flagncam
 		endelse

   
	endelse ;end manual polcal
	
print,'-------- polcal initialized, measurements found. -----------------------------'
endif

;----------------------------- final polarimetric calibrations including X and T ---------------------
if nr eq 14 then begin
print,'starting polcal'

 ;--- find out which timestampt to calibrate -----
   if (n_elements(*info.datadir2) eq 0) then begin ;if datadir2 undefined
       if total(*info.datptr) eq 0 then begin
       print,'choose which directories to reduce!'
       return
       endif
       ind = where((*info.datptr) eq 1)
       *info.datadir2 = (*info.datadir)[ind]  ;new, shorter 
   endif
     ind = where((*info.datptr) eq 1) 
     if ind[0] eq -1 then begin
     print,'choose which directories to reduce!'
     return
     endif

   currpath = (*info.rpath)[ind] ;path to results_xxx

    if (*info.datadir2)[0] eq 'temp' then begin ;if datadir2 wrongly defined
     print,'choose which directories to reduce!'
     return
    endif

       ;logging
       time=systime()
       newtext = strarr(2)      ;new entries for log
       newtext[0] = '\textbf{Final polcal: }\\'
       newtext[1] = string(time)+'\\'
       log_write,*info.path1,newtext


	;; CALCULATE POLCAL CURVES
	;; now the measurements at the 4 table angles are averaged over the FOV and 
	;; wavelength and are combined. light curves are saved.
	
    for ll=0,n_elements(*info.la)-1 do begin ;for all wavelengths
           lambda = (*info.la)[ll]

                                ;-set interactively for each wl --
           calex = FILE_SEARCH(*info.spath1+'polcal_15/'+string(lambda)+'*.sav',COUNT = n15f) ;sample files
           IF (n15f EQ 0) THEN BEGIN
              PRINT,' '
              PRINT,' NO POLCAL FILES FOUND WAVELENGTH ' + STRTRIM((*info.la)[ll],2)
              PRINT,' THUS, NO POLCAL PERFORMED AT THIS WAVELENGTH'
              goto,skip_ll
           ENDIF
           restore,calex[0]
           sz = size(nb_data)
 
           tmp = ibis_mask2(nb_data[*,*,0],cut=20)
           xprofile = avg(tmp,1)
 
;this is not very reliable
 ;       posi =localmax(findgen(n_elements(xprofile)),abs(xprofile[1:*] - xprofile[0:*]))
 ;       posi2 = where(posi.ypos ge 0.15) ;mod LK 9.10.13
 ;       x0=min(where(avg(tmp,1) ge 0.5))
 ;       x1=posi.xpos[posi2[1]] 
 ;       dxy = posi.xpos[posi2[2]]-posi.xpos[posi2[0]]
 ;       y0=min(where(avg(tmp,0) ge 0.5))
 ;       y1=max(where(avg(tmp,0) ge 0.5))
  
;this may be better to find the FOV
           der = deriv(xprofile) ;this plot should have 4 peaks
           edges = where(abs(der) ge .1)
  ;      ;find max of coherent pixels
           jumps = where(edges[1:*]-edges ne 1)
           x0 = round(avg(edges[0:jumps[0]]))
           x1 = round(avg(edges[jumps[0]+1:jumps[1]]))
           dxy = round(avg(edges[jumps[1]+1:jumps[2]]))-x0
           y0=min(where(avg(tmp,0) ge 0.5))
           y1=max(where(avg(tmp,0) ge 0.5))

        ;old cam is binned anyway
           if sz[1] eq 1000 then bin=2 else bin=1
           fov = [x0,y0,x1,y1,dxy]/bin
           window,xsize=sz[1]/bin,ysize=sz[2]/bin
           tvscl,rebin(nb_data[*,*,0],sz[1]/bin,sz[2]/bin)
           loadct,3
	;plot current region for polcal
           plots,[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],color=120,thick=2,/dev
           plots,fov[4]+[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],$
                       color=120,thick=2,/dev
           loadct,0
           print,'the preselected region for polcal is drawn on the image'
           print,'press (a) to accept, (c) to change'
           if get_kbrd() eq 'c' then begin
              print,'select lower left then upper right, then lower left of r image'
              xy,/dev,count=3,pos=pos
              fov = [pos[0,0],pos[1,0],pos[0,1],pos[1,1],pos[0,2]-pos[0,0]]
              plots,[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],color=120,thick=2,/dev
              plots,fov[4]+[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],$
                    color=140,thick=2,/dev
	endif
                                ;modify fov for non-binned images
           if bin eq 2 then fov = 2*fov

	
        polcal_datafile = *info.cpath + *info.date + '_polcal_' + lambda + '.sav'      
        dfile           = *info.cpath + lambda + '_dark.sav'

        ;find the path to where ibis_gui is located. cu_settings.sav is in the same directory.
        routinepaths = (routine_info(_EXTRA=_extra,/source)).path
        index = where(strmatch(routinepaths,'*ibis_gui_new.pro',/fold_case) eq 1)
        pathtolib = routinepaths[index[0]]
        posibis = strpos(pathtolib,'ibis_gui_new')
        pathtolib = strmid(pathtolib,0,posibis)
	sfile = pathtolib+'cu_settings.sav'	;contains the settings for the calibration 
                                                ;unit (when they were not in the headers yet)



	;; CALCULATE X-MATRICES FOR BOTH BEAMS
        ;; new procedure by Tom Schad, which does the same as Hector's
        ;; ASP code but simpler
	

        ;; the turret mirrors were re-coated in may 09 -> choose t-matrix accordingly
        year = fix(strmid(*info.date,0,4))
        month = fix(strmid(*info.date,4,2))
     
        if year le 2009 then begin
           if month le 5 then begin
          tfile = pathtolib + 'Tmatrix_ibis_Feb1607.idl' 
          flag_oldtcal = 1.
          endif else begin
          tfile = pathtolib +  'Tmatrix_May2010_4708-14125.idl'
          flag_oldtcal = 0.
          endelse
        endif else begin
             tfile = pathtolib +  'Tmatrix_May2010_4708-14125.idl'
             flag_oldtcal = 0.
        endelse

       ;write t matrix into log (filename, not actual matrix)
       newtext=strarr(2)
       newtext[0] = 'Using T-matrix: '+string(tfile)+' \\'
       newtext[1] = ''
       for i=0,n_elements(newtext)-1 do begin
           res = strsplit(newtext[i],'_', count = n,/extr)

           if n gt 1 then begin
               newstring = res[0]
               for ss=1,n-1 do newstring = newstring+'\_'+res[ss]
               newtext[i] = newstring
           endif
       endfor
       log_write,*info.path1,newtext 


      IF NOT FILE_TEST(polcal_datafile) THEN BEGIN 
            ibis_get_polcalcurvec, lambda, *info.spath1, *info.tpdbfile, fov, dfile, sfile, *info.path1, $
             SAVEFILE = polcal_datafile,FLAGNCAM = *info.flagncam      
        ENDIF ELSE BEGIN
            PRINT,' Already done: ',polcal_datafile
        ENDELSE
       
   
	;; MAIN CORRECTION FOR INSTRUMENTAL POLARIZATION
	;--- phils version ---
	opath = currpath
	scan_file = file_search(currpath + '/' + lambda + '_nb???.sav', count = n) ;finds all files in chosen directories
        ;i.e. files in results_001 and results_002

	xfile1 = *info.cpath + '01.ta.' + *info.date + '.' + strtrim(string(fix(lambda)),1) +'.X.idl'
	xfile2 = *info.cpath + '01.tb.' + *info.date + '.' + strtrim(string(fix(lambda)),1) +'.X.idl'

       ;mod LK Nov 2014, only calculate X if not already existing and mod for parallelization
        if file_test(xfile1) ne 1 then $
     ibis_polcal_xcalc_v4, FLOAT(lambda), polcal_datafile, *info.path1, SAVEFILE1 = xfile1, SAVEFILE2 = xfile2,$
          TFILENAME = tfile, FLAG_OLDTCAL = flag_oldtcal
  ;  ibis_polcal_xcalc_v2, FLOAT(lambda), polcal_datafile, *info.path1, SAVEFILE1 = xfile1, SAVEFILE2 = xfile2,$
  ;        TFILENAME = tfile, FLAG_OLDTCAL = flag_oldtcal


;restore,xfile1
;stop



;------------------------------------------------ choose quiet sun once for each directory
;### todo: keyword to redo
     ;if QS file does not exist, create it
           ndir = n_elements(*info.datadir)      ;total number of timestamps
            if file_test('qsreg.txt') ne 1 then begin 
              qsregion = fltarr(4,ndir) ;separate file for cont region, because wl-dependent
 ;             qsregion = fltarr(6,ndir)
              openw, 1, 'qsreg.txt'
              for i=0,ndir-1 do printf,1,qsregion[0,i],qsregion[1,i],qsregion[2,i],qsregion[3,i];,qsregion[4,i],qsregion[5,i]
              close,1
           endif

  print,'---- quiet Sun selection process ---'
  FOR fl=0,n_elements(opath)-1 do begin                                      ;selected timestamps
     scan_file = file_search(opath[fl] + '/' + lambda + '_nb???.sav', count = n) ;finds all files in chosen dirs
     currind = where(opath[fl] eq *info.rpath)
    ; currind = float(strmid(opath[fl],2,3,/rev)) ;example: get 001 from results_001
     print,'Directory:',opath[fl]
     
 ;    qsregion = fltarr(6,ndir)
     qsregion = fltarr(4,ndir)
     openr,1,'qsreg.txt'
     for ww=0,ndir-1 do begin    ;read qsreg.txt
        readf, 1, tmp1,tmp2,tmp3,tmp4;,tmp5,tmp6  ;last two ind are continuum indices
 ;       qsregion[*,ww]=[tmp1,tmp2,tmp3,tmp4,tmp5,tmp6]
        qsregion[*,ww]=[tmp1,tmp2,tmp3,tmp4]
     endfor
     close,1
   
;print,qsregion[*,currind]
;test if qs already defined
    if total(qsregion[0:3,currind]) eq 0 then begin

           restore,scan_file[0],/ve
           fov=[20,20,50,50]
           smwld = size(mwld)
           window,xsize=smwld[1],ysize=smwld[2]
           tvscl,mwld[*,*,0]
           loadct,3
           plots,[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],color=120,thick=2,/dev
           loadct,0
           print,'the preselected region of quiet Sun is drawn on the image'
           print,'press (a) to accept, (c) to change'
           if get_kbrd() eq 'c' then begin
              print,'select lower left then upper right'
              xy,/dev,count=2,pos=pos
              qsregion[0:3,currind] = [pos[0,0],pos[1,0],pos[0,1],pos[1,1]]
              fov = qsregion[*,currind]
              plots,[fov[0],fov[2],fov[2],fov[0],fov[0]],[fov[1],fov[1],fov[3],fov[3],fov[1]],color=150,thick=2,/dev
           endif

           openw, 1, 'qsreg.txt'
           for i=0,ndir-1 do printf,1,qsregion[0,i],qsregion[1,i],qsregion[2,i],qsregion[3,i] ;,qsregion[4,i],qsregion[5,i]
           close,1

      endif else begin  ;qs chosen already
         fov = qsregion[*,currind]
          


      endelse
    
	;--------------- end choice of qs region ------------------
	;---- begin choice of continuum and wl range --------------
  
 ;if continuum exists, then restore and plot it
        if file_test('cont_'+lambda+'.sav') then begin
           restore,'cont_'+lambda+'.sav'
           cregion = contregion[*,currind]
           ;only ask user for continuum if cregion is zero:
           if total(cregion) eq 0 then goto,changecont
       endif else begin
           contregion = fltarr(2,ndir)
           changecont:
           restore,scan_file[0],/ve ;may be duplicated effort, but file won't been restored yet if qs defined and cont isn't
           window,5
           avgprof = avg(avg(nbdwc[fov[0]:fov[2],fov[1]:fov[3],0,*],0),0)
           plot,avgprof
           print,'choose continuum (left then right border)'
           xy,pos=pos, count=2
           cregion = [round(pos[0,0]),round(pos[0,1])]
           xline,cregion,lines=1,thick=2
           contregion[*,currind] = cregion
           save,filename='cont_'+lambda+'.sav',contregion
           ;log this in .tex
           log_add_qscont,*info.path1,fov,mwld[*,*,0],avgprof,cregion,lambda,scan_file
       endelse
;do continuum selection for each timestamp because it could change
;with doppler shifts.

	;---- end choice of continuum and wl range ----
   
 
    
 ENDFOR ;end checking qs ROI for selected timestamps

 
;---- new 9.oct.13: do this for each directory
        FOR fl=0,n_elements(opath)-1 do begin

    	scan_file = file_search(opath[fl] + '/' + lambda + '_nb???.sav', count = n) ;finds all files in chosen directories
        outpath = opath[fl]

;	FOR i = 0,n-1 do BEGIN ;count of files
;		FOR k = 0,n_elements(opath)-1 DO BEGIN ;3 series;
;		mmm = strpos(scan_file[i],opath[k]);
;		IF mmm GT -1 THEN outpath = opath[k]+'/' ;chooses correct results_xxx
;		ENDFOR

        if (file_test(outpath+'/'+lambda+'*.tatb.sav')) then begin
        print,'At least one calibrated file found for wavelength: ',lambda, outpath
        print,'Continue (y,n)?'
        ans = ''
        read,ans
;        ftest = 1.
        if ans ne 'y' then goto,skip_ll
        endif

   
;read qs and wl range from file
;   currind = float(strmid(opath[fl],2,3,/rev)) ;example: get 001 from results_001
  currind = where(opath[fl] eq *info.rpath)
   qsregiontmp = fltarr(4,ndir)
 ;  cregiontmp = fltarr(2,ndir)
   openr,1,'qsreg.txt'
   for ww=0,ndir-1 do begin     ;read qsreg.txt
      readf, 1, tmp1,tmp2,tmp3,tmp4;,tmp5,tmp6
      qsregiontmp[*,ww]=[tmp1,tmp2,tmp3,tmp4]
   ;   cregiontmp[*,ww] = [tmp5,tmp6]
   endfor
   close,1

  ;choose correct qs and cont regions
   qsregion = qsregiontmp[*,currind]
   restore,'cont_'+lambda+'.sav'
   cregion = contregion[*,currind]

   ;; DO not DO residual  Xtalk correction. I commented it out in main_ibis_calibrate.
   threshold = [5.,5.,2.,2.]      ;; threshold values for residual xtalk
   fovarea = [10,40,240,465]      ;; solar FOV for residual xtalk
   ;     ftest = 0 ;flag for filetest
   wregion=[0,10]               ;not used later


     ;do polcal correction for each file in given directory for given wavelength
For aa=0,n-1 do main_ibis_calibrate_v2, lambda, *info.date, cregion, wregion, threshold, fovarea, scan_file[aa], tfile, xfile1, xfile2, outpath,region=qsregion


skip_ll:
     ENDFOR ;end fl (each directory)

     endfor                     ;end wl

print,'-------- polcal done. THE END! :-) -----------------------------'
endif



;----------------------------- reduce imaging spectral scan  ---------------------
if nr eq 155 then begin  ;keyword overwrite
     if *info.button15b_state eq 0 then *info.button15b_state = 1 else *info.button15b_state = 0
endif


if nr eq 15 then begin
  if *info.flagncam ne 1 then message,'Does not work with old camera!'
  print,'reducing imaging spectral scans'

  if *info.button15b_state eq 1 then $
  read_imagingspecscan,*info.path,*info.cpath,*info.path1,/overwrite else read_imagingspecscan,*info.path,*info.cpath,*info.path1
  ;output is saved in cpath as imgspecscan_{wl}.sav


print,'-------- Imaging spectral scans reduced -----------------------------'
endif

;----------------------------- get prefilter profile  ---------------------
if nr eq 16 then begin

  if *info.flagncam ne 1 then message,'Does not work with old camera!'
 
  print,'calculating prefilter profile'
 
  ;files such as prefilter.profile.6302.Sep2011.sav must be present in the GUI directory
  ;these files are done by Kevin. They contain the prefilter profile from the lamp tunings
  ;but with an arbitrary wavelength scale because of the different optical paths
  ; this program finds the best wl shift for the prefilter by crosscorrelations
 
  ;find the path to where ibis_gui is located. prefilters are in the same directory.
        routinepaths = (routine_info(_EXTRA=_extra,/source)).path
        index = where(strmatch(routinepaths,'*ibis_gui_new.pro',/fold_case) eq 1)
        pathtolib = routinepaths[index[0]]
        posibis = strpos(pathtolib,'ibis_gui_new')
        pathtolib = strmid(pathtolib,0,posibis)

        ;log
        newtext=strarr(2)
        time=systime()
        newtext[0] = '\textbf{Calculating prefilter profile: ibis\_corr\_prefilter.pro}\\'
        newtext[1] = string(time)+'\\'
        log_write,*info.path1,newtext

  for ll=0,n_elements(*info.la)-1 do begin ;for all specified wavelengths
	lambda = (*info.la)[ll]
        ibis_corr_prefilter,*info.cpath,*info.path1,wlobs=lambda,pathtolib=pathtolib

  endfor
  
  newtext = strarr(1) ;new entries for log
  newtext[0]=' '
  log_write,*info.path1,newtext

  print,'----------------------------------------------------------------------------'
  print,'Prefilter profiles done. Check the last page of the ps files! only tested: '
  print,'     6302,8542,6563,5896,6173,5434' 
  print,'----------------------------------------------------------------------------'

;### todo: update flatfield
;### todo: add check for prefilter corrected flat

endif

;----------------------------- do prefilter correction  ---------------------
if nr eq 17 then begin
;only done for selected directories

 if *info.flagncam ne 1 then message,'Does not work with old camera!'
 
;log
         newtext=strarr(2)
        time=systime()
        newtext[0] = '\textbf{Correcting observation for prefilter: apply\_prefilter.pro}\\'
        newtext[1] = string(time)+'\\'
        log_write,*info.path1,newtext


 for ll=0,n_elements(*info.la)-1 do begin ;for all specified wavelengths
	lambda = (*info.la)[ll]
 
   ;--- find out which timestamp to correct -----
   if (n_elements(*info.datadir2) eq 0) then begin ;if datadir2 undefined
       if total(*info.datptr) eq 0 then begin
       print,'choose which directories to reduce!'
       return
       endif
       ind = where((*info.datptr) eq 1)
       *info.datadir2 = (*info.datadir)[ind]  ;new, shorter 
   endif
     ind = where((*info.datptr) eq 1) 
     if ind[0] eq -1 then begin
     print,'choose which directories to reduce!'
     return
     endif

   currpath = (*info.rpath)[ind] ;path to results_xxx

    if (*info.datadir2)[0] eq 'temp' then begin ;if datadir2 wrongly defined
     print,'choose which directories to reduce!'
     return
    endif

   
    ;stokesout is overwritten with the corrected stokesout
    apply_prefilter,*info.cpath,*info.path1, lambda=lambda,dir=currpath

 endfor

print,'-------- Observations corrected for prefilter transmission----------------------'
endif

;----------------------------- do prefilter correction for flat  ---------------------
if nr eq 18 then begin
;only done for selected directories

 if *info.flagncam ne 1 then message,'Does not work with old camera!'
 
;log
        newtext=strarr(2)
        time=systime()
        newtext[0] = '\textbf{Correcting flat for prefilter: flat\_prefilter.pro}\\'
        newtext[1] = string(time)+'\\'
        log_write,*info.path1,newtext


 for ll=0,n_elements(*info.la)-1 do begin ;for all specified wavelengths
	lambda = (*info.la)[ll]
   
    ;gain is overwritten with the corrected gain
    flat_prefilter,*info.cpath,lambda=lambda

 endfor

print,'-------- Flat corrected for prefilter transmission----------------------'
endif


;------------------- close gui -----------------------------------------------
IF nr eq 100 THEN begin
   ;only save structure if first button was pressed
   if (*info.cpath)[0] ne '' then save,info,filename='info_'+*info.date+'.sav'
   print,'GUI closed'
   WIDGET_CONTROL, ev.TOP, /DESTROY  
ENDIF


END


;------------------------------------------------------------------------------------------------
PRO ibis_gui_new,file=file,xoffset=xoffset,yoffset=yoffset

;define pointers
date   =  ptr_new('20111014')
path   =  ptr_new('/Users/kleintl/sanhome/ibis/14oct11/ibis/')
path1  =  ptr_new('/Users/kleintl/sanhome/ibis/reduc/20111014test/')
wpath  =  ptr_new('/Users/kleintl/sanhome/ibis/14oct11/whitelight/')
polpath = ptr_new('')
la      = ptr_new(['8542 6302'])
spath = ptr_new('') & spath1 = ptr_new('') & cpath = ptr_new('') & speckle_dir = ptr_new('')
wldbfile = ptr_new('') & nbdbfile = ptr_new('') & tpdbfile = ptr_new('') & tppdbfile = ptr_new('')
sdbfile = ptr_new('') & snbdbfile = ptr_new('') & wdfile = ptr_new('') & wffile = ptr_new('')
wlffile = ptr_new('') & wldfile = ptr_new('') & dcdir = ptr_new('') 
griddir = ptr_new('') & tardir = ptr_new('') & poldir = ptr_new('')
ts = ptr_new('') & upath = ptr_new('') & rpath = ptr_new('') & flagncam = ptr_new(1)
kerneltxt = ptr_new('[0,64,32,16]')
 

;--- pointer for button states
  button2b_state=ptr_new(0) 
  button2c_state=ptr_new(0) 
  button2d_state = ptr_new(1) 
  button4b_state=ptr_new(0)
  button6a_state=ptr_new(0)
  button8b_state = ptr_new(1) ;cog fit by default 
  button9b_state=ptr_new(0)
  button11c_state = ptr_new(0)
  button11d_state = ptr_new(0) 
  button12b_state=ptr_new(0) 
  button12c_state=ptr_new(0) 
  button13b_state=ptr_new(0) 
  button15b_state = ptr_new(0) 
  text12c = ptr_new(0)  
  exclcal = ptr_new(strarr(4)) ;to save exclude series
    
;need to have datadir defined at the beginning of program and then rebuild widget when it is changed
  datadir=ptr_new(['temp'])
  datadir2 = ptr_new('')
  ffdir= ptr_new(['temp'])
  polvals = ptr_new(strarr(4))
  flag = ptr_new(0)                       ;flag to label the automatic polcal buttons
  nfl = n_elements(ffdir) 
  ffptr = ptr_new(intarr(nfl))
 
  ndat = n_elements(datadir)
  datptr = ptr_new(intarr(ndat))

 
info = {date:date,path:path,path1:path1,wpath:wpath,polpath:polpath,la:la,spath:spath,spath1:spath1,cpath:cpath,speckle_dir:speckle_dir,$
        wldbfile:wldbfile,nbdbfile:nbdbfile,tpdbfile:tpdbfile,tppdbfile:tppdbfile,sdbfile:sdbfile,snbdbfile:snbdbfile,wdfile:wdfile,$
        wffile:wffile, wlffile:wlffile, wldfile:wldfile, dcdir:dcdir, ffdir:ffdir, datadir:datadir, griddir:griddir, tardir:tardir, $
        poldir:poldir, ts:ts, upath:upath,rpath:rpath,flagncam:flagncam,datadir2:datadir2,$
        button2b_state:button2b_state,button2c_state:button2c_state,button2d_state:button2d_state, button4b_state:button4b_state,$
        button6a_state:button6a_state,button8b_state:button8b_state,button9b_state:button9b_state,button11c_state:button11c_state,$
        button11d_state:button11d_state,button12b_state:button12b_state, button12c_state:button12c_state,text12c:text12c,$
        button13b_state:button13b_state, button15b_state:button15b_state,$
        kerneltxt:kerneltxt,polvals:polvals,flag:flag,ffptr:ffptr,datptr:datptr,exclcal:exclcal}
 
;restore files if available
if keyword_set(file) then begin
if file_test(file) then restore,file
*info.ffptr = intarr(n_elements(*info.ffdir))
*info.datptr = intarr(n_elements(*info.datadir))
endif

;--- main window
;window with scroll bars if smaller than screen...
scrsz=get_screen_size()
xs=scrsz(0) & ys=scrsz(1)

if ~keyword_set(xoffset) then xoffset=0
if ~keyword_set(yoffset) then yoffset=0

if (xs ge 800 and ys ge 950) then begin
guiwindow = widget_base(title='IBIS data reduction, v20150621',/column,/align_center,x_scroll_size=800,y_scroll_size=950,$
            /scroll, xoffset=xoffset, yoffset=yoffset) 
endif else begin
if xs lt 800 then xwind = xs else xwind = 800
if ys lt 950 then ywind = ys else ywind = 950

guiwindow = widget_base(title='IBIS data reduction, v20150621',/column,/align_center,x_scroll_size=xwind,y_scroll_size=ywind*.9,$
            /scroll, xoffset=xoffset, yoffset=yoffset)
endelse
;--- end main window


;--------- variable section  ---------------
 wind1 = widget_base(guiwindow,/row)
 button1 = widget_button(wind1, value='variables', uvalue=1, scr_xsize=120) 
 label=widget_label(wind1,value='execute the first button for each new IDL session to set paths',/align_right)

 wind2b = widget_base(guiwindow,/row)
; label=widget_label(wind2b,value='observation type: ',/align_right)
 button2b = cw_bgroup(wind2b,['polarimetry','spectroscopy (=no LCVR and beamsplitter).'],/exclusive,uvalue=22, /row, set_value=*button2b_state,/no_release)
 label=widget_label(wind2b,value='If LCVR present, choose polarimetry even if only one pol state.',/align_right)

 wind2c = widget_base(guiwindow,/row)
 button2c = cw_bgroup(wind2c,['dual beam','single beam (required for spectroscopy)'],/exclusive,uvalue=23, /row, set_value=*button2c_state,/no_release)
 
 wind2d = widget_base(guiwindow,/row)
 label=widget_label(wind2d,value='Parallelization (through bridges):',/align_left)
 button2c = cw_bgroup(wind2d,['yes','no'],/exclusive,uvalue=24, /row, set_value=*button2d_state,/no_release)


 horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=2)



;------------ white light database ibis_wl_db_new --------------------------
;may not be needed
; wind3 = widget_base(guiwindow,/row)
; button3 = widget_button(wind3, value='wl database', uvalue=3, scr_xsize=120)
; label=widget_label(wind3,value='create whitelight database with filenames, extensions, time stamps etc.',/align_right)




;------------ white light dark & flat --------------------------
 wind4 = widget_base(guiwindow,/row)
 button4 = widget_button(wind4, value='wl dark and flat', uvalue=4, scr_xsize=120)
 button4b = cw_bgroup(wind4, ['overwrite.'],/nonexc,uvalue = 44, set_value=*info.button4b_state)
 txt = 'Create mean wl dark and flat. Use overwrite to set keyword if re-doing reduction.'
 label=widget_label(wind4,value=txt,/align_right)




;------------ nb & wl alignment --------------------------
 wind5 = widget_base(guiwindow,/row)
 button5 = widget_button(wind5, value='nb and wl align', uvalue=5, scr_xsize=120)
 label=widget_label(wind5,value='alignment. Is performed for all observed lambdas.',/align_right)

horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)


;------------ nb dark & flat --------------------------
 wind6 = widget_base(guiwindow,row=1)
 button6 = widget_button(wind6, value='nb dark and flat', uvalue=6, scr_xsize=120, scr_ysize=5,ysize=5)
 button6a = cw_bgroup(wind6, ['overwrite.'],/nonexc,uvalue = 65, set_value=*info.button6a_state)
 
 wind6b = widget_base(wind6,/column)
 label=widget_label(wind6b,value='select flat scan(s):',/align_right)


 fftext=ptr_new(strarr(nfl)+'0')
 wind6c = widget_base(guiwindow,row=nfl)
 button6b = cw_bgroup(wind6c,strmid(*info.ffdir,14,15,/rev),/nonexc,uvalue=66,/row, set_value=*info.ffptr)
 
 horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)



;------------ blueshift calc. --------------------------
 wind7 = widget_base(guiwindow,row=1)
 button7 = widget_button(wind7, value='calc. blueshift', uvalue=7, scr_xsize=120)
 label=widget_label(wind7,value='blueshift for chosen lambda(s)',/align_right)
 button7b = widget_button(wind7, value='show blueshift', uvalue=77, scr_xsize=120)



;------------ nb gain --------------------------
 wind8 = widget_base(guiwindow,row=1)
 button8 = widget_button(wind8, value='nb gain table', uvalue=8, scr_xsize=120)
 label=widget_label(wind8,value='ibis_gain.pro. Blueshift method: ',/align_right)
 button8b = cw_bgroup(wind8,['poly_fit','cog'],/exclusive,uvalue=88, /row, set_value=*info.button8b_state)


;------------ speckle --------------------------
 wind9 = widget_base(guiwindow,row=1)
 button9 = widget_button(wind9, value='prepare speckle', uvalue=9, scr_xsize=120)
 button9b = cw_bgroup(wind9,'limb data (experimental!)?',/nonexc,uvalue=98,/row, set_value=*info.button9b_state)

 horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='1. run kisip code... ',$ ;long line
      frame=1,scr_ysize=15)
 
 button9c = widget_button(horizline, value='Open speckle GUI', uvalue=95, scr_xsize=120)

;------------ speckle 2--------------------------
 wind10 = widget_base(guiwindow,row=1)
 button10 = widget_button(wind10, value='speckle to idl', uvalue=10, scr_xsize=120)
 label=widget_label(wind10,value='transform fortran output back to idl format.',/align_right)


;------------ speckle destretch--------------------------
 wind10b = widget_base(guiwindow,row=1)
 button10b = widget_button(wind10b, value='destretch speckle', uvalue=101, scr_xsize=120)
 label=widget_label(wind10b,value='destretch each speckle img wrt to previous image',/align_right)

horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)

;------------ imag. spec scan -------------------------------------------------

 wind15 = widget_base(guiwindow,row=1)
 button15 = widget_button(wind15, value='reduce spec scan', uvalue=15, scr_xsize=120)
 button15b = cw_bgroup(wind15, ['overwrite.'],/nonexc,uvalue = 155, set_value=*info.button15b_state)
 label=widget_label(wind15,value='reduce imaging spectral scans (for prefilter corr)',/align_right)

;------------ calc prefilter  -------------------------------------------------

 wind16 = widget_base(guiwindow,row=1)
 button16 = widget_button(wind16, value='calc prefilter', uvalue=16, scr_xsize=120)
 label=widget_label(wind16,value='find prefilter profile, shift to corr wl and save',/align_right)


;------------  prefilter to flat -------------------------------------------------

 wind18 = widget_base(guiwindow,row=1)
 button18 = widget_button(wind18, value='prefilter -> flat', uvalue=18, scr_xsize=120)
 label=widget_label(wind18,value='incorporate prefilter into flat/gain',/align_right)

horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)


;------------ ibis_combine --------------------------
 wind11 = widget_base(guiwindow,row=1)
 button11 = widget_button(wind11, value='combine all', uvalue=11, scr_xsize=120)
 button11c = cw_bgroup(wind11, ['overwrite.'],/nonexc,uvalue = 112, set_value=*info.button11c_state)
 button11d = cw_bgroup(wind11, ['movie.'],/nonexc,uvalue = 113, set_value=*info.button11d_state)
 
 label=widget_label(wind11,value='Choose dataseries (more than 1 possible):',/align_right)

 wind11b = widget_base(guiwindow,row=n_elements(*info.datptr))
 nrows = 6 ;maximally 6 timestamps next to each other, also used in event
 button11b = cw_bgrid_lk(wind11b,strmid(*info.datadir,14,15,/rev),/nonexc,uvalue=111,rows=nrows,set_value=*info.datptr)
;cw_bgrid_lk arranges the buttons in rows and if datadir has more than
;6 elements, then in subsequent rows

!path=!path+':'+programrootdir()
RESOLVE_ROUTINE,'reg', /COMPILE_FULL_FILE,/either

horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)


;------- new 140306: shift for atmospheric refraction ---------
 wind11c = widget_base(guiwindow,row=1)
 button11c = widget_button(wind11c, value='shift NB on WL', uvalue=114, scr_xsize=120)
 label=widget_label(wind11c,value='Corrects for shifts, e.g. due to atmospheric dispersion.',/align_right)


;------------ reduce ibis data ------------------------------
 wind12 = widget_base(guiwindow,row=1)
 button12 = widget_button(wind12, value='reduce IBIS', uvalue=12, scr_xsize=120)
 label=widget_label(wind12,value='saves scans. end for spectroscopy.',/align_right)
 button12b = cw_bgroup(wind12,'limb data (experimental!)?',/nonexc,uvalue=122,$
             /row, set_value=*info.button12b_state)

 wind12c = widget_base(guiwindow,row=1)
 label=widget_label(wind12c,value='reconstruction type: ',/align_right)
 button12c = cw_bgroup(wind12c,['speckle','avg','seq.'],/exclusive,uvalue=123, /row, set_value=*info.button12c_state)
 label=widget_label(wind12c,value='kernel (start with 0): ',/align_right)
 *info.text12c = Widget_Text(wind12c, /Editable, /align_left,Value=*info.kerneltxt,$
        scr_xsize=110,scr_ysize=30,uvalue=124)


;### also i=0,n-1 should be interactive to select range of scans to reduce
horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)


;------------ polcal -----------------------------------------
 wind13b = widget_base(guiwindow,row=1)

 if *info.button13b_state eq 1 then begin  ;manual selected
    label=widget_label(wind13b,value='Manual: choose sequences to exclude (pos 330/15/60/105, no commas):',/align_right)
    text13 = fltarr(4)
    for i=0,3 do text13[i] = Widget_Text(wind13b, /Editable, /align_left,Value=(*info.exclcal)[i],$
                                         scr_xsize=50,scr_ysize=30,uvalue=134+i)
 endif 

 if *info.button13b_state eq 0 then begin ;auto selected
                                ;automatic calibration: if aborted, there will be more than 4*28 directories -> choose manually
    wind13c = widget_base(guiwindow,row=1)
    label=widget_label(wind13c,value='Choose start of seq (330/15/60/105) or of fewer pos:',/align_right)
    button13c = fltarr(4)
    npolcal = round(n_elements(*info.poldir)/28.)<4 ;max 4 table positions
                                ;label automatic cal buttons. first run of gui: 'N/A', after variables are set: [0], [28] etc.
;if buttons are clicked -> actual chosen values
    if *info.flag ne 1 then begin
       if (*info.poldir)[0] eq '' then begin
          for i=0,3 do (*info.polvals)[i] = 'N/A'
       endif else begin
          for i=0,npolcal-1 do (*info.polvals)[i] = strmid((*info.poldir)[i*28],14,15,/rever) 
          if npolcal lt 4 then for i=npolcal,3 do  (*info.polvals)[i] = 'N/A' ;not all 4 tablepos
          *info.flag=1
       endelse
    endif
    for i=0,3 do button13c[i] = widget_button(wind13c,Value=(*info.polvals)[i], uvalue=134+i,$
                                              scr_xsize=115)
 endif 

 wind13 = widget_base(guiwindow,row=1)
 button13 = widget_button(wind13, value='find polcal', uvalue=13, scr_xsize=120)
 label=widget_label(wind13,value='collect measurements from diff. table positions (for all lambdas).',/align_right)
 button13b = cw_bgroup(wind13,['Automatic','Manual'],/exclusive,uvalue=133, /row, set_value=*info.button13b_state)


 horizline = widget_base(guiwindow,/row)
 tmp = widget_label(horizline,$
      value='                                                                                         ',$ ;long line
      frame=1,scr_ysize=1)

;------------ polcal 2-------------------------------------------------
 wind14 = widget_base(guiwindow,row=1)
 button14 = widget_button(wind14, value='do polcal', uvalue=14, scr_xsize=120)
 label=widget_label(wind14,value='make light curves, X and T and final correction for scans (for specified lambdas).',/align_right)



;------------ do prefilter correction ---------------------------------------------
;now done during flatfielding

; wind17 = widget_base(guiwindow,row=1)
; button17 = widget_button(wind17, value='do prefilter corr', uvalue=17, scr_xsize=120)
; label=widget_label(wind17,value='do prefilter correction (for selected wl and directories)',/align_right)





;-------------- QUIT -------------------------
 wind100 = widget_base(guiwindow,/row)
 button100 = widget_button(wind100, value=$
      ColorButtonBitmap('QUIT & CLOSE', FGCOLOR='RED', BGCOLOR='GREY'), uvalue=100, scr_xsize=120)

WIDGET_CONTROL, /REALIZE, guiwindow, set_uvalue=info
XMANAGER, 'ibis_gui_new', guiwindow


END
