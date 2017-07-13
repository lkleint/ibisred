;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Main procedure to calibrate the IBIS observations for darks
;; gain, blueshift and image distortions (destretch). The procedure
;; uses the output of ibis_combine.pro as input data.
;;
;; ref:      reference image(s)
;;    	    if equal to a scalar > 0, then a running mean is used
;; data:     data cube do be destreched
;; ks:       vector with kernel sizes, in the order the alignment is to be done
;;           rigid alignment corresponds to a zero, needs to be the first index
;; sfp:      2x2 array that describes lower left and upper right positions
;;           within which the 'rigid' alignment is to be done
;;           e.g. sfp = [ [xll, yll], [xur, yur] ]
;; dp:       detrend parameter
;; 
;; shifts:   return shifts on finest grid, if desired
;; grid:     return finest grid, if desired
;; 
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION destretch_ibis, ref, data, ks, sfp, dp, SHIFTS = shifts, GRID = grid, idlbridge = idlbridge
pr=0 ;print debug info
IF KEYWORD_SET(idlbridge) THEN BEGIN

    ;#################################################################
    ; FIND IDL BRIDGES
    ;#################################################################
    
    HELP,idlbridge
    bridges_destr = idlbridge
    ncpus = N_ELEMENTS(bridges_destr)

    ;#################################################################

    ;;***************************************************************
    ;; copy data
    ;;***************************************************************

    cube = data
    
    ;***************************************************************
    ; dimension of ref
    ;***************************************************************
    
    dimr = SIZE(ref)
    
    IF (dimr[0] LT 2) THEN BEGIN
        
        dimr = [dimr[0], 1, 1, 1]
        tr = ref
        
    ENDIF
    
    IF (dimr[0] EQ 2) THEN dimr = [dimr[0:2], 1]
    
    ;***************************************************************
    ; dimension of cube
    ;***************************************************************

    dim = SIZE(cube)
    IF (dim[0] LT 2) THEN message, '2D or 3D data required as input.'
    IF (dim[0] EQ 2) THEN dim = [dim[0:2], 1]
    IF (dim[0] GT 3) THEN message, '2D or 3D data required as input.'
    
    ;***************************************************************
    ; computations for multi-reference destretch
    ;***************************************************************

    ;#images to destretch divided by #reference images
    nim1 = dim[3]/dimr[3]	; even number
    nim2 = dim[3] MOD nim1	; remainder, usually 0
    
    ;***************************************************************
    ; flag for combination of shifts
    ;***************************************************************

    flag = 0

    ;***************************************************************
    ; rigid alignment, if wanted
    ;***************************************************************

    IF (ks[0] eq 0) THEN BEGIN ;0 means rigid alignment
        
        sft = FLTARR(2, 1, 1, dim[3])
        
        ;***************************************************************
        ; do the first chunk with the even numbers
        ;***************************************************************

        FOR cpu = 0,ncpus-1 DO BEGIN
            (bridges_destr[cpu])->SetVar, "i",-1 
            (bridges_destr[cpu])->SetVar, "j",-1 
        ENDFOR
        
        FOR j = 0, dimr[3]-1 DO BEGIN

            FOR i = j*nim1, (j+1)*nim1-1 DO BEGIN
                IF (dimr[0] LT 2) THEN IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]
                
                bridge =  GET_IDLE_BRIDGE(bridges_destr)
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN  sft[*,0,0,iold] = bridge->GetVar('result')
                IF (iold ne -1) THEN  cube[0,0,iold] = shift_bicub(cube[*,*,iold], sft[0,0,0,iold], sft[1,0,0,iold])
                bridge->SetVar,  "i",i
                bridge->SetVar,  "j",j
                bridge->SetVar,  'ref_j',ref[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], j]
                bridge->SetVar,  "cube_i",  cube[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], i]
                bridge->Execute, 'result = shc( ref_j, cube_i ,/INTERP)',/NOWAIT ;nowait will cause problems with the GUI in case you used control+c before (=command line visible). In that case, you must run destretch_ibis twice by repeating the click on 'reduce IBIS'.

            ENDFOR
            
            BARRIER_BRIDGES,bridges_destr
            
            FOR cpu = 0,ncpus-1 DO BEGIN
                iold = (bridges_destr[cpu])->GetVar('i')
                IF (iold ne -1) THEN  sft[*,0,0,iold] = (bridges_destr[cpu])->GetVar('result')
                IF (iold ne -1) THEN  cube[0,0,iold] = shift_bicub(cube[*,*,iold], sft[0,0,0,iold], sft[1,0,0,iold])              
            ENDFOR
            
        ENDFOR

        ;***************************************************************
        ; do the remainder, if neccessary
        ;***************************************************************

        PRINT,' WORKING ON REMAINDER '
        ;usually =0 for IBIS
        IF (nim2 GT 0) THEN BEGIN
            
            FOR cpu = 0,ncpus-1 DO BEGIN
                (bridges_destr[cpu])->SetVar, "i",-1 
                (bridges_destr[cpu])->SetVar, "j",-1 
            ENDFOR
            
            FOR i = dimr[3]*nim1, dim[3]-1 DO BEGIN
                
                IF (dimr[0] LT 2) THEN IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]
                
                bridge =  GET_IDLE_BRIDGE(bridges_destr)
                iold = bridge->GetVar('i')
                IF (iold ne -1) THEN  sft[*,0,0,iold] = bridge->GetVar('result')
                IF (iold ne -1) THEN  cube[0,0,iold] = shift_bicub(cube[*,*,iold], sft[0,0,0,iold], sft[1,0,0,iold])
                              
                bridge->SetVar,  "i",i
                bridge->SetVar,  "j",j
                bridge->SetVar,  "ref_remain", ref[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], dimr[3]-1]
                bridge->SetVar,  "cube_i", cube[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], i]
                
                bridge->Execute, "result = shc( ref_remain,cube_i, /interp )",/NOWAIT
                
            ENDFOR  
            
            BARRIER_BRIDGES,bridges_destr
            
            FOR cpu = 0,ncpus-1 DO BEGIN
                iold = (bridges_destr[cpu])->GetVar('i')
                IF (iold ne -1) THEN  sft[*,0,0,iold] =  (bridges_destr[cpu])->GetVar('result')
                IF (iold ne -1) THEN  cube[0,0,iold] = shift_bicub(cube[*,*,iold], sft[0,0,0,iold], sft[1,0,0,iold])              
            ENDFOR
                 
        ENDIF
        
        flag = 1
        rt = -1
        IF (N_ELEMENTS(ks) eq 1) THEN BEGIN  ;if only rigid alignment
            IF arg_present(shifts) THEN shifts = sft
            RETURN, cube 
        ENDIF ELSE ks = ks[1:*]  ;otherwise delete 0 from [0,64,24,..] vector

     ENDIF

     ;***************************************************************
     ; dimension of (remaining) kernel size vector
     ;***************************************************************

     run = (SIZE(ks,/dimensions))[0]
     
     ;***************************************************************
     ; destretch
     ;***************************************************************
         
     PRINT,' using idl bridges to calculate destretch'

     FOR j = 0, run-1 DO BEGIN ;for each field size (rigid case was deleted above)

         PRINT,' performing destretch of kernel ',j,' OF ',run-1

         ; get shifts
   
         kernel = BYTARR(ks[j], ks[j])
         r      = mkcps(ref, kernel)
                  
         IF (dimr[0] LT 2) THEN BEGIN
             
             shft = FLTARR((SIZE(r))[1], (SIZE(r))[2], (SIZE(r))[3], dim[3])    
             
             FOR cpu = 0,ncpus-1 DO BEGIN
                 (bridges_destr[cpu])->SetVar, "i",-1  
                 (bridges_destr[cpu])->SetVar,  "kernel",kernel
                 (bridges_destr[cpu])->Execute, "dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))"
                 (bridges_destr[cpu])->Execute, "forward_function doreg, mkcps, repair"
             ENDFOR
             
             FOR i = 0, dim[3]-1 DO BEGIN
                 
                 bridge =  GET_IDLE_BRIDGE(bridges_destr)
                 iold = bridge->GetVar('i')
                 IF (iold ne -1) THEN shft[*,*,*,iold] = bridge->GetVar('result')

                 IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]

                 bridge->SetVar,  "i",i
                 bridge->SetVar,  "ref_i",ref
                 bridge->SetVar,  "cube_i",cube[*,*,i]
                 resolve_routine,'reg',/is_fun
                 resolve_routine,'cps',/is_fun
                 bridge->Execute, "result = cps(cube_i, ref_i, kernel)",/NOWAIT

             ENDFOR
             
             BARRIER_BRIDGES,bridges_destr
            
             FOR cpu = 0,ncpus-1 DO BEGIN
                 iold = (bridges_destr[cpu])->GetVar('i')
                 IF (iold ne -1) THEN shft[*,*,*,iold] =  (bridges_destr[cpu])->GetVar('result')
             ENDFOR
             
         ENDIF ELSE IF (dimr[3] gt 1) THEN BEGIN
             
             shft = fltarr((SIZE(r))[1], (SIZE(r))[2], (SIZE(r))[3], dim[3])
             
             FOR cpu = 0,ncpus-1 DO BEGIN
                 (bridges_destr[cpu])->SetVar, "i",-1  
                 (bridges_destr[cpu])->SetVar,  "kernel",kernel
                 (bridges_destr[cpu])->Execute, "dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))"
                 (bridges_destr[cpu])->Execute, "forward_function doreg, mkcps, repair"
             ENDFOR
             
             FOR i = 0, dimr[3]-2 DO BEGIN
                 
                 bridge =  GET_IDLE_BRIDGE(bridges_destr)
                 iold = bridge->GetVar('i')
                 IF (iold ne -1) THEN shft[*,*,*,iold*nim1:(iold+1)*nim1] = bridge->GetVar('result')
                 
                 bridge->SetVar,  "i",i
                 bridge->SetVar,  "ref_i",REFORM(ref[*,*,i])
                 bridge->SetVar,  "cube_i",REFORM(cube[*,*,i*nim1:(i+1)*nim1])
                 
                 bridge->Execute, "result = cps(cube_i,ref_i, kernel)",/NOWAIT
                 
             ENDFOR
             
             BARRIER_BRIDGES,bridges_destr
             
             FOR cpu = 0,ncpus-1 DO BEGIN
                 iold = (bridges_destr[cpu])->GetVar('i')
                 IF (iold ne -1) THEN  shft[*,*,*,iold*nim1:(iold+1)*nim1] =  (bridges_destr[cpu])->GetVar('result')
             ENDFOR
             
             ; remainder

             shft[*,*,*,(dimr[3]-1)*nim1:*] = cps(REFORM(cube[*,*,(dimr[3]-1)*nim1:*]), $ 
                                                  REFORM(ref[*,*,dimr[3]-1]), kernel)
             
         ENDIF ELSE BEGIN  
             
             i=0
             forward_function doreg, mkcps, repair
             dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))
             shft_test = cps(cube[*,*,i],ref,kernel)
             dshft = SIZE(shft_test,/DIMENS)
             shft = FLTARR([dshft,dim[3]])
             
             FOR cpu = 0,ncpus-1 DO BEGIN
                 (bridges_destr[cpu])->SetVar, "i",-1  
                 (bridges_destr[cpu])->SetVar,  "kernel",kernel
                 (bridges_destr[cpu])->Execute, "dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))"
                 (bridges_destr[cpu])->Execute, "forward_function doreg, mkcps, repair"
             ENDFOR
             
             FOR i=0,dim[3]-1 DO BEGIN
                 
                 bridge =  GET_IDLE_BRIDGE(bridges_destr)
                 iold = bridge->GetVar('i')
                 IF (iold ne -1) THEN shft[*,*,*,iold] =  bridge->GetVar('result')
                 
                 bridge->SetVar,  "i",i
                 bridge->SetVar,  "ref",ref
                 bridge->SetVar,  "cube_i",cube[*,*,i]
                 
                 bridge->Execute, "result = cps(cube_i, ref, kernel)",/NOWAIT
                 
             ENDFOR
             
             BARRIER_BRIDGES,bridges_destr
             
             FOR cpu = 0,ncpus-1 DO BEGIN
                 iold = (bridges_destr[cpu])->GetVar('i')
                 IF (iold ne -1) THEN  shft[*,*,*,iold] =  (bridges_destr[cpu])->GetVar('result')
             ENDFOR
             
         ENDELSE
         
         ; detrend shifts

         IF (dp NE 0) THEN shft = detrendshifts(shft, r, dp)
         
         ; register
         
         PRINT,' REGISTERING IMAGES '

         FOR cpu = 0,ncpus-1 DO BEGIN
             (bridges_destr[cpu])->SetVar, "i",-1  
             (bridges_destr[cpu])->Execute, "dummy = reg(findgen(100,100), findgen(100,100), bytarr(12,12))"
             (bridges_destr[cpu])->Execute, "forward_function doreg, mkcps, repair"
         ENDFOR
         
         FOR i = 0, dim[3]-1 DO BEGIN
             
             bridge =  GET_IDLE_BRIDGE(bridges_destr)
             iold = bridge->GetVar('i')
             IF (iold ne -1) THEN cube[*,*,iold] =  bridge->GetVar('result')
             
             bridge->SetVar,  "i",i
             bridge->SetVar,  "cube_i",cube[*,*,i]
             bridge->SetVar,  "shft_i",shft[*,*,*,i]
             bridge->SetVar,  "r",r
             
             bridge->Execute, "result = doreg(cube_i, r,shft_i)",/NOWAIT        
             
         ENDFOR    
         
         BARRIER_BRIDGES,bridges_destr
         
         FOR cpu = 0,ncpus-1 DO BEGIN
             iold = (bridges_destr[cpu])->GetVar('i')
             IF (iold ne -1) THEN cube[*,*,iold] =  (bridges_destr[cpu])->GetVar('result')
             (bridges_destr[cpu])->Execute, "WHILE (!d.window ne -1) DO WDELETE"
         ENDFOR
         
        ; combine shifts
         IF (flag EQ 1) THEN BEGIN
             
             sft = combineshifts(dim[1], dim[2], rt, r, sft, shft)
             rt  = r
             
         ENDIF
         
         flag = 1
         
     ENDFOR
     
     IF arg_present(shifts) THEN shifts = sft
     IF arg_present(grid)   THEN grid   = r
     
 ENDIF ELSE BEGIN  ;no bridges:

;;***************************************************************
;; copy data
;;***************************************************************

     cube = data
     
;;***************************************************************
;; dimension of ref
;;***************************************************************

     dimr = SIZE(ref)

     IF (dimr[0] LT 2) THEN BEGIN

        dimr = [dimr[0], 1, 1, 1]
        tr = ref

     ENDIF

     IF (dimr[0] EQ 2) THEN dimr = [dimr[0:2], 1]

;;***************************************************************
;; dimension of cube
;;***************************************************************

     dim = SIZE(cube)
     IF (dim[0] LT 2) THEN message, '2D or 3D data required as input.'
     IF (dim[0] EQ 2) THEN dim = [dim[0:2], 1]
     IF (dim[0] GT 3) THEN message, '2D or 3D data required as input.'

;;***************************************************************
;; computations for multi-reference destretch
;;***************************************************************

     nim1 = dim[3]/dimr[3]	; even number
     nim2 = dim[3] MOD nim1	; remainder

;;***************************************************************
;; flag for combination of shifts
;;***************************************************************

     flag = 0

;;***************************************************************
;; rigid alignment, if wanted
;;***************************************************************

     IF (ks[0] eq 0) THEN BEGIN

        sft = FLTARR(2, 1, 1, dim[3])

;;***************************************************************
;; do the first chunk with the even numbers
;;***************************************************************
;probably this is constant offset adjustment
        FOR j = 0, dimr[3]-1 DO BEGIN  ;number of images
            FOR i = j*nim1, (j+1)*nim1-1 DO BEGIN

                IF (dimr[0] LT 2) THEN IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]

                ;linear subpixel-shift by fourier crosscorrelation
                sft[*,0,0,i] = shc( ref[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], j], $
                                    cube[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], i], /interp )
                ;shift cube by fractional pixels
                cube[*,*,i] = shift_bicub(cube[*,*,i], sft[0,0,0,i], sft[1,0,0,i])
           ENDFOR

        ENDFOR
;does small shift, fov mask now moving and image too


;;***************************************************************
;; do the remainder, if neccessary
;;***************************************************************

        IF (nim2 GT 0) THEN BEGIN
           FOR i = dimr[3]*nim1, dim[3]-1 DO BEGIN

               IF (dimr[0] LT 2) THEN IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]
               sft[*,0,0,i] = shc( ref[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], dimr[3]-1], $
                                   cube[sfp[0,0]:sfp[0,1], sfp[1,0]:sfp[1,1], i], /interp )
               cube[*,*,i] = shift_bicub(cube[*,*,i], sft[0,0,0,i], sft[1,0,0,i])

           ENDFOR

        ENDIF
    
        flag = 1
        rt = -1

        IF (N_ELEMENTS(ks) eq 1) THEN BEGIN          ;if dstr_idl.kernel only [0] then done&return
            IF arg_present(shifts) THEN shifts = sft
            RETURN, cube 
        ENDIF ELSE ks = ks[1:*] ;if dstr_idl.kernel more elements than [0] then rigid alignment done and continue with rest

     ENDIF ELSE BEGIN ;lk added: no ridig aligment. rt is not defined then
     rt = -1 ;does not work, program crashes in combineshifts.pro
             ;-> must use rigid alignment
    ENDELSE

;;***************************************************************
;; dimension of (remaining) kernel size vector
;;***************************************************************

     run = (SIZE(ks,/dimensions))[0]

;;***************************************************************
; destretch
;;***************************************************************

     FOR j = 0, run-1 DO BEGIN ;for each number of subfields (rigid was omitted above)
if pr then print,j,' of ',run-1
         ; get shifts
   
         kernel = BYTARR(ks[j], ks[j])
         r      = mkcps(ref, kernel) ;compute reference control point coordinates (reg.pro)
                                     ;kernel (i.e. [24,12]) is size of single subframes, size of
                                     ;kernel will determine the number of iterations for shift calc.

         IF (dimr[0] LT 2) THEN BEGIN

            shft = FLTARR((SIZE(r))[1], (SIZE(r))[2], (SIZE(r))[3], dim[3])

            FOR i = 0, dim[3]-1 DO BEGIN
         
                IF (i GT 0) THEN ref = avg(cube[*,*,(i-tr)>0:i], 2) ELSE ref = cube[*,*,i]

                shft[*,*,*,i] = cps(cube[*,*,i], ref, kernel) ;compute offsets from reference points

            ENDFOR

         ENDIF ELSE IF (dimr[3] gt 1) THEN BEGIN

                       shft = fltarr((SIZE(r))[1], (SIZE(r))[2], (SIZE(r))[3], dim[3])
                       ; normal
                       test = shft

                       FOR i = 0, dimr[3]-2 DO BEGIN
                           shft[*,*,*,i*nim1:(i+1)*nim1] = cps(REFORM(cube[*,*,i*nim1:(i+1)*nim1]), $ 
                                                               REFORM(ref[*,*,i]), kernel) 
                                       ;makes grid with stars and rectangles
  ;lk testing, try to repair points which are too far off grid (i.e. off limb data)
                           test[*,*,*,i*nim1:(i+1)*nim1] = repair(r,shft[*,*,*,i*nim1:(i+1)*nim1])
                           shft[*,*,*,i*nim1:(i+1)*nim1]=test[*,*,*,i*nim1:(i+1)*nim1]

                       endfor

                       ; remainder

                       shft[*,*,*,(dimr[3]-1)*nim1:*] = cps(REFORM(cube[*,*,(dimr[3]-1)*nim1:*]), $ 
                                                            REFORM(ref[*,*,dimr[3]-1]), kernel)
                       test[*,*,*,(dimr[3]-1)*nim1:*] =repair(r,shft[*,*,*,(dimr[3]-1)*nim1:*])
                       shft[*,*,*,(dimr[3]-1)*nim1:*]=test[*,*,*,(dimr[3]-1)*nim1:*] 
                   ENDIF ELSE BEGIN

                           shft = cps(cube, ref, kernel) ;to compute the offsets for current scene from reference
                           test = repair(r, shft) ;to repair control point displacements
                           shft = test
                           ENDELSE

        ; detrend shifts

;not executed during IBIS red.
        IF (dp NE 0) THEN shft = detrendshifts(shft, r, dp) ;r=reference point coords.
                                    ;dp = dstr_idl.detrend from template file

        ; register
        FOR i = 0, dim[3]-1 DO cube[*,*,i] = doreg(cube[*,*,i], r, shft[*,*,*,i]) ;actual destretchning.
               ;interpolation for shifted grid
        ; combine shifts
        ;flag always 1 if started with rigid alignment
        IF (flag EQ 1) THEN BEGIN
           if pr then print,'combining shifts',sft[*,*,0]
           if pr then print,shft[*,*,0]
           sft = combineshifts(dim[1], dim[2], rt, r, sft, shft)
           if pr then print,'----'
           if pr then print,sft[*,*,0]
           rt  = r
        ENDIF

        flag = 1


     ENDFOR
if pr then print,'return'
     IF arg_present(shifts) THEN shifts = sft
     IF arg_present(grid)   THEN grid   = r

ENDELSE


;;***************************************************************
;; Done
;;***************************************************************

     RETURN, cube

END
