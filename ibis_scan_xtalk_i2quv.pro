;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;  Procedure: ibis_scan_xtalk_i2quv
;
;  Purpose: Removal of crosstalk I --> Q,U,V
;
;  Input: stksin -- Input Stokes scan, dimensions = (x,y,lambda,4).
;
;          qsrange -- vector of 4 elements [x1,y1,x2,y2] defining Quiet Sun region.
;
;          crange -- spectral region used for averaging of Stokes signal.
;
;
;
;  Output: stksout -- Output Stokes scan, dimensions = (x,y,lambda,4)
;
;          pfac -- 4-element vector with polarization factors 
;                  [polfacv, polfacq, polfacu] 
;           
;  ali@nso.edu February 2007
;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_scan_xtalk_i2quv, stksin, crange, stksout, pfac, $
                           MASK = mask, REGION = region

;-------------------------------------------------
;; Constants, variables, settings
;-------------------------------------------------

     c1 = crange[0]
     c2 = crange[1]
     nc = FLOAT(c2-c1+1)
 
;-------------------------------------------------
;; Form Stokes data arrays and continuum maps
;-------------------------------------------------

     ii = REFORM( stksin[*,*,*,0] )
     qq = REFORM( stksin[*,*,*,1] )
     uu = REFORM( stksin[*,*,*,2] )
     vv = REFORM( stksin[*,*,*,3] )

     if nc ne 1 then begin
     vcont = TOTAL( vv[*,*,c1:c2], 3 ) / nc
     icont = TOTAL( ii[*,*,c1:c2], 3 ) / nc
     qcont = TOTAL( qq[*,*,c1:c2], 3 ) / nc
     ucont = TOTAL( uu[*,*,c1:c2], 3 ) / nc
    endif else begin
     vcont = vv[*,*,c1:c2]
     icont = ii[*,*,c1:c2]
     qcont = qq[*,*,c1:c2]
     ucont = uu[*,*,c1:c2]
    endelse

;-------------------------------------------------
;; Calculate I->QUV cross talk fractions
;-------------------------------------------------

     IF KEYWORD_SET(REGION) THEN BEGIN

        xa = region[0] & xb = region[2]
        ya = region[1] & yb = region[3]

        polfacv = AVG( (vcont/icont)[xa:xb,ya:yb] )
        polfacq = AVG( (qcont/icont)[xa:xb,ya:yb] )
        polfacu = AVG( (ucont/icont)[xa:xb,ya:yb] )

     ENDIF

     IF KEYWORD_SET(MASK) THEN BEGIN

        index  = where(mask eq 1)
        polfacv = AVG( (vcont/icont)[index] )
        polfacq = AVG( (qcont/icont)[index] )
        polfacu = AVG( (ucont/icont)[index] )

     ENDIF

     pfac = [polfacv, polfacq, polfacu]

;-------------------------------------------------
;; Correct for crosstalk I-->Q,U,V 
;-------------------------------------------------

     vmap = vv
     qmap = qq
     umap = uu
     
     s = SIZE(vmap)             


     FOR j = 0,s[3]-1 DO BEGIN
         
         vmap[*,*,j] = vv[*,*,j] - (polfacv * ii[*,*,j])
         qmap[*,*,j] = qq[*,*,j] - (polfacq * ii[*,*,j])
         umap[*,*,j] = uu[*,*,j] - (polfacu * ii[*,*,j])
             
     ENDFOR 

;-------------------------------------------------
;; Form output array
;-------------------------------------------------

     stksout = stksin

     stksout[*,*,*,0] = ii
     stksout[*,*,*,1] = qmap
     stksout[*,*,*,2] = umap
     stksout[*,*,*,3] = vmap

;-------------------------------------------------
;; Done
;-------------------------------------------------

END
