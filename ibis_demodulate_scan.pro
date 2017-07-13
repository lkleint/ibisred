;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
;  Procedure: ibis_demodulate_scan
;
;  Purpose: Demodulates the input array with respect
;           to a given modulation scheme
;
;  Input:    indata -- 5-dimensional data scan, dimension (x,y,6,lambda,beam)
;
;         modscheme -- used modulation scheme, stringarr
;
;  Output:  outdata -- 5-dimensional data scan, dimension (x,y,lambda,4,beam)
;
;  ali@nso.edu February 2007
;LK: not used anymore for response matrices
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_demodulate_scan, indata, modscheme, outdata

;--------------------------------
;; Get Modulation Scheme
;-------------------------------

     ipq = WHERE( modscheme EQ 'I+Q' )

     if ipq[0] eq -1 then begin
        print,'ipq index not correct! have to improvise ...'
        ipq[0] = 0
     endif

     imq = WHERE( modscheme EQ 'I-Q' )
     if imq[0] eq -1 then begin
        print,'imq index not correct! have to improvise ...'
        imq[0] = 2
     endif

     ipv = WHERE( modscheme EQ 'I+V' )
     if ipv[0] eq -1 then begin
        print,'ipv index not correct! have to improvise ...'
        ipv[0] = 1
     endif

     imv = WHERE( modscheme EQ 'I-V' )
     if imv[0] eq -1 then begin
        print,'imv index not correct! have to improvise ...'
        imv[0] = 3
     endif

     ipu = WHERE( modscheme EQ 'I+U' )
     if ipu[0] eq -1 then begin
        print,'ipu index not correct! have to improvise ...'
        ipu[0] = 5
     endif

     imu = WHERE( modscheme EQ 'I-U' )
     if imu[0] eq -1 then begin
        print,'imu index not correct! have to improvise ...'
        imu[0] = 4
     endif

     LEFT = 0 & RIGHT = 1

;-------------------------------------------------
; Create dual beam Stokes from temporal modulation 
;-------------------------------------------------

     s = SIZE(indata)
     outdata = FLTARR(s[1], s[2], s[4], 4, 2)

;----- intensities ----------
;iql = (I+Q)_L + (I-Q)_L
     iql = REFORM( 0.5 * ( indata[*,*,ipq[0],*,LEFT] + indata[*,*,imq[0],*,LEFT] ) )
     ivl = REFORM( 0.5 * ( indata[*,*,ipv[0],*,LEFT] + indata[*,*,imv[0],*,LEFT] ) )
     iul = REFORM( 0.5 * ( indata[*,*,ipu[0],*,LEFT] + indata[*,*,imu[0],*,LEFT] ) )

     iqr = REFORM( 0.5 * ( indata[*,*,ipq[0],*,RIGHT] + indata[*,*,imq[0],*,RIGHT] ) )
     ivr = REFORM( 0.5 * ( indata[*,*,ipv[0],*,RIGHT] + indata[*,*,imv[0],*,RIGHT] ) )
     iur = REFORM( 0.5 * ( indata[*,*,ipu[0],*,RIGHT] + indata[*,*,imu[0],*,RIGHT] ) )

 ;lk: this it not ideal: it reduces the spatial resolution 
    iil = (iql+ivl+iul) / 3.0d
    iir = (iqr+ivr+iur) / 3.0d
  

  
;---------- stokes -------------
     outdata[*,*,*,0,LEFT] = iil
;q = (I+Q - (I-Q)) /  (I+Q + I-Q) * (I_norm)
     outdata[*,*,*,1,LEFT] = REFORM( 0.5 * ( indata[*,*,ipq[0],*,LEFT] - indata[*,*,imq[0],*,LEFT] ) ) / iql * iil
     outdata[*,*,*,2,LEFT] = REFORM( 0.5 * ( indata[*,*,ipu[0],*,LEFT] - indata[*,*,imu[0],*,LEFT] ) ) / iul * iil
     outdata[*,*,*,3,LEFT] = REFORM( 0.5 * ( indata[*,*,ipv[0],*,LEFT] - indata[*,*,imv[0],*,LEFT] ) ) / ivl * iil

     outdata[*,*,*,0,RIGHT] = iir
     outdata[*,*,*,1,RIGHT] = REFORM( 0.5 * ( indata[*,*,imq[0],*,RIGHT] - indata[*,*,ipq[0],*,RIGHT] ) ) / iqr * iir
     outdata[*,*,*,2,RIGHT] = REFORM( 0.5 * ( indata[*,*,imu[0],*,RIGHT] - indata[*,*,ipu[0],*,RIGHT] ) ) / iur * iir
     outdata[*,*,*,3,RIGHT] = REFORM( 0.5 * ( indata[*,*,imv[0],*,RIGHT] - indata[*,*,ipv[0],*,RIGHT] ) ) / ivr * iir

;;;;; old version

;     outdata[*,*,*,0,LEFT] = REFORM( 0.5 * ( indata[*,*,ipq[0],*,LEFT] + indata[*,*,imq,*,LEFT] ) )
;     outdata[*,*,*,1,LEFT] = REFORM( 0.5 * ( indata[*,*,ipq[0],*,LEFT] - indata[*,*,imq,*,LEFT] ) )
;     outdata[*,*,*,2,LEFT] = REFORM( 0.5 * ( indata[*,*,ipu[0],*,LEFT] - indata[*,*,imu,*,LEFT] ) )
;     outdata[*,*,*,3,LEFT] = REFORM( 0.5 * ( indata[*,*,ipv[0],*,LEFT] - indata[*,*,imv,*,LEFT] ) )

;     outdata[*,*,*,0,RIGHT] = REFORM( 0.5 * ( indata[*,*,ipq[0],*,RIGHT] + indata[*,*,imq[0],*,RIGHT] ) )
;     outdata[*,*,*,1,RIGHT] = REFORM( 0.5 * ( indata[*,*,imq[0],*,RIGHT] - indata[*,*,ipq[0],*,RIGHT] ) )
;     outdata[*,*,*,2,RIGHT] = REFORM( 0.5 * ( indata[*,*,imu[0],*,RIGHT] - indata[*,*,ipu[0],*,RIGHT] ) )
;     outdata[*,*,*,3,RIGHT] = REFORM( 0.5 * ( indata[*,*,imv[0],*,RIGHT] - indata[*,*,ipv[0],*,RIGHT] ) )

;-------------------------------------------------
;; Done
;-------------------------------------------------

END
