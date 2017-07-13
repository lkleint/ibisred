pro constructTmatrix,tt,angles,lam,Tmat,pol_angle=pol_angle, $
                     polM=polM,nonorm=nonorm

; Calling sequence:
; constructTmatrix,tt,angles,lam,Tmat
;
; Inputs:
; tt -> 22-element array with telescope parameters (see below). Note: Al
;       angles in tt are in deg.
; angles -> 3-element vector containing the VTT angles (AZ,EL,TB) (degrees)
; lam -> Wavelength in Angstroms for which the T-matrix is desired
;
; Outputs:
; Tmat -> 4x4 Mueller matrix of the VTT
;
; NOTE: All matrices returned by this procedure are in the form A(column,row)
;  This means that the usual matrix multiplication operation should be done
;  with the ## operator. For instance, if we want to apply Tmat to an
;  unpolarized vector, we should do:   Out = Tmat##[1,0,0,0]
;
; Optional keywords:
; pol_angle -> If this keyword is present, a perfect linear polarizer is
;              placed on top of the telescope. The value of pol_angle should
;              be the angle in degrees of the polarizer axis. NOTE: The
;              WPOL angle in the ASP file headers has the wrong sign!!
; polM -> If pol_angle is present, polM returns a 4x4 Mueller matrix of the
;         entrance window polarizer. Otherwise, the identity matrix is
;         returned
; nonorm -> Do not normalize Tmat to its first element
;
; Telescope parameters (tt):
;
; Let dl = (lam-6000)/1000 (i.e., wavelength in units of 1000A from 6000A)
;
; Entrance window retardance: tt(0) + tt(10)*dl + tt(16)*dl^2 + tt(22)*dl^3
; Entrance window fast axis orientation: tt(1)
; Exit window retardance: tt(2) + tt(11)*dl + tt(17)*dl^2 + tt(23)*dl^3
; Exit window fast axis orientation: tt(3)
; Telescope-Polarimeter frame rotation: tt(4)
; EL, AZ mirrors rs/rp: tt(5) + tt(12)*dl + tt(18)*dl^2 + tt(24)*dl^3
; EL, AZ mirrors retardance: tt(6) + tt(13)*dl + tt(19)*dl^2 + tt(25)*dl^3
; Primary mirror rs/rp: tt(7) + tt(14)*dl + tt(20)*dl^2 + tt(26)*dl^3
; Primary mirror retardance: tt(8) + tt(15)*dl + tt(21)*dl^2 + tt(27)*dl^3
;

if (n_elements(pol_angle) eq 0) then pol_angle=-1

dlambda=(lam-6000.)/1000. ; Wavelength from 6000. in 1000-Angstrom units

polM=fltarr(4,4)
for ii=0,3 do polM(ii,ii)=1.

Tmat=fltarr(4,4)

; Normalize angles first

tt(0)=norm_angle(tt(0))
tt(1)=norm_angle(tt(1),limit=180)
tt(2)=norm_angle(tt(2))
tt(3)=norm_angle(tt(3),limit=180)
tt(4)=norm_angle(tt(4),limit=180)
tt(6)=norm_angle(tt(6))
tt(8)=norm_angle(tt(8))
tt(9)=norm_angle(tt(9),limit=180)

; Free parameters

delta_ew=tt(0)*!pi/180.  ; Retardance of Entrance Window
phi_ew=tt(1)*!pi/180.    ; Fast axis angle of Entrance Window
delta_xw=tt(2)*!pi/180.  ; Retardance of Exit Window
phi_xw=tt(3)*!pi/180.    ; Fast axis angle of Exit Window
offout=tt(4)*!pi/180.    ; Polarimeter rotation on exit port
rs_over_rp=tt(5)         ; Reflectances of coelostat mirrors rs/rp
ret=tt(6)*!pi/180.       ; Retardance of coelostat mirrors
prirs_over_rp=tt(7)      ; Reflectances of primary mirror rs/rp
priret=tt(8)*!pi/180.    ; Retardance of primary mirror
offpol=tt(9)             ; Offset of entrance polarizer

; Linear wavelength variation (1st order)

delta_ew_1=tt(10)*!pi/180.  ; Entrance Window retardance wavelength gradient
delta_xw_1=tt(11)*!pi/180.  ; Exit Window retardance wavelength gradient
rs_over_rp_1=tt(12)         ; El/Az mirrors reflectance ratio gradient
ret_1=tt(13)*!pi/180.       ; El/Az mirrors retardance gradient
pri_rsrp_1=tt(14)           ; Primary mirror refl ratio gradient
pri_ret_1=tt(15)*!pi/180.   ; Primary retardance gradient

; Parabolic wavelength variation (2nd order)

delta_ew_2=tt(16)*!pi/180. ; Parabolic variation of entrance window ret
delta_xw_2=tt(17)*!pi/180. ; Parabolic variation of exit window ret
rs_over_rp_2=tt(18)   ; Parabolic wavelength variation of El/Az mirrors rs/rp
ret_2=tt(19)*!pi/180. ; Parabolic wavelength variation of El/Az mirrors ret
pri_rsrp_2=tt(20)   ; Parabolic wavelength variation of Main mirror rs/rp
pri_ret_2=tt(21)*!pi/180. ; Parabolic wavelength variation of Main mirrors ret

; Cubic wavelength variation (3rd order)

delta_ew_3=tt(22)*!pi/180. ; Parabolic variation of entrance window ret
delta_xw_3=tt(23)*!pi/180. ; Parabolic variation of exit window ret
rs_over_rp_3=tt(24)   ; Parabolic wavelength variation of El/Az mirrors rs/rp
ret_3=tt(25)*!pi/180. ; Parabolic wavelength variation of El/Az mirrors ret
pri_rsrp_3=tt(26)   ; Parabolic wavelength variation of Main mirror rs/rp
pri_ret_3=tt(27)*!pi/180. ; Parabolic wavelength variation of Main mirrors ret

; Add 1st and 2nd order terms

delta_ew=delta_ew + delta_ew_1*dlambda + delta_ew_2*(dlambda^2)
delta_xw=delta_xw + delta_xw_1*dlambda + delta_xw_2*(dlambda^2)
rs_over_rp=rs_over_rp + rs_over_rp_1*dlambda + rs_over_rp_2*(dlambda^2)
ret=ret + ret_1*dlambda + ret_2*(dlambda^2)
prirs_over_rp=prirs_over_rp + pri_rsrp_1*dlambda + pri_rsrp_2*(dlambda^2)
priret=priret + pri_ret_1*dlambda + pri_ret_2*(dlambda^2)

; Add 3rd order terms

delta_ew=delta_ew + delta_ew_3*(dlambda^3)
delta_xw=delta_xw + delta_xw_3*(dlambda^3)
rs_over_rp=rs_over_rp + rs_over_rp_3*(dlambda^3)
ret=ret + ret_3*(dlambda^3)
prirs_over_rp=prirs_over_rp + pri_rsrp_3*(dlambda^3)
priret=priret + pri_ret_3*(dlambda^3)

; Telescope angles

AZ=angles(0)
EL=angles(1)
TB=angles(2)

phi_ae=(EL+90)*!pi/180.
phi_main_az=(TB-AZ-30)*!pi/180.
phi_pol_main=offout ; Free_parameter
;eta_el=45.*!pi/180. ; Coelostat
;eta_az=45.*!pi/180. ;  "
;eta_main=(1.26/2.)*!pi/180. ; Incidence on primary mirror

; Telescope sub-matrices

D_we=Retarder(phi_ew, delta_ew)
M_el=Mirror(rs_over_rp,ret)
R_az_el=Rotat(phi_ae)
M_az=Mirror(rs_over_rp,ret)
R_main_az=Rotat(phi_main_az)
M_main=Mirror(prirs_over_rp,priret)
D_xw=Retarder(phi_xw, delta_xw)
R_pol_main=Rotat(phi_pol_main)

; Telescope Mueller matrix

Tmat=M_el##D_we
Tmat=R_az_el##Tmat
Tmat=M_az##Tmat
Tmat=R_main_az##Tmat
Tmat=M_main##Tmat
Tmat=D_xw##Tmat
Tmat=R_pol_main##Tmat

if (pol_angle ne -1) then begin
    th=(pol_angle+offpol)*!pi/180.
    c2=cos(2.*th) & s2=sin(2.*th)
    polM=0.5*[ [1.  ,c2  ,s2  ,0.  ] , $
               [c2  ,c2^2,c2*s2,0. ] , $
               [s2  ,c2*s2,s2^2,0. ] , $
               [0.  ,  0., 0., 0.  ]  ]
    Tmat=Tmat##polM
endif

if (not keyword_set(nonorm)) then Tmat=Tmat/Tmat(0,0)

end
