pro constructTmatrix_new,tt,angles,lam,Tmat,pol_angle=pol_angle, $
                     polM=polM,nonorm=nonorm

; Calling sequence:
; constructTmatrix,tt,angles,lam,Tmat
;
; Inputs:
; tt -> Array with 5+7*nw elements containing the telescope parameters
;       (see below, nw=number of wavelengths). All angles in tt are in deg.
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
;   tt(0)=Number of wavelengths
;   tt(1)=Entrance Window (EW) retarder orientation
;   tt(2)=Exit Window (XW) retarder orientation
;   tt(3)=Polarimeter-Telescope reference frame rotation
;   tt(4)=Entrance window polarizer angle offset
; 
; Let i denote the wavelength index, starting with 0 running up to tt(0)-1
;   tt(5+i*7)=Wavelength (A)
;   tt(6+i*7)=EW retardance
;   tt(7+i*7)=XW retardance
;   tt(8+i*7)=Coelostat rs/rp
;   tt(9+i*7)=Coelostat retardance
;   tt(10+i*7)=Primary mirror rs/rp
;   tt(11+i*7)=Primary mirror retardance

if (n_elements(pol_angle) eq 0) then pol_angle=-1

polM=fltarr(4,4)
for ii=0,3 do polM(ii,ii)=1.

Tmat=fltarr(4,4)

; Normalize angles first

tt(1)=norm_angle(tt(1),limit=180)
tt(2)=norm_angle(tt(2),limit=180)
tt(3)=norm_angle(tt(3))
tt(4)=norm_angle(tt(4))
tt(6)=norm_angle(tt(6))
tt(7)=norm_angle(tt(7))
tt(9)=norm_angle(tt(9))
tt(11)=norm_angle(tt(11))

; Wavelength linear interpolation

if (tt(0) eq 1) then begin ; Wavelength-independent telescope
   ttl=tt
endif else begin ; Multi-wavelength telescope
   ttl=tt(0:11)
   ilam0=0
   while (tt(5+ilam0*7) lt lam and ilam0 lt tt(0)-2) do ilam0=ilam0+1
   ilam1=ilam0+1
   if (lam le tt(5+ilam0*7)) then begin
      ttl(5:11)=tt(5+ilam0*7:11+ilam0*7)
   endif else  if (lam ge tt(5+ilam1*7)) then begin
      ttl(5:11)=tt(5+ilam1*7:11+ilam1*7)
   endif else begin
      x0=tt(5+ilam0*7)
      x1=tt(5+ilam1*7)
      for j=5, 11 do begin
         y0=tt(j+ilam0*7)
         y1=tt(j+ilam1*7)
         ttl(j)=y0+(y1-y0)/(x1-x0)*(lam-x0)
      endfor
   endelse
endelse


; Free parameters

delta_ew=ttl(6)*!pi/180.  ; Retardance of Entrance Window
phi_ew=ttl(1)*!pi/180.    ; Fast axis angle of Entrance Window
delta_xw=ttl(7)*!pi/180.  ; Retardance of Exit Window
phi_xw=ttl(2)*!pi/180.    ; Fast axis angle of Exit Window
offout=ttl(3)*!pi/180.    ; Polarimeter rotation on exit port
rs_over_rp=ttl(8)         ; Reflectances of coelostat mirrors rs/rp
ret=ttl(9)*!pi/180.       ; Retardance of coelostat mirrors
prirs_over_rp=ttl(10)      ; Reflectances of primary mirror rs/rp
priret=ttl(11)*!pi/180.    ; Retardance of primary mirror
offpol=ttl(4)             ; Offset of entrance polarizer

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
