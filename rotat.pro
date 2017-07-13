function rotat,phi
; angle: in radians

phi2=2*phi
Matrix=fltarr(4,4)
Matrix(0,0)=1.
Matrix(3,3)=1.
Matrix(1,1)=cos(phi2)
Matrix(2,2)=cos(phi2)
Matrix(1,2)=-sin(phi2)
Matrix(2,1)=sin(phi2)

return,Matrix

end
