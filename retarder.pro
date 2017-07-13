function retarder,axis_angle,retardance
; angles: in radians

delta=retardance
th=axis_angle

cd=cos(delta)
sd=sin(delta)
c2=cos(2.*th)
c22=c2^2
s2=sin(2.*th)
s22=s2^2
cs2=s2*c2
Matrix=[ [1.       ,0.        ,0.        ,0.       ] ,$
         [0.       ,c22+s22*cd  ,cs2*(1-cd),-s2*sd ] ,$
         [0.       ,cs2*(1-cd),s22+c22*cd,c2*sd    ] ,$
         [0.       ,s2*sd     ,-c2*sd    ,cd       ]  ]

return,Matrix

end
