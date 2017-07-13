function mirror,rs_over_rp,ret
; incidence_angle: in radians

rsp=rs_over_rp

rt=sqrt (rs_over_rp)
sd=sin (ret)
cd=cos (ret)

Matrix=[ [(1.+rsp)/2., (1.-rsp)/2., 0.      , 0.        ]  , $
         [(1.-rsp)/2., (1.+rsp)/2., 0.      , 0.        ]  , $
         [0.        , 0.        ,rt*cd   ,rt*sd       ]  , $
         [0.        , 0.        ,-rt*sd  ,rt*cd       ]  ]

return,Matrix

end
