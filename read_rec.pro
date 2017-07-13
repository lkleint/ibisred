function read_rec, file, xs , ys

     openr,1,file  ;;; ,swap_endian=1
     z=assoc(1, fltarr(xs,ys)) & image=z(0)
     close,1

return,image
end
