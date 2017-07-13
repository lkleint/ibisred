FUNCTION max_pos, f

  ;  @compile_opt.pro        ; On error, return to caller

dim = size(f,/dim)

fx = max(f,ip)
ip = ArrayLocation(ip, dim=dim)

ix = ip[0]
iy = ip[1]

pp = float(ip)

IF ip[0] NE 0 AND ip[0] NE dim[0]-1 THEN    $
    pp[0] = pp[0]+.5*(f[ix+1,iy]-f[ix-1,iy])/(2*fx-f[ix-1,iy]-f[ix+1,iy])

IF ip[1] NE 0 AND ip[1] NE dim[1]-1 THEN    $
    pp[1] = pp[1]+.5*(f[ix,iy+1]-f[ix,iy-1])/(2*fx-f[ix,iy-1]-f[ix,iy+1])

RETURN, pp  &  END
