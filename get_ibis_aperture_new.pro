FUNCTION get_ibis_aperture_new, frame, DRADIUS=dradius, PLOT=plot

;; Determine the IBIS aperture in frame by determining the extreme
;; locations where the maxima in the derivatives of the frame in x and y
;; appear. When /PLOT is set verify result by plotting results.

  aperture = create_struct('x0', 0.0, 'y0', 0.0, 'radius', 0.)

  IF (NOT keyword_set(DRADIUS)) THEN dradius = 0.0

  dim = size(frame)
  Nx  = dim[1]
  Ny  = dim[2]

  mask = frame ne frame[5,5]
  tmp = roberts(mask)
  tmp = tmp ne 0.
  index = where_n(tmp eq 1)
  indx = index[*,0]
  indy = index[*,1]
  res = elli_fit(indx, indy)

  ;; Store results in aperture structure.

  aperture.x0 = res[0]
  aperture.y0 = res[2]
  aperture.radius = res[4] + dradius

  ;; graph results if requested.

  IF (keyword_set(PLOT)) THEN BEGIN
    tvscl, frame
    oplot_ibis_aperture, aperture, COLOR=255B
  ENDIF

  return, aperture
END
