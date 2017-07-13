;modified for modal widget with group leader
;otherwise align_ibis just continues

pro EHANDLER, ev
  widget_control, /DESTROY, ev.TOP
end

pro ANIM_lk, movie, TRACK=track, CYCLE=cycle, SURFACE=surface, PNG_NAME=png_name, Group_Leader=groupleader

  movieSize = size(movie)
  if (movieSize(0) ne 3) then begin
    print, 'Not a three-dimensional array'
    return
  endif
  cycle = keyword_set(CYCLE)
  track = keyword_set(TRACK)

  IF (keyword_set(SURFACE)) THEN BEGIN
    Nx = 400
    Ny = 300
    moviemin = min(movie, MAX=moviemax)
    zrange   = [moviemin, moviemax]
  ENDIF ELSE BEGIN
    Nx = movieSize(1)
    Ny = movieSize(2)
  ENDELSE
  Nt = movieSize(3) 

   ; Return to caller if there is an error. Set the cancel
   ; flag and destroy the group leader if it was created.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
   cancel = 1
   RETURN;, 'done' ;modified lk
ENDIF

   ; Provide a group leader if not supplied with one. This
   ; is required for modal operation of widgets. Set a flag
   ; for destroying the group leader widget before returning.

IF N_Elements(groupleader) EQ 0 THEN BEGIN
   groupleader = Widget_Base(Map=0)
   Widget_Control, groupleader, /Realize
   destroy_groupleader = 1
ENDIF ELSE destroy_groupleader = 0

  baseWidget    = widget_base(TITLE='Animate',/modal, Group_Leader=groupleader)
  animateWidget = cw_animate(baseWidget, Nx, Ny, Nt, TRACK=track, CYCLE=cycle)
  widget_control, /REALIZE, baseWidget

  IF (keyword_set(SURFACE)) THEN BEGIN
    window, /FREE, XSIZE=Nx, YSIZE=Ny, /PIXMAP
    pixMap = !D.WINDOW
    scale3, AX=30, AZ=30, ZRANGE=zrange
    t3d, TRANSLATE=[-.1,-.1,-.1], SCALE=replicate(sqrt(1.5), 3)

    FOR nIt=0, Nt-1 DO BEGIN
      shade_surf, /T3D, movie(*, *, nIt), ZRANGE=zrange
      cw_animate_load, animateWidget, FRAME=nIt, WINDOW=pixMap
    ENDFOR
    wdelete, pixMap
  ENDIF ELSE BEGIN
    FOR i=0,Nt-1 DO BEGIN
      imgmax = max(movie(*, *, i), MIN=imgmin)
;lk changed -1 otherwise the overexposed parts are black, not white
      image = (!D.TABLE_SIZE-1) * (movie(*, *, i) - imgmin) / $
       float(imgmax - imgmin)
      cw_animate_load, animateWidget, FRAME=i, IMAGE=image

      IF (keyword_set(PNG_NAME)) THEN BEGIN
        filename = png_name + '_' + $
                   string(i, FORMAT=((i LT 10) ? '(I1)' : '(I2)')) + '.png'
        write_png, filename, tvrd(), r_curr, g_curr, b_curr
      ENDIF
    ENDFOR
  ENDELSE

  cw_animate_run, animateWidget
  xmanager, "Xanim", baseWidget, EVENT_HANDLER="EHANDLER"

IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
return; ,'done'
end
