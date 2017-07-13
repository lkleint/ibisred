; $Id: cw_bgroup_mod.pro,v 1.25 2000/01/21 00:28:57 scottm Exp $
;
; Copyright (c) 1992-2000, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;+
; NAME:
;   CW_BGROUP_MOD
;
; PURPOSE:
;   CW_BGROUP_MOD is a modification of CW_BGROUP that allows for
;   turning off all of the buttons in the group.
;       
; CATEGORY:
;   Compound widgets
;
; CALLING SEQUENCE:
;       Widget = CW_BGROUP_MOD(Parent, Names)
;
;       Otherwise the same as CW_BGROUP
;
; INPUTS:
;   Parent:     The ID of the parent widget.
;   Names:      A string array, containing one string per button,
;               giving the name of each button.
;
; KEYWORD PARAMETERS:
;   All keywords are the same as CW_BGROUP except:
;
;   SET_VALUE:  The initial value of the buttons. This is equivalent
;           to the later statement:
;
;           WIDGET_CONTROL, widget, set_value=value
; 
;        Pass in a negative value to turn off all of the buttons
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;
;        Same as CW_BGROUP.
;
; RESTRICTIONS:
;        
;        Same as CW_BGROUP.
;
; MODIFICATION HISTORY:
;	Written by:	Edward C. Wiebe, 2002-08-27
;	Modified:	ECW, 2002-01-15 (changed the functionality of
;             CW_BGROUP so that all buttons in in an exclusive group
;             could be off).
;-


pro CW_BGROUP_MOD_SETV, id, value
  compile_opt hidden

  ON_ERROR, 2                       ;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  case state.type of
    0: message,'unable to set plain button group value'
    1: begin
      WIDGET_CONTROL, SET_BUTTON=0, state.ids[state.excl_pos]
;     turn on the newly selected button if the value is ge 0.
      if (value ge 0) then begin
        state.excl_pos = value
        WIDGET_CONTROL, /SET_BUTTON, state.ids[value]
      endif
    end
    2: begin
      n = n_elements(value)-1
      for i = 0, n do begin
        state.nonexcl_curpos[i] = value[i]
        WIDGET_CONTROL, state.ids[i], SET_BUTTON=value[i]
      endfor
    end
  endcase

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
end



function CW_BGROUP_MOD_GETV, id, value

  compile_opt hidden
  ON_ERROR, 2                       ;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  case state.type of
    0: message,'unable to get plain button group value'
    1: ret = state.excl_pos
    2: ret = state.nonexcl_curpos
  endcase

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

  return, ret

end



function CW_BGROUP_MOD_EVENT, ev
  compile_opt hidden
  WIDGET_CONTROL, ev.handler, GET_UVALUE=stash
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY
  WIDGET_CONTROL, ev.id, get_uvalue=uvalue

  ret = 1           ;Assume we return a struct
  case state.type of
    0:
    1: if (ev.select eq 1) then begin
      state.excl_pos = uvalue
    ENDIF else begin
      if (state.no_release ne 0) then ret = 0
    ENDELSE
    2: begin
      ; Keep track of the current state
      state.nonexcl_curpos[uvalue] = ev.select
          if (state.no_release ne 0) and (ev.select eq 0) then ret = 0
    end
  endcase

  if ret then begin     ;Return a struct?
      ret = { ID:state.base, TOP:ev.top, HANDLER:0L, SELECT:ev.select, $
           VALUE:state.ret_arr[uvalue] }
      efun = state.efun
      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
      if efun ne '' then return, CALL_FUNCTION(efun, ret) $
      else return, ret
  endif else begin      ;Trash the event
      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
      return, 0
  endelse
end







function CW_BGROUP_MOD, parent, names, $
    BUTTON_UVALUE = button_uvalue, COLUMN=column, EVENT_FUNCT = efun, $
    EXCLUSIVE=excl, FONT=font, FRAME=frame, IDS=ids, LABEL_TOP=label_top, $
    LABEL_LEFT=label_left, MAP=map, $
    NONEXCLUSIVE=nonexcl, NO_RELEASE=no_release, RETURN_ID=return_id, $
    RETURN_INDEX=return_index, RETURN_NAME=return_name, $
    ROW=row, SCROLL=scroll, SET_VALUE=sval, SPACE=space, UVALUE=uvalue, $
    XOFFSET=xoffset, XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size,$
    YOFFSET=yoffset, YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size, $
    UNAME=uname

  IF (N_PARAMS() ne 2) THEN MESSAGE, 'Incorrect number of arguments'

  ON_ERROR, 2                       ;return to caller

  ; Set default values for the keywords
  version = WIDGET_INFO(/version)
  if (version.toolkit eq 'OLIT') then def_space_pad = 4 else def_space_pad = 3
  IF (N_ELEMENTS(column) eq 0)      then column = 0
  IF (N_ELEMENTS(excl) eq 0)        then excl = 0
  IF (N_ELEMENTS(frame) eq 0)       then frame = 0
  IF (N_ELEMENTS(map) eq 0)     then map=1
  IF (N_ELEMENTS(nonexcl) eq 0)     then nonexcl = 0
  IF (N_ELEMENTS(no_release) eq 0)  then no_release = 0
  IF (N_ELEMENTS(row) eq 0)     then row = 0
  IF (N_ELEMENTS(scroll) eq 0)      then scroll = 0
  IF (N_ELEMENTS(space) eq 0)       then space = def_space_pad
  IF (N_ELEMENTS(uname) eq 0)      then uname = 'CW_BGROUP_MOD_UNAME'
  IF (N_ELEMENTS(uvalue) eq 0)      then uvalue = 0
  IF (N_ELEMENTS(xoffset) eq 0)     then xoffset=0
  IF (N_ELEMENTS(xpad) eq 0)        then xpad = def_space_pad
  IF (N_ELEMENTS(xsize) eq 0)       then xsize = 0
  IF (N_ELEMENTS(x_scroll_size) eq 0)   then x_scroll_size = 0
  IF (N_ELEMENTS(yoffset) eq 0)     then yoffset=0
  IF (N_ELEMENTS(ypad) eq 0)        then ypad = def_space_pad
  IF (N_ELEMENTS(ysize) eq 0)       then ysize = 0
  IF (N_ELEMENTS(y_scroll_size) eq 0)   then y_scroll_size = 0




  top_base = 0L
  if (n_elements(label_top) ne 0) then begin
    next_base = WIDGET_BASE(parent, XOFFSET=xoffset, YOFFSET=yoffset, /COLUMN)
    if(keyword_set(font))then $
       junk = WIDGET_LABEL(next_base, value=label_top,font=font) $
    else    junk = WIDGET_LABEL(next_base, value=label_top)
    top_base = next_base
  endif else next_base = parent

  if (n_elements(label_left) ne 0) then begin
    next_base = WIDGET_BASE(next_base, XOFFSET=xoffset, YOFFSET=yoffset, /ROW)
    if(keyword_set(font))then $
       junk = WIDGET_LABEL(next_base, value=label_left, font=font) $
    else junk = WIDGET_LABEL(next_base, value=label_left)
    if (top_base eq 0L) then top_base = next_base
  endif
  ; We need some kind of outer base to hold the users UVALUE
  if (top_base eq 0L) then begin
    top_base = WIDGET_BASE(parent, XOFFSET=xoffset, YOFFSET=yoffset)
    next_base = top_base
  endif
  If (top_base EQ next_base) THEN $
     next_base = WIDGET_BASE(top_base, Xpad=1, Ypad=1, Space=1)

  ; Set top level base attributes
  WIDGET_CONTROL, top_base, MAP=map, $
    FUNC_GET_VALUE='CW_BGROUP_MOD_GETV', PRO_SET_VALUE='CW_BGROUP_MOD_SETV', $
    SET_UVALUE=uvalue, SET_UNAME=uname

  ; The actual button holding base
  base = WIDGET_BASE(next_base, COLUMN=column, EXCLUSIVE=excl, FRAME=frame, $
    NONEXCLUSIVE=nonexcl, ROW=row, SCROLL=scroll, SPACE=space, $
    XPAD=xpad, XSIZE=xsize, X_SCROLL_SIZE=x_scroll_size, $
    YPAD=ypad, YSIZE=ysize, Y_SCROLL_SIZE=y_scroll_size, $
    EVENT_FUNC='CW_BGROUP_MOD_EVENT', $
    UVALUE=WIDGET_INFO(top_base, /child))


  n = n_elements(names)
  ids = lonarr(n)
  for i = 0, n-1 do begin
    if (n_elements(font) eq 0) then begin
      ids[i] = WIDGET_BUTTON(base, value=names[i], UVALUE=i, $
      UNAME=uname+'_BUTTON'+STRTRIM(i,2))
    endif else begin
      ids[i] = WIDGET_BUTTON(base, value=names[i], FONT=font, $
      UVALUE=i, UNAME=uname+'_BUTTON'+STRTRIM(i,2))
    endelse
  endfor

  ; Keep the state info in the real (inner) base UVALUE.
  ; Pick an event value type:
  ; 0 - Return ID
  ; 1 - Return INDEX
  ; 2 - Return NAME
  ret_type = 1
  if KEYWORD_SET(RETURN_ID) then ret_type = 0
  if KEYWORD_SET(RETURN_NAME) then ret_type = 2
  if KEYWORD_SET(BUTTON_UVALUE) then ret_type = 3
    case ret_type of
      0: ret_arr = ids
      1: ret_arr = indgen(n)
      2: ret_arr = names
      3: ret_arr = button_uvalue
    endcase
  type = 0
  if (excl ne 0) then type = 1

  if (nonexcl ne 0) then type = 2
  if n_elements(efun) le 0 then efun = ''
  state = { type:type, $    ; 0-Standard, 1-Exclusive, 2-Non-exclusive
        base: top_base, $   ; clcw_bgroup base...
        ret_arr:ret_arr, $  ; Vector of event values
        efun : efun, $  ; Name of event fcn
        nonexcl_curpos:intarr(n), $ ; If non-exclus, tracks state
        excl_pos:0, $           ; If exclusive, current button
        ids:ids, $          ; Ids of buttons
        no_release:no_release }
  WIDGET_CONTROL, WIDGET_INFO(top_base, /CHILD), SET_UVALUE=state, /NO_COPY

  if (n_elements(sval) ne 0) then CW_BGROUP_MOD_SETV, top_base, sval

  return, top_base
END
