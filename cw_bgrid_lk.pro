;+
; NAME:
;      CW_BGRID
;
; PURPOSE:
;      CW_BGRID is a compound widget that implements a multi-column
;      cw_bgroup widget. 
;      lk: modified for multi-row, not column!
;
; CATEGORY:
;	Compound widgets
;
; CALLING SEQUENCE:
;	widget = CW_BGRID(parent,names)
;
;   To get or set the value of a CW_BGRID, use the GET_VALUE and
;   SET_VALUE keywords to WIDGET_CONTROL. The value of a CW_BGRID
;   is:
;
;       -----------------------------------------------
;       Type        Value
;       -----------------------------------------------
;       normal      None
;       exclusive       label of currently set button
;       non-exclusive   Vector indicating the position
;                       of each button (1-set, 0-unset)
;       -----------------------------------------------
;
; INPUTS:
;       parent - The ID of the parent widget.
;       names  - A StrArr of button names (a vector).   
;
; KEYWORD PARAMETERS:
;       EVENT_FUNC - The name of an optional user-supplied event function
;           for buttons. This function is called with the return
;           value structure whenever a button is pressed, and
;           follows the conventions for user-written event
;           functions.
;
;	UVALUE - Supplies the user value for the widget.
;       UNAME - Supplies the user name for the widget.
;       ROWS - The number of rows in the grid (the default is eight).
;       GREY_BUTTONS - Use this keyword to turn off certain buttons.
;                      Pass in an array of 0s and 1s with the same
;                      dimensions as the names input array.  Buttons
;                      will be made insensitive where this array holds
;                      a 0.
;       SET_VALUE - Set the initial value of each button in the grid. 
;                   This keyword is an array of 0s and 1s with the
;                   same dimensions as the names input array.  Buttons
;                   will be set on where this array holds a 1.  This
;                   keyword only applies when the NONEXCLUSIVE keyword
;                   is also set.
;       SORT - Sort the button names into alphabetical order before
;              creating the widget.  This option uses IDL's sort
;              function.          
;       EXCLUSIVE - The buttons will take the form of radio buttons
;                   where only one button may be selected at a time.
;       NONEXCLUSIVE - The buttons take the form of the nonexclusive
;                      cw_bgroup buttons.  A checkmark appears when
;                      the button is selected and more than one button
;                      may be selected at a time.   
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; USES: 
;       CW_BGROUP_MOD (for the ability to set a cw_bgroup to all buttons off)
;
;
; MODIFICATION HISTORY:
;     Written by:   Edward C. Wiebe, 2002-08-27
;     modified oct 2011, lk, from columns to rows, no_copy removed
;-


pro CW_BGrid_SetV, id, value
  compile_opt hidden
  On_Error, 2

  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state;, /NO_COPY
  if (state.exclusive eq 0) then  $
    Message,'Unable to set the value for non-exclusive bgroups.'

  if (state.exclusive eq 1) then begin
    s = Size(state.names)
    rows    = s[2]
    columns = s[1]

    indx = (Where(value eq state.names,cnt))[0]
    if (cnt gt 0 ) then begin
      x = indx/columns ;x and y swapped
      y = indx mod columns    
      Widget_Control,state.ids[x],SET_VALUE=y
      if (state.ids[x] ne state.last_id) then begin
        Widget_Control,state.last_id,SET_VALUE=-1    
      endif 
    endif
  endif

  if (state.exclusive eq 2) then begin
    columns = (Size(state.ids))[1]
    t = (Size(value))[1]
    rows = state.rows
    for n=0,columns-1 do begin
      start = n*rows
      stop = (n+1)*rows-1
      if (stop gt t-1) then stop = t-1
      Widget_Control,state.ids[n],SET_VALUE=value[start:stop]
    endfor     
  endif

  Widget_Control, stash, SET_UVALUE=state;, /NO_COPY
  Return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function CW_BGrid_GetV, id
 compile_opt hidden  
  On_Error, 2

  stash = Widget_Info(id, /CHILD)
  Widget_Control, stash, GET_UVALUE=state;
  if (state.exclusive eq 1) then  $
     Message,'Unable to get the value for non-exclusive bgroups.'

;  value = state.names
  ;new lk:
   rws = (size(state.names))[2]  ;number of columns (=6)
   tmp = where(state.names ne '',nele) ;nele=number of directories
   value = intarr(nele)

  for i=0,n_elements(state.ids) -1 do begin ;number of rows 
    Widget_Control,state.ids[i],GET_VALUE=index
;    value[i,*] = index
     value[i*rws:i*rws+n_elements(index)-1] = index
 endfor

;  Widget_Control,state.last_id,GET_VALUE=index
;  value=state.names[(Where(state.ids eq state.last_id))[0],index] 

  Widget_Control, stash, SET_UVALUE=state;

  Return,value
end

;-----------------------------------------------------------------------------

function CW_BGrid_Event, ev
  compile_opt hidden

  parent=ev.handler
; Retrieve the structure from the child that contains the sub ids.
  stash = Widget_Info(parent, /CHILD)
  Widget_Control, stash, GET_UVALUE=state;, /NO_COPY
  if (state.exclusive) then begin
    wid = state.last_id
    if (wid ge 0) and (wid ne ev.ID) then begin
;     state contains the id of the last CLCW_BGroup that changed.
;     deselect the button by calling the appropriate CLCW_BGroup with a -1   
      Widget_Control,wid,SET_VALUE=-1
    endif 

  endif
; save the currently active CLCW_Bgroup
  state.last_id = ev.id

; get the event_function
  event_func = state.event_func

; Restore the state structure
  Widget_Control, stash, SET_UVALUE=state;, /NO_COPY

  result = { ID:parent, TOP:ev.top, HANDLER:0L, VALUE:ev.value, SELECT:ev.select }  

  if (event_func ne '') then begin
    Return, Call_Function(event_func, result) 
  endif else Return, result

end

;-----------------------------------------------------------------------------

Function CW_BGrid_lk, parent, names   $
                   , EVENT_FUNC    = event_func   $
                   , UVALUE        = uval         $
                   , UNAME         = uname        $
                   , ROWS          = rows         $
                   , GREY_BUTTONS  = grey_buttons $
                   , SET_VALUE     = set_value    $
                   , SORT          = sort         $
                   , EXCLUSIVE     =  exclusive   $
                   , NONEXCLUSIVE  = nonexclusive $
                   , _EXTRA        = _EXTRA

  if (N_Params() eq 0) then Message, 'Must specify a parent for CW_BGrid'

  On_Error, 2                   ;return to caller

  if (not Keyword_Set(uval))  then uval = 0
  IF (not Keyword_Set(uname))  then uname = 'CW_BGRID_UNAME'

   ;mod lk: if there are only 3 timestamps but 6 (predefined) rows,
   ;the return value will have a wrong dimension. Fixed:
  if rows gt n_elements(set_value) then rows = n_elements(set_value)

  base = Widget_Base(parent, UVALUE = uval, UNAME = uname   $
                     , EVENT_FUNC = "cw_bgrid_event"        $
                     , FUNC_GET_VALUE = "cw_bgrid_getv"     $
                     , PRO_SET_VALUE = "cw_bgrid_setv"      $
                     , /column)

  if (Keyword_Set(sort)) then begin
    names = names[Sort(names)]
  endif

  if (N_Elements(names) eq 0) then begin
    names = [ 'a','b','c','d','e','f'  $
              ,'g','h','i','j','k','l' $
              ,'m','n','o','p','q','r' $
              ,'s','t','u','v','w','x' $
              ,'y','z']+'   '
  endif

  s = (Size(names))[1]

  if (not Keyword_Set(rows)) then rows = 8
  ncol = Ceil(Float(s)/Float(rows))

; There will be ncol CW_BGroup_Mods.  The last one may have fewer than
; 'rows' elements.
  
  bg_wids = LonArr(ncol)
  nms = StrArr(ncol,rows)

  for n=0,ncol-1 do begin 
    start = n*rows
    stop  = (n+1)*rows-1
    if (stop gt s-1) then stop = s-1


    if (Keyword_Set(set_value)) then begin
      value = set_value[start:stop]
    endif    

    id = CW_BGroup_Mod(base,names[start:stop]        $
                       , /RETURN_NAME                $
                       , /ROW                        $
                       , /FRAME                      $
                       , IDS = ids                   $
                       , SET_VALUE = value           $
                       , EXCLUSIVE    = exclusive       $
                       , NONEXCLUSIVE = nonexclusive $
                       , _EXTRA = _extra)

    if (Keyword_Set(grey_buttons)) then begin
      for m=start,stop do begin
        Widget_Control,ids[m-(rows*n)],SENSITIVE=grey_buttons[m]
      endfor
    endif
    
    bg_wids[n] = id
    nms[n,0:stop-start]     = names[start:stop]
;    Widget_Control,id,SET_UVALUE=parent
  endfor
  
  last_id = -1L
  excl = 0
  if (Keyword_Set(   exclusive)) then excl = 1 
  if (Keyword_Set(nonexclusive)) then excl = 2

  
  if (N_Elements(event_func) le 0) then event_func = ''

  state = { last_id:last_id, exclusive:excl, ids:bg_wids, names:nms $
            , rows:rows, event_func:event_func }
  Widget_Control, Widget_Info(base, /CHILD), SET_UVALUE=state;, /NO_COPY
  
  Return, base
end

