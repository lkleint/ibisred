;+
; NAME:
;  buttonbox_lk
;
; PURPOSE:
;
;  This function allows the user to click a button in a
;  pop-up dialog widget and have it returned to the program.
;  This is an example of a Pop-Up Dialog Widget.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;
; modified June 2011, lk, from textbox_lk
;
; CATEGORY:
;
;  Utility, Widgets
;
; CALLING SEQUENCE:
;
;  thetext = TextBox()
;
; INPUTS:
;
;  None.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;  theText: The string of characters the user typed in the
;       text widget. No error checking is done.
;
;-----------------------------------------------------

PRO buttonbox_lk_CenterTLB, tlb

   ; This utility routine centers the TLB.

Device, Get_Screen_Size=screenSize
IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;-----------------------------------------------------



PRO buttonbox_lk_Event, event
common variables, date,path,path1,wpath,telparpath,lfile,$ ;global variables
                  spath, spath1, cpath, tpath, ppath, speckle_dir,$ ;from paths_vars
                  wldbfile, nbdbfile, tpdbfile, tppdbfile, sdbfile, snbdbfile, $
                  wdfile, wffile, afile, ffile, dfile, bfile, gfile, wlffile, wldfile, $
                  dcdir, ffdir, datadir, griddir, tardir, poldir, upath, rpath, dpath, la,$
                  wl_sid,offset_map,aps,datadir2
common point, ptr

   ; This event handler responds to all events. Widget
   ; is always destoyed. The text is recorded if ACCEPT
   ; button is selected or user hits CR in text widget.

;Widget_Control, event.top, Get_UValue=poldir
widget_control, event.id, get_uvalue=nr2

npolcal = n_elements(poldir)

if (nr2 ge 0 and nr2 le npolcal) then begin
(*ptr).text = poldir[nr2]

Widget_Control, event.top, /Destroy
endif


;clicking on 'done' returns empty string
;it is smarter to keep this button for the case that there are no poldirs
;and the user clicks on the N/A button in ibis_gui. In that case, this modal
;widget would block the command line and there would be no way to stop it and
;return to the ibis_gui
if nr2 eq 1000 then begin
(*ptr).text = ''
  Widget_Control, event.top, /Destroy
endif

    

END ;-----------------------------------------------------



FUNCTION buttonbox_lk,Cancel=cancel, Group_Leader=groupleader

common variables, date,path,path1,wpath,telparpath,lfile,$ ;global variables
                  spath, spath1, cpath, tpath, ppath, speckle_dir,$ ;from paths_vars
                  wldbfile, nbdbfile, tpdbfile, tppdbfile, sdbfile, snbdbfile, $
                  wdfile, wffile, afile, ffile, dfile, bfile, gfile, wlffile, wldfile, $
                  dcdir, ffdir, datadir, griddir, tardir, poldir, upath, rpath, dpath, la,$
                  wl_sid,offset_map,aps,datadir2
common point, ptr

;### these sizes may need to be changed for other screens. modal widgets are non-scrollable
;so I chose some size that should fit laptop screens and still showed my full path
;names.
xsize = 500    ;increase if there is not enough space for path names
textlength=330 ;increase if text on left not fully displayed

   ; Return to caller if there is an error. Set the cancel
   ; flag and destroy the group leader if it was created.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Dialog_Message(!Error_State.Msg)
   IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
   cancel = 1
   RETURN, 'done' ;modified lk
ENDIF

   ; Check parameters and keywords.

title = 'Polarization directories...'



   ; Provide a group leader if not supplied with one. This
   ; is required for modal operation of widgets. Set a flag
   ; for destroying the group leader widget before returning.

IF N_Elements(groupleader) EQ 0 THEN BEGIN
   groupleader = Widget_Base(Map=0)
   Widget_Control, groupleader, /Realize
   destroy_groupleader = 1
ENDIF ELSE destroy_groupleader = 0

   ; Create modal base widget.

npolcal = n_elements(poldir)

tlb = Widget_Base(Title=title, row=npolcal/7+1, /Modal, $
   /Base_Align_Center, Group_Leader=groupleader, Scr_XSize=750)

;tlb = Widget_Base(tla,Column=7)

   ; Create the rest of the widgets.


labelbase = Widget_Base(tlb, /row)
;label = Widget_Label(labelbase, Value=label,scr_xsize=textlength,/align_left)

for i=0,npolcal-1 do textid = widget_button(tlb, value=poldir[i],uvalue=i)
;textID = Widget_Text(labelbase, /Editable, Scr_XSize=xsize, Value=value)


buttonBase = Widget_Base(tlb, Row=1)
;cancelID = Widget_Button(buttonBase, Value='Cancel')
acceptID = Widget_Button(buttonBase, Value='DONE',/frame, uvalue=1000)

   ; Center the widgets on display.

buttonbox_lk_CenterTLB, tlb
Widget_Control, tlb, /Realize

   ; Create a pointer for the text the user will type into the program.
   ; The cancel field is set to 1 to indicate that the user canceled
   ; the operation. Only if a successful conclusion is reached (i.e.,
   ; a Carriage Return or Accept button selection) is the cancel field
   ; set to 0.

ptr = Ptr_New({text:""})

   ; Store the program information:

;info = {ptr:ptr}
;Widget_Control, tlb, Set_UValue=poldir, /No_Copy

   ; Blocking or modal widget, depending upon group leader.

XManager, 'buttonbox_lk', tlb

   ; Return from block. Return the text to the caller of the program,
   ; taking care to clean up pointer and group leader, if needed.
   ; Set the cancel keyword.



tmp = (*ptr).text



;IF destroy_groupleader THEN 
Widget_Control, groupleader, /Destroy

;la is a string in txt file and input file, for example '6302 8542'
;make string array from that for program use
;la = strsplit(la,' ',/extract)

RETURN, tmp
END ;-----------------------------------------------------


