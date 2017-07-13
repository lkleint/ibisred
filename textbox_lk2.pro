;+
; NAME:
;  TEXTBOX_lk
;
; PURPOSE:
;
;  This function allows the user to type some text in a
;  pop-up dialog widget and have it returned to the program.
;  This is an example of a Pop-Up Dialog Widget.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; modified May 2011, lk, to have 5 input fields instead of 1 for IBIS data
; mod Jan 3, 2014 for new datared
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
;  CANCEL: An output parameter. If the user kills the widget or clicks the Cancel
;       button this keyword is set to 1. It is set to 0 otherwise. It
;       allows you to determine if the user canceled the dialog without
;       having to check the validity of the answer.
;
;       theText = TextBox(Title='Provide Phone Number...', Label='Number:', Cancel=cancelled)
;       IF cancelled THEN Return
;
;  GROUP_LEADER: The widget ID of the group leader of this pop-up
;       dialog. This should be provided if you are calling
;       the program from within a widget program:
;
;          thetext = TextBox(Group_Leader=event.top)
;
;       If a group leader is not provided, an unmapped top-level base widget
;       will be created as a group leader.
;
;
; OUTPUTS:
;
;  theText: The string of characters the user typed in the
;       text widget. No error checking is done.
;
; RESTRICTIONS:
;
;  The widget is destroyed if the user clicks on either button or
;  if they hit a carriage return (CR) in the text widget. The
;  text is recorded if the user hits the ACCEPT button or hits
;  a CR in the text widget.
;
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, December 20, 2001.
;  Added VALUE keyword to set the initial value of the text box. 4 Nov 2002. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO TextBox_lk2_CenterTLB, tlb

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



PRO TextBox_lk2_Event, event,info=info

   ; This event handler responds to all events. Widget
   ; is always destoyed. The text is recorded if ACCEPT
   ; button is selected or user hits CR in text widget.

Widget_Control, event.top, Get_UValue=info2

      ; Get the text and store it in the pointer location.
   
      Widget_Control, info2.textID, Get_Value=date
      (*info2.ptr).text = date[0]
     
      Widget_Control, info2.textID2, Get_Value=path
      (*info2.ptr).text2 = path[0]
    
      Widget_Control, info2.textID3, Get_Value=path1
      (*info2.ptr).text3 = path1[0]
    
      Widget_Control, info2.textID4, Get_Value=wpath
      (*info2.ptr).text4 = wpath[0]
     
      Widget_Control, info2.textID8, Get_Value=polpath
      (*info2.ptr).text8 = polpath[0]
    
      Widget_Control, info2.textID7, Get_Value=la
      (*info2.ptr).text7 = la[0]
     Widget_Control, event.top, /Destroy


END ;-----------------------------------------------------



FUNCTION TextBox_lk2,Cancel=cancel, Group_Leader=groupleader, info=info

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

title = 'Paths...'


label = "date [yyyymmdd]:"
label2 = "path to original data:"
label3 = "path to save data:"
label4 = "path to original wl data:"
label8 = "path to polcal (if taken on different date):"
label7 = "wavelength(s) to reduce (no commas):"

value = *info.date
value2 = *info.path
value3 = *info.path1
value4 = *info.wpath
value8 = *info.polpath
value7 = *info.la


   ; Provide a group leader if not supplied with one. This
   ; is required for modal operation of widgets. Set a flag
   ; for destroying the group leader widget before returning.

IF N_Elements(groupleader) EQ 0 THEN BEGIN
   groupleader = Widget_Base(Map=0)
   Widget_Control, groupleader, /Realize
   destroy_groupleader = 1
ENDIF ELSE destroy_groupleader = 0

   ; Create modal base widget.

tlb = Widget_Base(Title=title, Column=1, /Modal, $
   /Base_Align_Center, Group_Leader=groupleader)

   ; Create the rest of the widgets.


labelbase = Widget_Base(tlb, /row)
label = Widget_Label(labelbase, Value=label,scr_xsize=textlength,/align_left)
textID = Widget_Text(labelbase, /Editable, Scr_XSize=xsize, Value=value)

labelbase2 = Widget_Base(tlb, /row)
label = Widget_Label(labelbase2, Value=label2,scr_xsize=textlength,/align_left)
textID2 = Widget_Text(labelbase2, /Editable, Scr_XSize=xsize, Value=value2)

labelbase3 = Widget_Base(tlb, /row)
label = Widget_Label(labelbase3, Value=label3,scr_xsize=textlength,/align_left)
textID3 = Widget_Text(labelbase3, /Editable, Scr_XSize=xsize, Value=value3)

labelbase4 = Widget_Base(tlb, /row)
label = Widget_Label(labelbase4, Value=label4,scr_xsize=textlength,/align_left)
textID4 = Widget_Text(labelbase4, /Editable, Scr_XSize=xsize, Value=value4)

labelbase8 = Widget_Base(tlb, /row)
label = Widget_Label(labelbase8, Value=label8,scr_xsize=textlength,/align_left)
textID8 = Widget_Text(labelbase8, /Editable, Scr_XSize=xsize, Value=value8)

labelbase7 = Widget_Base(tlb, /row)
label = Widget_Label(labelbase7, Value=label7,scr_xsize=textlength,/align_left)
textID7 = Widget_Text(labelbase7, /Editable, Scr_XSize=xsize, Value=value7)

buttonBase = Widget_Base(tlb, Row=1)
;cancelID = Widget_Button(buttonBase, Value='Cancel')
acceptID = Widget_Button(buttonBase, Value='DONE',/frame)

   ; Center the widgets on display.

TextBox_lk2_CenterTLB, tlb
Widget_Control, tlb, /Realize

   ; Create a pointer for the text the user will type into the program.
   ; The cancel field is set to 1 to indicate that the user canceled
   ; the operation. Only if a successful conclusion is reached (i.e.,
   ; a Carriage Return or Accept button selection) is the cancel field
   ; set to 0.

ptr = Ptr_New({text:"", text2:"", text3:"",text4:"",text8:"",text7:""})

   ; Store the program information:

info2 = {ptr:ptr, textID:textID, textID2:textID2, textID3:textID3, textID4:textID4,$
        textID8:textID8, textID7:textID7}
Widget_Control, tlb, Set_UValue=info2, /No_Copy

   ; Blocking or modal widget, depending upon group leader.

XManager, 'textbox_lk2', tlb

   ; Return from block. Return the text to the caller of the program,
   ; taking care to clean up pointer and group leader, if needed.
   ; Set the cancel keyword.

;---update the pointer variables -----
*info.date = (*ptr).text
*info.path = (*ptr).text2
*info.path1 = (*ptr).text3
*info.wpath = (*ptr).text4
*info.polpath = (*ptr).text8
*info.la = (*ptr).text7
Ptr_Free, ptr


;----- write txt file for next program call ---
 openw,1,'ibis_paths.txt'
 printf,1,*info.date  ;yyyymmdd
 printf,1,*info.path  ;path to original data
 printf,1,*info.path1 ;path to save data
 printf,1,*info.wpath ;path to original wl data
 printf,1,*info.polpath ;path to polcal if taken on different date
 printf,1,*info.la ;wavelengths to reduce
 close,1



IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy

;la is a string in txt file and input file, for example '6302 8542'
;make string array from that for program use
*info.la = strsplit(*info.la,' ',/extract)

RETURN, 'done'
END ;-----------------------------------------------------


