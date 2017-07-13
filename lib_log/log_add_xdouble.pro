pro log_add_xdouble,path1,xmat,wpoff,wpret,savefile1,xmat2,wpoff2,wpret2,savefile2,lambda
;adds variables that were set to log file
time=systime()

newtext = strarr(17) ;new entries for log

newtext[0] = 'ibis_polcal_xcalc.pro to calculate X matrix for '+string(lambda)+' \\'
newtext[1] = string(time)+'\\'
newtext[2] = '$$ X_{left} =  \left( \begin{array}{cccc}'
newtext[3] = string(xmat[0,0])+' & '+string(xmat[1,0])+$
       ' & '+string(xmat[2,0])+' & '+string(xmat[3,0])+' \\'
newtext[4] = string(xmat[0,1])+' & '+string(xmat[1,1])+$
       ' & '+string(xmat[2,1])+' & '+string(xmat[3,1])+' \\'
newtext[5] = string(xmat[0,2])+' & '+string(xmat[1,2])+$
       ' & '+string(xmat[2,2])+' & '+string(xmat[3,2])+' \\'
newtext[6] = string(xmat[0,3])+' & '+string(xmat[1,3])+$
       ' & '+string(xmat[2,3])+' & '+string(xmat[3,3])+' \end{array} \right) $$' 
newtext[7] = ' Wave Plate Offset [deg]: '+string(wpoff)+$
      ', and Retardance [deg]: '+string(wpret)+' \\'
newtext[8] = 'Saved in: '+string(savefile1)+' \\'

newtext[9] = '$$ X_{right} =  \left( \begin{array}{cccc}'
newtext[10] = string(xmat2[0,0])+' & '+string(xmat2[1,0])+$
       ' & '+string(xmat2[2,0])+' & '+string(xmat2[3,0])+' \\'
newtext[11] = string(xmat2[0,1])+' & '+string(xmat2[1,1])+$
       ' & '+string(xmat2[2,1])+' & '+string(xmat2[3,1])+' \\'
newtext[12] = string(xmat2[0,2])+' & '+string(xmat2[1,2])+$
       ' & '+string(xmat2[2,2])+' & '+string(xmat2[3,2])+' \\'
newtext[13] = string(xmat2[0,3])+' & '+string(xmat2[1,3])+$
       ' & '+string(xmat2[2,3])+' & '+string(xmat2[3,3])+' \end{array} \right) $$' 
newtext[14] = ' Wave Plate Offset [deg]: '+string(wpoff2)+$
      ', and Retardance [deg]: '+string(wpret2)+' \\'
newtext[15] = 'Saved in: '+string(savefile2)+' \\'

newtext[16] = ' '


;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext


print,'writing variables into log'
log_write,path1,newtext

end
