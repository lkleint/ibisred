 set_plot,'PS'
 device,filename='icrosstalk.eps',/encaps
 device,xsize=20,ysize=25
!p.multi=[0,3,5]

plot,avg(avg(ii[50:100,10:20,*],0),0),title='Intensity'   
plot,avg(avg(qq[50:100,10:20,*],0),0),title='Q orig'           
plot,avg(avg(qmap[50:100,10:20,*],0),0),title='Q after I->Q,U,V corr'

plot,avg(avg(ii[50:100,100:120,*],0),0),title='Intensity'   
plot,avg(avg(qq[50:100,100:120,*],0),0),title='Q orig'           
plot,avg(avg(qmap[50:100,100:120,*],0),0),title='Q after I->Q,U,V corr'

plot,avg(avg(ii[250:300,100:120,*],0),0),title='Intensity'   
plot,avg(avg(qq[250:300,100:120,*],0),0),title='Q orig'           
plot,avg(avg(qmap[250:300,100:120,*],0),0),title='Q after I->Q,U,V corr'

plot,avg(avg(ii[250:300,400:420,*],0),0),title='Intensity'   
plot,avg(avg(qq[250:300,400:420,*],0),0),title='Q orig'           
plot,avg(avg(qmap[250:300,400:420,*],0),0),title='Q after I->Q,U,V corr'
 
plot,avg(avg(ii[250:300,600:620,*],0),0),title='Intensity'   
plot,avg(avg(qq[250:300,600:620,*],0),0),title='Q orig'           
plot,avg(avg(qmap[250:300,600:620,*],0),0),title='Q after I->Q,U,V corr'

device,/close
set_plot,'x'


 set_plot,'PS'
 device,filename='icrosstalk2.eps',/encaps
 device,xsize=20,ysize=25
!p.multi=[0,3,5]
!p.charsize=1.2
plot,wl_obs,avg(avg(ii[50:100,10:20,*],0),0),title='Intensity',xthick=2,ythick=2,thick=2,charthick=2,/xs,psym=-4
plot,wl_obs,avg(avg(qq[50:100,10:20,*],0),0),title='Q orig'           ,/xs
plot,wl_obs,avg(avg(qmap[50:100,10:20,*],0),0),title='Q after I->Q,U,V corr',/xs

plot,wl_obs,avg(avg(ii[50:100,100:120,*],0),0),title='Intensity'   ,xthick=2,ythick=2,thick=2,charthick=2,/xs,psym=-4
plot,wl_obs,avg(avg(qq[50:100,100:120,*],0),0),title='Q orig'           ,/xs
plot,wl_obs,avg(avg(qmap[50:100,100:120,*],0),0),title='Q after I->Q,U,V corr',/xs

plot,wl_obs,avg(avg(ii[250:300,100:120,*],0),0),title='Intensity'   ,xthick=2,ythick=2,thick=2,charthick=2,/xs,psym=-4
plot,wl_obs,avg(avg(qq[250:300,100:120,*],0),0),title='Q orig'           ,/xs
plot,wl_obs,avg(avg(qmap[250:300,100:120,*],0),0),title='Q after I->Q,U,V corr',/xs

plot,wl_obs,avg(avg(ii[250:300,400:420,*],0),0),title='Intensity'  ,xthick=2,ythick=2,thick=2,charthick=2 ,/xs,psym=-4
plot,wl_obs,avg(avg(qq[250:300,400:420,*],0),0),title='Q orig'           ,/xs
plot,wl_obs,avg(avg(qmap[250:300,400:420,*],0),0),title='Q after I->Q,U,V corr',/xs
 
plot,wl_obs,avg(avg(ii[250:300,600:620,*],0),0),title='Intensity'   ,xthick=2,ythick=2,thick=2,charthick=2,/xs,psym=-4
plot,wl_obs,avg(avg(qq[250:300,600:620,*],0),0),title='Q orig'           ,/xs
plot,wl_obs,avg(avg(qmap[250:300,600:620,*],0),0),title='Q after I->Q,U,V corr',/xs

device,/close
set_plot,'x'
