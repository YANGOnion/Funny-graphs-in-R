library(grid)

heart=function(lcolor){
  t=seq(0,2*pi,by=0.1)
  x=16*sin(t)^3
  y=13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
  a=(x-min(x))/(max(x)-min(x))
  b=(y-min(y))/(max(y)-min(y))
  grid.lines(a,b,gp=gpar(col=lcolor,lty="solid",lwd=3))
}
heart("hotpink")

rose=function(cenX,cenY,sizeW,sizeH,rotate){
  vp=viewport(x=cenX,y=cenY,w=sizeW,h=sizeH,angle=rotate)
  pushViewport(vp)
  grid.polygon(x=c(0.5,0.5+sqrt(3)/4,0.5+sqrt(3)/4,0.5,0.5-sqrt(3)/4,0.5-sqrt(3)/4),
               y=c(0,0.25,0.75,1,0.75,0.25),
               gp=gpar(fill="red",lwd=2,col="white"))
  vp_iter=viewport(x=0.5,y=0.5,w=sqrt(3)/2,h=sqrt(3)/2,angle=-30)
  for(i in 1:4){
    pushViewport(vp_iter)
    grid.polygon(x=c(0.5,0.5+sqrt(3)/4,0.5+sqrt(3)/4,0.5,0.5-sqrt(3)/4,0.5-sqrt(3)/4),
                 y=c(0,0.25,0.75,1,0.75,0.25),
                 gp=gpar(fill="red",lwd=2,col="white"))
  }
  pushViewport(vp_iter)
  grid.circle(x=0.5,y=0.5,r=0.5,gp=gpar(fill="orangered4",lwd=2,col="white"))
}
rose(0.5,0.5,0.3,0.3,-10)

ring=function(){
  wave=function(a,R,p,w){
    x=(0.5+0.5*R*sin(p+w*a))*cos(a)
    y=(0.5+0.5*R*sin(p+w*a))*sin(a)
    return(cbind(x/1.2+0.5,y/1.2+0.5))
  }
  color=c("#3F8492","#E57660","#3F8492","#E57660","#3F8492","#E57660")
  a=seq(0,2*pi,length.out=100)
  pinc=2*pi/5
  for(i in 1:5)
    grid.lines(wave(a,0.2,pinc*(i-1),2)[,1],wave(a,0.2,pinc*(i-1),2)[,2],gp=gpar(col=sprintf('%s%2x',color[i],0.6*255),lwd=7))
}
ring()

grid.newpage()
ring()
pushViewport(viewport(.5,.5,w=.6,h=.6))
vp1=viewport(.4,.5,w=.5,h=.5,angle=15)
pushViewport(vp1)
heart("lightslateblue")
vp2=viewport(.9,.27,w=.7,h=.7,angle=-30)
pushViewport(vp2)
heart("hotpink")
vp_text=viewport(.5,.5,w=.9,h=.9)
grid.text("Happy Night of Sevens",
          x=0.2,y=1.5,just=c("center","bottom"),
          gp=gpar(fontsize=20,fontface="italic"),vp=vp_text)
grid.text("to ZZY!",
          x=0.2,y=1.2,just=c("center","bottom"),
          gp=gpar(fontsize=20,fontface="italic"),vp=vp_text)
vp3=viewport(-0.95,1.2,w=.6,h=.6,angle=-30)
pushViewport(vp3)
rose(0.5,0.5,0.8,0.8,-10)



















