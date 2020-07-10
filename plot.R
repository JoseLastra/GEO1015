coropleta<-function(campo,quiebres,nClases,shp,paleta,revertir,etiqueta){

  
  if(revertir ==TRUE){
  colores<-rev(brewer.pal(n = nClases, name = paleta))}
  if(revertir ==FALSE){
    colores<-brewer.pal(n = nClases, name = paleta)}
  #main body for plot
 
 ##Set up visualization parameters
  columna<-colnames(shp)
  campo_quiebres<-as.data.frame(shp[,which(columna==campo)])[,1]
  breaks.plot<-getBreaks(v = campo_quiebres, nclass = nClases,method = quiebres)
  
  #plot layout
  
  
  if(etiqueta ==TRUE){
    mapa_base<-tm_shape(shp)+
      tm_polygons(campo,n=nClases,palette=colores,border.col = 'black',border.alpha = 0.2,
                  breaks=breaks.plot)+
      tm_compass(type = "arrow", position = c("right", "top")) +
      tm_scale_bar(breaks = c(0, 25, 50), text.size = 1,position = c('left','bottom'))+
      tm_layout(legend.outside = F,title.size = 1,frame = F,
                legend.position = c('left','top')) +
      tm_text("label",size =0.43,legend.size.show = F)}
  if(etiqueta ==FALSE){
    mapa_base<-tm_shape(shp)+
      tm_polygons(campo,n=nClases,palette=colores,border.col = 'black',border.alpha = 0.2,
                  breaks=breaks.plot)+
      tm_compass(type = "arrow", position = c("right", "top")) +
      tm_scale_bar(breaks = c(0, 25, 50), text.size = 1,position = c('left','bottom'))+
      tm_layout(legend.outside = F,title.size = 1,frame = F,
                legend.position = c('left','top'))}
  
  mapa_base
  

}