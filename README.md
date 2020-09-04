# Ecovid
Encuesta E-covid
##Hogares mexicanos más allá del confinamiento: Un análisis de desempleo y subocupación en tiempos de pandemia##
##Ali M. Arrieta##
##28-08-2020##


rm(list=ls())
gc()
setwd("~/Trabajo&Covid")

Ecovid <- read.csv("ECOVID/ecovid0420.csv", header=TRUE)

library(tidyverse)
library(car) 
#install.packages('fdth')
library(fdth)

summary(Ecovid$CLASE2)
Ecovid <- Ecovid %>% 
  mutate(
    PNEA_disp=case_when(
      CLASE2==3 ~ 1,
      CLASE2==1 ~ 0,
      CLASE2==2 ~ 0,
      CLASE2==4 ~ 0),
    PNEA_disp=factor(PNEA_disp, levels = c("1", "0"), labels = c("Disponibles", "No disponibles")
    )
  )
summary(Ecovid$PNEA_disp)


##Variable dependiente##
summary(Ecovid$PC6)
Ecovid <- Ecovid %>% 
  mutate(
    IncluML_disp=case_when(
      PC6==1|PC6==2|PC6==3|PC6==4 ~1,
      PC6==5|PC6==6~2,
      PD1==2~3),
    IncluML1_disp=IncluML_disp,
    IncluML_disp=factor(IncluML_disp, levels = c("1", "2","3"), labels = c("Regresa", "No regresa", "Disponible")
    )
   )
summary(Ecovid$IncluML_disp)

Disponibles <- filter(Ecovid, IncluML_disp %in% c("Regresa", "No regresa", "Disponible"), PNEA_disp=="Disponibles")
summary(Disponibles$IncluML_disp)

##Variable independientes##

##Edad##
plot(Disponibles$PB2, type="l")
a <- fdt(Disponibles$PB2, start=18, end=98, h=1)

Disponibles <- Disponibles  %>% 
  mutate(
    Edad_disp=PB2,
    Group_edad_disp= case_when(
    Edad_disp <30 ~ 1,
    Edad_disp >=30 & Edad_disp <40 ~ 2,
    Edad_disp >=40 & Edad_disp <50 ~ 3,
    Edad_disp >=50 & Edad_disp <60 ~ 4,
    Edad_disp >=60 & Edad_disp <99 ~ 5),
    Group_edad_disp=factor(Group_edad_disp, levels = c("1", "2","3","4","5"), labels = c("18-29", "30-39", "40-49","50-59","60+")
    )
  )

summary(Disponibles$Edad_disp)
table1 <- summary(Disponibles$Group_edad_disp)
tabla1 <- as.data.frame(table(grupos=Disponibles$Group_edad_disp))
b <- transform(tabla1,
          Acum=cumsum(Freq),
          Prop=round(prop.table(Freq),2),
          Prop.Acum=round(cumsum(prop.table(Freq)),2))

##Sexo y educacion##
summary(Disponibles$PB1)
summary(Disponibles$PB3)

Disponibles <- Disponibles  %>% 
  mutate(
    Sexo_disp= case_when(
      PB1==1~0,
      PB1==2~1),
    Sexo_disp=factor(Sexo_disp, levels = c("0","1"), labels = c("Hombre", "Mujer")),
    Escolaridad_disp=case_when(
      PB3==0|PB3==1|PB3==2 ~1,
      PB3==3~2,
      PB3==4|PB3==5|PB3==6|PB3==7~3,
      PB3==8|PB3==9~4),
    Escolaridad_disp=factor(Escolaridad_disp, levels =c("1","2","3","4") , labels = c("Primaria", "Secundaria", "Prepa-Bachill", "Lic.Maest.Doc"))
      )

summary(Disponibles$Sexo_disp)
summary(Disponibles$Escolaridad_disp)
table(Disponibles$Escolaridad_disp)
tabla2 <- as.data.frame(table(grupos=Disponibles$Escolaridad_disp))

##Estructura y composicion del hogar##
summary(Disponibles$PA1)
summary(Disponibles$PA2_1_1)
summary(Disponibles$PA2_2_1)
Disponibles <- Disponibles  %>% 
  mutate(
    Num_Homb_disp=PA2_1_1,
    Num_Muj_disp=PA2_2_1,
    Num_Homb_disp= recode(Num_Homb_disp, "NA=0"),
    Num_Muj_disp= recode(Num_Muj_disp, "NA=0"),
    Num_Total_disp= (Num_Homb_disp+Num_Muj_disp)
     )
##Composicion##
Aux1 <- Disponibles
Aux2<-   Aux1 %>%
  pivot_longer(cols = c(`PA2_H_1`,`PA2_H_2`,`PA2_H_3`,`PA2_H_4`,`PA2_H_5`,`PA2_H_6`,`PA2_H_7`,`PA2_H_8`,`PA2_H_9`), names_to = "Hombres", values_to = "Edad_H") %>%
  mutate(
  H65_disp= case_when(
    Edad_H>65 ~1,
    Edad_H<=65~0),
  H15_disp= case_when(
    Edad_H<15 ~1,
    Edad_H>=15~0),
  H15_65_disp= case_when(
    Edad_H>=15 & Edad_H<=65~1,
    Edad_H<15 & Edad_H>65  ~0),
  H15_disp=recode(H15_disp, "1=1; NA=0"),
  H65_disp=recode(H65_disp, "1=1; NA=0"),
  H15_65_disp=recode(H15_65_disp, "1=1; NA=0")
)

Aux3<-   Aux1 %>%  
  pivot_longer(cols = c(`PA2_M_1`,`PA2_M_2`,`PA2_M_3`,`PA2_M_4`,`PA2_M_5`,`PA2_M_6`,`PA2_M_7`,`PA2_M_8`,`PA2_M_9`), names_to = "Mujeres", values_to = "Edad_M") %>%
   mutate(
    M65_disp= case_when(
      Edad_M>65 ~1,
      Edad_M<=65~0),
    M15_disp= case_when(
      Edad_M<15 ~1,
      Edad_M>=15~0),
    M15_65_disp= case_when(
      Edad_M>=15 & Edad_M<=65~1,
      Edad_M<15 & Edad_M>65  ~0),
    M15_disp=recode(M15_disp, "1=1; NA=0"),
    M65_disp=recode(M65_disp, "1=1; NA=0"),
    M15_65_disp=recode(M15_65_disp, "1=1; NA=0")
    )
   
   
Hombres_1565  <- Aux2 %>% 
  count (Id, H15_65_disp , sort  =  TRUE , name  =  "Hombres_1565_disp")
Hombres_1565  <-   filter(Hombres_1565, H15_65_disp %in% c("1"))  

Hombres_15  <- Aux2 %>% 
  count (Id,H15_disp , sort  =  TRUE , name  =  "Hombres_15_disp" )
Hombres_15  <-   filter(Hombres_15, H15_disp %in% c("1"))  

Hombres_65  <- Aux2 %>% 
  count (Id,H65_disp , sort  =  TRUE , name  =  "Hombres_65_disp" )
Hombres_65  <-   filter(Hombres_65, H65_disp %in% c("1"))  


Mujeres_1565  <- Aux3 %>% 
  count (Id, M15_65_disp , sort  =  TRUE , name  =  "Mujeres_1565_disp")
Mujeres_1565  <-   filter(Mujeres_1565, M15_65_disp %in% c("1"))  

Mujeres_15  <- Aux3 %>% 
  count (Id,M15_disp , sort  =  TRUE , name  =  "Mujeres_15_disp" )
Mujeres_15  <-   filter(Mujeres_15, M15_disp %in% c("1"))  

Mujeres_65  <- Aux3 %>% 
  count (Id,M65_disp , sort  =  TRUE , name  =  "Mujeres_65_disp" )
Mujeres_65  <-   filter(Mujeres_65, M65_disp %in% c("1"))  


Disponibles <- merge(Disponibles,Hombres_1565, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Hombres_15, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Hombres_65, by = c("Id"), all.x=TRUE)

Disponibles <- merge(Disponibles,Mujeres_1565, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Mujeres_15, by = c("Id"), all.x=TRUE)
Disponibles <- merge(Disponibles,Mujeres_65, by = c("Id"), all.x=TRUE)

Disponibles <- Disponibles %>%
               mutate(
                 Hombres_1565_disp=recode(Hombres_1565_disp, "NA=0"),
                 Hombres_15_disp=recode(Hombres_15_disp, "NA=0"),
                 Hombres_65_disp=recode(Hombres_65_disp, "NA=0"),
                 Mujeres_1565_disp=recode(Mujeres_1565_disp, "NA=0"),
                 Mujeres_15_disp=recode(Mujeres_15_disp, "NA=0"),
                 Mujeres_65_disp=recode(Mujeres_65_disp, "NA=0"),
                 Suma_H=(Hombres_1565_disp+Hombres_15_disp+Hombres_65_disp),
                 Suma_M=(Mujeres_1565_disp+Mujeres_15_disp+Mujeres_65_disp),
                 Prueba_H=(Num_Homb_disp-Suma_H),
                 Prueba_M=(Num_Muj_disp-Suma_M)
                  )
str(Disponibles$Prueba_H)
str(Disponibles$Prueba_M)

Disponibles<- select(Disponibles, -H15_65_disp,-H15_disp,-H65_disp)
Disponibles<- select(Disponibles, -M15_65_disp,-M15_disp,-M65_disp)

Disponibles <- rename(Disponibles, Id_disp = Id, PER_disp = PER, Entidad_disp = ENT)

Disponibles <- Disponibles %>%
  select(ends_with("_disp"))

##Indicadores de estructura y composicion##  

library(dplyr) 
  
  Disponibles  %>% 
  select(Hombres_15_disp, Hombres_65_disp, Mujeres_15_disp, Mujeres_65_disp) %>% 
  rowSums(na.rm=TRUE) -> Disponibles$Pob15y65_disp 
    
  Disponibles  %>% 
  select(Hombres_1565_disp, Mujeres_1565_disp) %>%
  rowSums(na.rm=TRUE) -> Disponibles$Pobprod_disp 
   
   
   Disponibles <- Disponibles%>%
                  mutate(IDD_disp=(Pob15y65_disp/Pobprod_disp),
                         RM_disp=(Num_Homb_disp/Num_Muj_disp)
                         )
 ##Tablas##        

  Disponibles <- Disponibles %>% 
                 mutate(
                  Regresa=case_when(
                  IncluML1_disp==1~1,
                  IncluML1_disp!=1~0),
                  No_Regresa=case_when(
                    IncluML1_disp==2~1,
                    IncluML1_disp!=2~0),
                  Disponible=case_when(
                    IncluML1_disp==3~1,
                    IncluML1_disp!=3~0)
                 )
##Gra1##                     

    
    Tab_1 <- Disponibles %>%
      group_by(Sexo_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_1<-   Tab_1 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    ggplot(Tab_1, aes(x=Condición, y=Proporción, fill=Sexo_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30", "gray5")) +
      theme_bw()
    
    ##Grap2##
    Tab_2 <- Disponibles %>%
      group_by(Group_edad_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_2<-   Tab_2 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    ggplot(Tab_2, aes(x=Condición, y=Proporción, fill=Group_edad_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30","gray20", "gray10", "gray5","gray1" )) +
      theme_bw()+
      coord_flip()
    
    ##Grap3##
    Tab_3 <- Disponibles %>%
      group_by(Escolaridad_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_3<-   Tab_3 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    ggplot(Tab_3, aes(x=Condición, y=Proporción, fill=Escolaridad_disp)) +
      geom_bar(stat = "identity", position = "dodge")+
      labs(title="Proporción de población disponible por categorias",
           x="Condición de los disponibles",
           y="Proporción(%)")+
      scale_fill_manual(values=c("gray30","gray20", "gray10", "gray5" )) +
      theme_bw()
      
    ##Grap4##
    Tab_4 <- Disponibles %>%
      group_by(Num_Total_disp) %>%
      summarize(Mean_regresa=(mean(Regresa)*100),
                Mean_NOregresa=(mean(No_Regresa)*100),
                Mean_Dispobibles=(mean(Disponible)*100)
      )
    
    Tab_4<-   Tab_4 %>%  
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
 
    Tab_4<-  filter(Tab_4 , Num_Total_disp %in% c("1", "2","3", "4", "5", "6","7", "8", "9", "10", "11", "12"))  
    
    
    ggplot(Tab_4, aes(x=Num_Total_disp, y=Proporción, color=Condición))+
      geom_line(size=2)+
      geom_point(size=4)+
      labs(title="Proporción de población disponible por categorias por tamaño promedio del Hogar",
           x="Número promedio de personas por Hogar",
           y="Proporción (%)")+
      theme_minimal()+
      scale_linetype_manual(values=c("gray30", "gray10", "gray5" )) +
      theme(plot.title=element_text(hjust = 0.5, size = 10))
      
  
    ##Grap5##
    Disponibles <-  Disponibles %>% 
      mutate(
      IDD_disp=recode(IDD_disp, "Inf=0") )
    
    Tab_5 <- Disponibles %>%
      group_by(Escolaridad_disp) %>%
      summarize(Mean_IDD=(mean(IDD_disp))
       )
    
    Tab_5<-   Tab_5 %>%  
      M15_65_disp=recode(M15_65_disp, "1=1; NA=0")
      pivot_longer(cols = c(`Mean_regresa`,`Mean_NOregresa`,`Mean_Dispobibles`), names_to = "Condición", values_to = "Proporción")
    
    
    ggplot(Tab_5, aes(x=IDD_disp, y=Proporción, color=Condición))+
      geom_line(size=2)+
      geom_point(size=4)+
      labs(title="Proporción de población disponible por categorias por tamaño promedio del Hogar",
           x="Número promedio de personas por Hogar",
           y="Proporción (%)")+
      theme_minimal()+
      scale_linetype_manual(values=c("gray30", "gray10", "gray5" )) +
      theme(plot.title=element_text(hjust = 0.5, size = 10))
    
    
    
 summary(Disponibles$Num_Total_disp)
 summary(Disponibles$IDD_disp)
 summary(Disponibles$RM_disp)
 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
