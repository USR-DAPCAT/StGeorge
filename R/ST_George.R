#' Sant Georges is Disease-specific instrument
#'
#' More detailed description.
#'
#' @param dt data.frame
#'
#' @return  dt and SYMPTOM,ACTIVIT,IMPACTS,TOTALSG variables
#'
#' @examples
#' BD <- data.frame(
#'"sg1" = c(0, 1, 1, 0),
#'"sg2" = c(0, 1, 1, 0),
#'"sg3" = c(0, 1, 1, 0),
#'"sg4" = c(0, 1, 1, 0),
#'"sg5" = c(0, 1, 1, 0),
#'"sg6" = c(0, 1, 1, 0),
#'"sg7" = c(0, 1, 1, 0),
#'"sg8" = c(0, 1, 1, 0),
#'"sg9" = c(0, 1, 1, 0),
#'"sg10" = c(0, 1, 1, 0),
#'"sg111" = c(0, 1, 1, 0),
#'"sg112" = c(0, 1, 1, 0),
#'"sg113" = c(0, 1, 1, 0),
#'"sg114" = c(0, 1, 1, 0),
#'"sg115" = c(1, 1, 1, 1),
#'"sg116" = c(1, 1, 1, 1),
#'"sg117" = c(1, 1, 1, 1),
#'"sg121" = c(1, 1, 1, 1),
#'"sg122" = c(1, 1, 1, 1),
#'"sg123" = c(1, 1, 1, 1),
#'"sg124" = c(1, 1, 1, 1),
#'"sg125" = c(1, 1, 1, 1),
#'"sg126" = c(1, 1, 1, 1),
#'"sg131" = c(1, 1, 1, 1),
#'"sg132" = c(1, 1, 1, 1),
#'"sg133" = c(1, 1, 1, 1),
#'"sg134" = c(1, 1, 1, 1),
#'"sg135" = c(1, 1, 1, 1),
#'"sg136" = c(1, 1, 1, 1),
#'"sg137" = c(1, 1, 1, 1),
#'"sg138" = c(1, 1, 1, 1),
#'"sg141" = c(1, 1, 1, 1),
#'"sg142" = c(1, 1, 1, 1),
#'"sg143" = c(1, 1, 1, 1),
#'"sg144" = c(1, 1, 1, 1),
#'"sg151" = c(1, 1, 1, 1),
#'"sg152" = c(1, 1, 1, 1),
#'"sg153" = c(1, 1, 1, 1),
#'"sg154" = c(1, 1, 1, 1),
#'"sg155" = c(1, 0, 1, 0),
#'"sg156" = c(1, 0, 1, 0),
#'"sg157" = c(1, 0, 1, 0),
#'"sg158" = c(1, 0, 1, 0),
#'"sg159" = c(1, 0, 1, 0),
#'"sg161" = c(1, 0, 1, 0),
#'"sg162" = c(1, 0, 1, 0),
#'"sg163" = c(1, 0, 1, 0),
#'"sg164" = c(1, 0, 1, 0),
#'"sg165" = c(1, 0, 1, 0),
#'"sg17" = c(1, 0, 1, 0)
#')
#'ST_George(BD)
#' @export
ST_George<-function(dt) {


  dades_copia<-dt

  #0_i)

  dt<-dt %>% rename_at(vars(starts_with("SG")),.funs = tolower)

  vector_vars<-c("sg1","sg2","sg3","sg4","sg5","sg6","sg7","sg8","sg9","sg10","sg111","sg112","sg113","sg114",
                 "sg115","sg116","sg117","sg121","sg122","sg123","sg124","sg125","sg126","sg131","sg132","sg133",
                 "sg134","sg135","sg136","sg137", "sg138","sg141","sg142","sg143","sg144","sg151","sg152","sg153",
                 "sg154","sg155","sg156","sg157","sg158","sg159","sg161","sg162","sg163","sg164","sg165","sg17")
  #----------------------------------------------------------------------------#
  cols<-c(vector_vars)%in%colnames(dt)

  varsquefalten<-vector_vars[!(c(vector_vars)%in%colnames(dt))] %>% paste0(collapse = ", ")


  count <- length(which(cols== TRUE))
  #----------------------------------------------------------------------------#


  if(count<50)

  {

    return(print(paste0("Missing some variable!", varsquefalten,"we cannot do the function: SANT_GEORGE ")))


  }



  dt<-dt %>% select(vector_vars)

  # Tot numeric
  dt<-dt %>% mutate_all(as.numeric)

  sel1<-c("sg1","sg2","sg3","sg4","sg5","sg6","sg7")
  sel2<-c("sg8")
  sel3<-c("sg10")
  sel4<-c("sg9","sg17")
  sel5<-c("sg111","sg112","sg113","sg114",
          "sg115","sg116","sg117","sg121","sg122","sg123","sg124","sg125","sg126","sg131","sg132","sg133",
          "sg134","sg135","sg136","sg137", "sg138","sg141","sg142","sg143","sg144","sg151","sg152","sg153",
          "sg154","sg155","sg156","sg157","sg158","sg159","sg161","sg162","sg163","sg164","sg165")

  dt<-dt %>% mutate_at(sel1,~if_else(.<1 | .>5,NA_real_,.))
  dt<-dt %>% mutate_at(sel2,~if_else((.<1  | .>2) & .!=8  ,NA_real_,.))
  dt<-dt %>% mutate_at(sel3,~if_else((.<1  | .>3) & .!=8  ,NA_real_,.))
  dt<-dt %>% mutate_at(sel4,~if_else(.<1 | .>4,NA_real_,.))
  dt<-dt %>% mutate_at(sel5,~if_else(.<0 | .>1,NA_real_,.))

  #0_ii)
  # Recode the missings!
  dt <-dt %>%mutate_at(dplyr::vars("sg1":"sg17"),~if_else(is.na(.),9,.))

  # 0_iii)
  # Recode 2 to 0!
  dt <-dt %>%
    mutate_at(dplyr::vars("sg111":"sg165"),~ifelse(.==2,0,.))

  #0_iv)
  # Invalid values in variable SG10 , (IF (SG10 EQ 4) SG10=9).
  #dt<-dt%>%
  #  mutate(sg10=case_when(sg10>=4~ 9,TRUE ~ sg10))



  #1)
  #*SG1
  dt<-dt%>%mutate(PSG1=case_when(sg1==1~ 80.6,
                                 sg1==2~ 63.2,
                                 sg1==3~ 29.3,
                                 sg1==4~ 28.1,
                                 sg1==5~ 0,
                                 sg1==9~ 0))
  #2)
  #*SG2
  dt<-dt%>%mutate(PSG2=case_when(sg2==1~ 76.8,
                                 sg2==2~ 60.0,
                                 sg2==3~ 34.0,
                                 sg2==4~ 30.2,
                                 sg2==5~ 0,
                                 sg2==9~ 0))
  #3)
  #*SG3
  dt<-dt%>%mutate(PSG3=case_when(sg3==1~ 87.2,
                                 sg3==2~ 71.4,
                                 sg3==3~ 43.7,
                                 sg3==4~ 35.7,
                                 sg3==5~ 0,
                                 sg3==9~ 0))
  #4)
  #*SG4
  dt<-dt%>%mutate(PSG4=case_when(sg4==1~ 86.2,
                                 sg4==2~ 71.0,
                                 sg4==3~ 45.6,
                                 sg4==4~ 36.4,
                                 sg4==5~ 0,
                                 sg4==9~ 0))
  #5)
  #*SG5
  dt<-dt%>%mutate(PSG5=case_when(sg5==1~ 86.7,
                                 sg5==2~ 73.5,
                                 sg5==3~ 60.3,
                                 sg5==4~ 44.2,
                                 sg5==5~ 0,
                                 sg5==9~ 0))
  #6)
  #*SG6
  dt<-dt%>%mutate(PSG6=case_when(sg6==1~ 89.7,
                                 sg6==2~ 73.5,
                                 sg6==3~ 58.8,
                                 sg6==4~ 41.9,
                                 sg6==8~ 0,
                                 sg6==9~ 0))
  #7)
  #*SG7
  dt<-dt%>%mutate(PSG7=case_when(sg7==1~ 93.3,
                                 sg7==2~ 76.6,
                                 sg7==3~ 61.5,
                                 sg7==4~ 15.4,
                                 sg7==5~ 0,
                                 sg7==9~ 0))
  #8)
  #*SG8
  dt<-dt%>%mutate(PSG8=case_when(sg8==1~ 0,
                                 sg8==2~ 62.0,
                                 sg8==9~ 0,
                                 sg8==8~ 0))

  #9)
  #*SG9
  dt<-dt%>%mutate(PSG9=case_when(sg9==1~ 83.2,
                                 sg9==2~ 82.5,
                                 sg9==3~ 34.6,
                                 sg9==4~ 0,
                                 sg9==9~ 0))
  #10)
  #*SG10
  dt<-dt%>%mutate(PSG10=case_when(sg10==1~ 88.9,
                                  sg10==2~ 77.6,
                                  sg10==3~ 0,
                                  sg10==8~ 0,
                                  sg10==9~ 0))

  #11)
  #SG111
  dt<-dt%>%mutate(PSG111=case_when(sg111==1~90.6,
                                   sg111==0~0,
                                   sg111==9~0))
  #11B)
  #SG111
  dt<-dt%>%mutate(R111=case_when(sg111==9~90.6,
                                 sg111==0~0,
                                 sg111==1~0))

  #12)
  #SG112
  dt<-dt%>%mutate(PSG112=case_when(sg112==1~82.8,
                                   sg112==0~0,
                                   sg112==9~0))
  #12B)
  #SG111
  dt<-dt%>%mutate(R112=case_when(sg112==9~82.8,
                                 sg112==0~0,
                                 sg112==1~0))

  #13)
  #SG113
  dt<-dt%>%mutate(PSG113=case_when(sg113==1~80.2,
                                   sg113==0~0,
                                   sg113==9~0))
  #13B)
  #SG113
  dt<-dt%>%mutate(R113=case_when(sg113==9~80.2,
                                 sg113==0~0,
                                 sg113==1~0))
  #14)
  #SG114
  dt<-dt%>%mutate(PSG114=case_when(sg114==1~81.4,
                                   sg114==0~0,
                                   sg114==9~0))
  #14B)
  #SG114
  dt<-dt%>%mutate(R114=case_when(sg114==9~81.4,
                                 sg114==0~0,
                                 sg114==1~0))
  #15)
  #SG115
  dt<-dt%>%mutate(PSG115=case_when(sg115==1~76.1,
                                   sg115==0~0,
                                   sg115==9~0))

  #15B)
  #SG115
  dt<-dt%>%mutate(R115=case_when(sg115==9~76.1,
                                 sg115==0~0,
                                 sg115==1~0))
  #16)
  #SG116
  dt<-dt%>%mutate(PSG116=case_when(sg116==1~75.1,
                                   sg116==0~0,
                                   sg116==9~0))
  #16B)
  #SG116
  dt<-dt%>%mutate(R116=case_when(sg116==9~75.1,
                                 sg116==0~0,
                                 sg116==1~0))
  #17)
  #SG117
  dt<-dt%>%mutate(PSG117=case_when(sg117==1~72,
                                   sg117==0~0,
                                   sg117==9~0))
  #17B)
  #SG117
  dt<-dt%>%mutate(R117=case_when(sg117==9~72,
                                 sg117==0~0,
                                 sg117==1~0))
  #18)
  #SG121
  dt<-dt%>%mutate(PSG121=case_when(sg121==1~81.1,
                                   sg121==0~0,
                                   sg121==9~0))
  #18B)
  #SG121
  dt<-dt%>%mutate(R121=case_when(sg121==9~81.1,
                                 sg121==0~0,
                                 sg121==1~0))
  #19)
  #SG122
  dt<-dt%>%mutate(PSG122=case_when(sg122==1~79.1,
                                   sg122==0~0,
                                   sg122==9~0))
  #19B)
  #SG122
  dt<-dt%>%mutate(R122=case_when(sg122==9~79.1,
                                 sg122==0~0,
                                 sg122==1~0))
  #20)
  #SG123
  dt<-dt%>%mutate(PSG123=case_when(sg123==1~84.5,
                                   sg123==0~0,
                                   sg123==9~0))
  #20B)
  #SG123
  dt<-dt%>%mutate(R123=case_when(sg123==9~84.5,
                                 sg123==0~0,
                                 sg123==1~0))
  #21)
  #SG124
  dt<-dt%>%mutate(PSG124=case_when(sg124==1~76.8,
                                   sg124==0~0,
                                   sg124==9~0))
  #21B)
  #SG124
  dt<-dt%>%mutate(R124=case_when(sg124==9~76.8,
                                 sg124==0~0,
                                 sg124==1~0))
  #22)
  #SG125
  dt<-dt%>%mutate(PSG125=case_when(sg125==1~87.9,
                                   sg125==0~0,
                                   sg125==9~0))
  #22B)
  #SG125
  dt<-dt%>%mutate(R125=case_when(sg125==9~87.9,
                                 sg125==0~0,
                                 sg125==1~0))
  #23)
  #SG126
  dt<-dt%>%mutate(PSG126=case_when(sg126==1~84.0,
                                   sg126==0~0,
                                   sg126==9~0))
  #23B)
  #SG126
  dt<-dt%>%mutate(R126=case_when(sg126==9~84.0,
                                 sg126==0~0,
                                 sg126==1~0))

  #24)
  #SG131
  dt<-dt%>%mutate(PSG131=case_when(sg131==1~74.1,
                                   sg131==0~0,
                                   sg131==9~0))
  #24B)
  #SG131
  dt<-dt%>%mutate(R131=case_when(sg131==9~74.1,
                                 sg131==0~0,
                                 sg131==1~0))
  #25)
  #SG132
  dt<-dt%>%mutate(PSG132=case_when(sg132==1~79.1,
                                   sg132==0~0,
                                   sg132==9~0))
  #25B)
  #SG132
  dt<-dt%>%mutate(R132=case_when(sg132==9~79.1,
                                 sg132==0~0,
                                 sg132==1~0))
  #26)
  #SG133
  dt<-dt%>%mutate(PSG133=case_when(sg133==1~87.7,
                                   sg133==0~0,
                                   sg133==9~0))
  #26B)
  #SG133
  dt<-dt%>%mutate(R133=case_when(sg133==9~87.7,
                                 sg133==0~0,
                                 sg133==1~0))
  #27)
  #SG134
  dt<-dt%>%mutate(PSG134=case_when(sg134==1~90.1,
                                   sg134==0~0,
                                   sg134==9~0))
  #27B)
  #SG134
  dt<-dt%>%mutate(R134=case_when(sg134==9~90.1,
                                 sg134==0~0,
                                 sg134==1~0))
  #28)
  #SG135
  dt<-dt%>%mutate(PSG135=case_when(sg135==1~82.3,
                                   sg135==0~0,
                                   sg135==9~0))
  #28B)
  #SG135
  dt<-dt%>%mutate(R135=case_when(sg135==9~82.3,
                                 sg135==0~0,
                                 sg135==1~0))
  #29)
  #SG136
  dt<-dt%>%mutate(PSG136=case_when(sg136==1~89.9,
                                   sg136==0~0,
                                   sg136==9~0))
  #29B)
  #SG136
  dt<-dt%>%mutate(R136=case_when(sg136==9~89.9,
                                 sg136==0~0,
                                 sg136==1~0))
  #30)
  #SG137
  dt<-dt%>%mutate(PSG137=case_when(sg137==1~75.7,
                                   sg137==0~0,
                                   sg137==9~0))
  #30B)
  #SG137
  dt<-dt%>%mutate(R137=case_when(sg137==9~75.7,
                                 sg137==0~0,
                                 sg137==1~0))
  #31)
  #SG138
  dt<-dt%>%mutate(PSG138=case_when(sg138==1~84.5,
                                   sg138==0~0,
                                   sg138==9~0))
  #31B)
  #SG138
  dt<-dt%>%mutate(R138=case_when(sg138==9~84.5,
                                 sg138==0~0,
                                 sg138==1~0))

  #32)
  #SG141
  dt<-dt%>%mutate(PSG141=case_when(sg141==1~88.2,
                                   sg141==0~0,
                                   sg141==9~0,
                                   sg141==8~0))
  #32B)
  #SG141
  dt<-dt%>%mutate(R141=case_when(sg141==9~88.2,
                                 sg141==0~0,
                                 sg141==1~0,
                                 sg141==8~88.2))
  #33)
  #SG142
  dt<-dt%>%mutate(PSG142=case_when(sg142==1~53.9,
                                   sg142==0~0,
                                   sg142==9~0,
                                   sg142==8~0))
  #33B)
  #SG142
  dt<-dt%>%mutate(R142=case_when(sg142==9~53.9,
                                 sg142==0~0,
                                 sg142==1~0,
                                 sg142==8~53.9))
  #34)
  #SG143
  dt<-dt%>%mutate(PSG143=case_when(sg143==1~81.1,
                                   sg143==0~0,
                                   sg143==9~0,
                                   sg143==8~0))
  #34B)
  #SG143
  dt<-dt%>%mutate(R143=case_when(sg143==9~81.1,
                                 sg143==0~0,
                                 sg143==1~0,
                                 sg143==8~81.1))
  #35)
  #SG144
  dt<-dt%>%mutate(PSG144=case_when(sg144==1~70.3,
                                   sg144==0~0,
                                   sg144==9~0,
                                   sg144==8~0))
  #35B)
  #SG144B
  dt<-dt%>%mutate(R144=case_when(sg144==9~70.3,
                                 sg144==0~0,
                                 sg144==1~0,
                                 sg144==8~70.3))
  #36)
  #SG151
  dt<-dt%>%mutate(PSG151=case_when(sg151==1~74.2,
                                   sg151==0~0,
                                   sg151==9~0))
  #36B)
  #SG151B
  dt<-dt%>%mutate(R151=case_when(sg151==9~74.2,
                                 sg151==0~0,
                                 sg151==1~0))
  #37)
  #SG152
  dt<-dt%>%mutate(PSG152=case_when(sg152==1~81.0,
                                   sg152==0~0,
                                   sg152==9~0))
  #37B)
  #SG152B
  dt<-dt%>%mutate(R152=case_when(sg152==9~81.0,
                                 sg152==0~0,
                                 sg152==1~0))
  #38)
  #SG153
  dt<-dt%>%mutate(PSG153=case_when(sg153==1~71.7,
                                   sg153==0~0,
                                   sg153==9~0))
  #38B)
  #SG153B
  dt<-dt%>%mutate(R153=case_when(sg153==9~71.7,
                                 sg153==0~0,
                                 sg153==1~0))
  #39)
  #SG154
  dt<-dt%>%mutate(PSG154=case_when(sg154==1~70.6,
                                   sg154==0~0,
                                   sg154==9~0))
  #39B)
  #SG154B
  dt<-dt%>%mutate(R154=case_when(sg154==9~70.6,
                                 sg154==0~0,
                                 sg154==1~0))
  #40)
  #SG155
  dt<-dt%>%mutate(PSG155=case_when(sg155==1~71.6,
                                   sg155==0~0,
                                   sg155==9~0))
  #40B)
  #SG155B
  dt<-dt%>%mutate(R155=case_when(sg155==9~71.6,
                                 sg155==0~0,
                                 sg155==1~0))
  #41)
  #SG156
  dt<-dt%>%mutate(PSG156=case_when(sg156==1~72.3,
                                   sg156==0~0,
                                   sg156==9~0))
  #41B)
  #SG156B
  dt<-dt%>%mutate(R156=case_when(sg156==9~72.3,
                                 sg156==0~0,
                                 sg156==1~0))
  #42)
  #SG157
  dt<-dt%>%mutate(PSG157=case_when(sg157==1~74.5,
                                   sg157==0~0,
                                   sg157==9~0))
  #42B)
  #SG157B
  dt<-dt%>%mutate(R157=case_when(sg157==9~74.5,
                                 sg157==0~0,
                                 sg157==1~0))
  #43)
  #SG158
  dt<-dt%>%mutate(PSG158=case_when(sg158==1~71.4,
                                   sg158==0~0,
                                   sg158==9~0))
  #43B)
  #SG158B
  dt<-dt%>%mutate(R158=case_when(sg158==9~71.4,
                                 sg158==0~0,
                                 sg158==1~0))
  #44)
  #SG159
  dt<-dt%>%mutate(PSG159=case_when(sg159==1~63.5,
                                   sg159==0~0,
                                   sg159==9~0))
  #44B)
  #SG159B
  dt<-dt%>%mutate(R159=case_when(sg159==9~63.5,
                                 sg159==0~0,
                                 sg159==1~0))
  #45)
  #SG161
  dt<-dt%>%mutate(PSG161=case_when(sg161==0~64.8,
                                   sg161==1~0,
                                   sg161==9~0))
  #45B)
  #SG161B
  dt<-dt%>%mutate(R161=case_when(sg161==9~64.8,
                                 sg161==0~0,
                                 sg161==1~0))

  #46)
  #SG162
  dt<-dt%>%mutate(PSG162=case_when(sg162==0~79.8,
                                   sg162==1~0,
                                   sg162==9~0))
  #46B)
  #SG162B
  dt<-dt%>%mutate(R162=case_when(sg162==9~79.8,
                                 sg162==0~0,
                                 sg162==1~0))
  #47)
  #SG163
  dt<-dt%>%mutate(PSG163=case_when(sg163==0~81.0,
                                   sg163==1~0,
                                   sg163==9~0))
  #47B)
  #SG163B
  dt<-dt%>%mutate(R163=case_when(sg163==9~81.0,
                                 sg163==0~0,
                                 sg163==1~0))
  #48)
  #SG164
  dt<-dt%>%mutate(PSG164=case_when(sg164==0~79.1,
                                   sg164==1~0,
                                   sg164==9~0))
  #48B)
  #SG164B
  dt<-dt%>%mutate(R164=case_when(sg164==9~79.1,
                                 sg164==0~0,
                                 sg164==1~0))
  #49)
  #SG165
  dt<-dt%>%mutate(PSG165=case_when(sg165==0~94.0,
                                   sg165==1~0,
                                   sg165==9~0))
  #49B)
  #SG165B
  dt<-dt%>%mutate(R165=case_when(sg165==9~94.0,
                                 sg165==0~0,
                                 sg165==1~0))
  #50)
  #SG17
  dt<-dt%>%mutate(PSG17=case_when(sg17==1~0,
                                  sg17==2~42.0,
                                  sg17==3~84.2,
                                  sg17==4~96.7,
                                  sg17==9~0))

  #-------------------------------------------------#
  #*calculation of the score of each scale:
  #-------------------------------------------------#


  #-------------------------------------------------#
  #*SYMPTOMS:
  dt<-dt%>%mutate(SYMPTOM=100*(PSG1+PSG2+PSG3+PSG4+PSG5+PSG6+PSG7+PSG8)/662.5)
  #-------------------------------------------------#

  #-------------------------------------------------#
  #*ACTIVITY:
  dt<-dt%>%mutate(ACTIVIT=100*(PSG111+
                                 PSG112+
                                 PSG113+
                                 PSG114+
                                 PSG115+
                                 PSG116+
                                 PSG117+
                                 PSG151+
                                 PSG152+
                                 PSG153+
                                 PSG154+
                                 PSG155+
                                 PSG156+
                                 PSG157+
                                 PSG158+
                                 PSG159)/(1209.1
                                          -R111
                                          -R112
                                          -R113
                                          -R114
                                          -R115
                                          -R116
                                          -R117
                                          -R151
                                          -R152
                                          -R153
                                          -R154
                                          -R155
                                          -R156
                                          -R157
                                          -R158
                                          -R159))
  #-------------------------------------------------#

  #-------------------------------------------------#
  #IMPACTS:
  dt<-dt%>%mutate(IMPACTS=100*(PSG9+
                                 PSG10+
                                 PSG121+
                                 PSG122+
                                 PSG123+
                                 PSG124+
                                 PSG125+
                                 PSG126+
                                 PSG131+
                                 PSG132+
                                 PSG133+
                                 PSG134+
                                 PSG135+
                                 PSG136+
                                 PSG137+
                                 PSG138+
                                 PSG141+
                                 PSG142+
                                 PSG143+
                                 PSG144+
                                 PSG161+
                                 PSG162+
                                 PSG163+
                                 PSG164+
                                 PSG165+
                                 PSG17)/(2117.8
                                         -R121
                                         -R122
                                         -R123
                                         -R124
                                         -R125
                                         -R126
                                         -R131
                                         -R132
                                         -R133
                                         -R134
                                         -R135
                                         -R136
                                         -R137
                                         -R138
                                         -R141
                                         -R142
                                         -R143
                                         -R144
                                         -R161
                                         -R162
                                         -R163
                                         -R164
                                         -R165))
  #-------------------------------------------------#

  #-------------------------------------------------#
  #TOTALSG:
  dt<-dt%>%mutate(TOTALSG=100*(PSG1+PSG2+PSG3+PSG4+PSG5+PSG6+PSG7+PSG8+PSG9+PSG10
                               +PSG111+PSG112+PSG113+PSG114+PSG115+PSG116+PSG117
                               +PSG121+PSG122+PSG123+PSG124+PSG125+PSG126
                               +PSG131+PSG132+PSG133+PSG134+PSG135+PSG136+PSG137+PSG138
                               +PSG141+PSG142+PSG143+PSG144+PSG151+PSG152+PSG153+PSG154+PSG155
                               +PSG156+PSG157+PSG158+PSG159
                               +PSG161+PSG162+PSG163+PSG164+PSG165+PSG17)
                  /(3989.4-R111-R112-R113-R114-R115-R116-R117
                    -R121-R122-R123-R124-R125-R126
                    -R131-R132-R133-R134-R135-R136-R137-R138 -R141-R142-R143-R144
                    -R151-R152-R153-R154-R155-R156-R157-R158-R159
                    -R161-R162-R163-R164-R165))
  #-------------------------------------------------#

  #Print summary correlations and add new variables!
  #-------------------------------------------------#
  dt<-dt%>%select(SYMPTOM,ACTIVIT,IMPACTS,TOTALSG)
  print(summary(dt))
  cor(dt, method = c("pearson", "kendall", "spearman")) %>% print()
  #-------------------------------------------------#
  dades_copia %>% bind_cols(dt)
  #-------------------------------------------------#

}







