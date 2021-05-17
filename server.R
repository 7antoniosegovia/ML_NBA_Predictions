


server <- function(input, output) {

    set.seed(77)

    #season2021 <- game_logs(seasons = 2021, result_types = "Team")

       season2021v2 <- dplyr::select(season2021, -c(hasVideo, urlTeamSeasonLogo))

       pp <- season2021v2 %>% dplyr::arrange(idGame, locationGame)

       df_wide <- data.frame()

       for(i in seq (1, nrow(pp), 2)){

           fila <- cbind(pp[i,], pp[i+1,7:45])
           df_wide <- rbind(df_wide, fila)

       }


       colnames(df_wide)[1:6] <- colnames(pp)[1:6]

       nombrescols <- colnames(pp)[7:45]

       for(i in 1:39){
           colnames(df_wide)[i+6] <- paste0(nombrescols[i], "_A")
           colnames(df_wide)[i+45] <- paste0(nombrescols[i], "_H")
       }

       df_wide$result <- ifelse(df_wide$outcomeGame_A == "L", "H", "A")

       df_wide <- dplyr::select(df_wide, -c(locationGame_A, locationGame_H, slugMatchup_A, slugMatchup_H, slugOpponent_A, slugOpponent_H, slugTeamWinner_A, slugTeamWinner_H, slugTeamLoser_A, slugTeamLoser_H, outcomeGame_A, outcomeGame_H, isWin_A, isWin_H))

       df_wide <- df_wide %>% dplyr::select(1:6, nameTeam_A, slugTeam_A, idTeam_A, nameTeam_H, slugTeam_H, idTeam_H, ptsTeam_A, ptsTeam_H, result, everything())


       df_wide$isB2B_A <- ifelse(df_wide$isB2B_A == TRUE, 1, 0)
       df_wide$isB2B_H <- ifelse(df_wide$isB2B_H == TRUE, 1, 0)

       df_wide$isB2BFirst_A <- ifelse(df_wide$isB2BFirst_A == TRUE, 1, 0)
       df_wide$isB2BFirst_H <- ifelse(df_wide$isB2BFirst_H == TRUE, 1, 0)

       df_wide$isB2BSecond_A <- ifelse(df_wide$isB2BSecond_A == TRUE, 1, 0)
       df_wide$isB2BSecond_H <- ifelse(df_wide$isB2BSecond_H == TRUE, 1, 0)

       df_wide$H_Win <- ifelse(df_wide$result == "H", 1, 0)

       df_wide <- dplyr::arrange(df_wide, dateGame)

       ELO_before_A <- numeric(nrow(df_wide))
       ELO_before_H <- numeric(nrow(df_wide))
       ELO_after_A <- numeric(nrow(df_wide))
       ELO_after_H <- numeric(nrow(df_wide))

       for (i in 1:nrow(df_wide)) {

           #primer caso, es el primer partido que contabilizamos para el equipo de casa y el de fuera
           if(df_wide$numberGameTeamSeason_A[i] == 1 & df_wide$numberGameTeamSeason_H[i] == 1){

               #ELO_t+1 = k * (Steam - Eteam + ELO_t)

             ELO_before_A[i] <- 1500
             ELO_before_H[i] <- 1500

               Eteam <- 1/2
               k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/7.5

               if(df_wide$H_Win[i] == 1){

                   Steam_H <- 1
                   Steam_A <- 0

               } else if(df_wide$H_Win[i] == 0){

                   Steam_H <- 0
                   Steam_A <- 1

               }

               ELO_after_H[i] <- k*(Steam_H - Eteam) + 1500
               ELO_after_A[i] <- k*(Steam_A - Eteam) + 1500
           }

           #primer partido para el de casa pero no para el de fuera
           else if(df_wide$numberGameTeamSeason_H[i] == 1 & df_wide$numberGameTeamSeason_A[i] != 1){

               pp <- df_wide %>% slice(1:i-1)

               A_idx_A <- max(which(pp$slugTeam_A == df_wide$slugTeam_A[i])) #Primero con away team, vemos cual es la obs de su ultimo partido

               H_idx_A <- max(which(pp$slugTeam_H == df_wide$slugTeam_A[i]))

               donde_A <- ifelse(A_idx_A > H_idx_A, "A", "H")

               #Esto significaría que el último partido del equipo visitante fue EN CASA
               if(donde_A == "H"){

                   ELO_A_prev <- ELO_after_H[H_idx_A]
               #Esto significaría que el último partido del equipo visitante fue FUERA
               } else if(donde_A == "A"){

                   ELO_A_prev <- ELO_after_A[A_idx_A]

               }

               #ELO_t+1 = k * (Steam - Eteam + ELO_t)

               ELO_before_A[i] <- ELO_A_prev
               ELO_before_H[i] <- 1500

               Eteam_H <- 1/(1+10^((ELO_A_prev-1500)/400)) #Recuerda, su primer partido
               Eteam_A <- 1/(1+10^((1500-ELO_A_prev)/400)) #NO es su primer partido

               if(df_wide$H_Win[i] == 1){

                   Steam_H <- 1
                   Steam_A <- 0

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(1500-ELO_A_prev))

               } else if(df_wide$H_Win[i] == 0){

                   Steam_H <- 0
                   Steam_A <- 1

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(ELO_A_prev-1500))
               }

               ELO_after_H[i] <- k*(Steam_H - Eteam_H) + 1500
               ELO_after_A[i] <- k*(Steam_A - Eteam_A) + ELO_A_prev

           }

           #primer partido para el de fuera pero no para el de casa
           else if(df_wide$numberGameTeamSeason_A[i] == 1 & df_wide$numberGameTeamSeason_H[i] != 1){

               pp <- df_wide %>% slice(1:i-1)

               A_idx_H <- max(which(pp$slugTeam_A == df_wide$slugTeam_H[i])) #home team, vemos cual es la obs de su ultimo partido

               H_idx_H <- max(which(pp$slugTeam_H == df_wide$slugTeam_H[i]))

               donde_H <- ifelse(A_idx_H > H_idx_H, "A", "H")

               #Esto significaría que el último partido del equipo local fue EN CASA
               if(donde_H == "H"){

                   ELO_H_prev <- ELO_after_H[H_idx_H]
               #Esto significaría que el último partido del equipo local fue FUERA
               } else if(donde_H == "A"){

                   ELO_H_prev <- ELO_after_A[A_idx_H]

               }

               #ELO_t+1 = k * (Steam - Eteam + ELO_t)
               ELO_before_A[i] <- 1500
               ELO_before_H[i] <- ELO_H_prev

               Eteam_H <- 1/(1+10^((1500-ELO_H_prev)/400)) #Recuerda, NO es su primer partido
               Eteam_A <- 1/(1+10^((ELO_H_prev-1500)/400)) #su primer partido

               if(df_wide$H_Win[i] == 1){

                   Steam_H <- 1
                   Steam_A <- 0

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(ELO_H_prev-1500))

               } else if(df_wide$H_Win[i] == 0){

                   Steam_H <- 0
                   Steam_A <- 1

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(1500-ELO_H_prev))
               }

               ELO_after_H[i] <- k*(Steam_H - Eteam_H) + ELO_H_prev
               ELO_after_A[i] <- k*(Steam_A - Eteam_A) + 1500


           #No es el primer partido de ninguno de los dos
           } else if(df_wide$numberGameTeamSeason_A[i] != 1 & df_wide$numberGameTeamSeason_H[i] != 1){

               pp <- df_wide %>% slice(1:i-1)

               A_idx_A <- max(which(pp$slugTeam_A == df_wide$slugTeam_A[i])) #Primero con away team, vemos cual es la obs de su ultimo partido

               H_idx_A <- max(which(pp$slugTeam_H == df_wide$slugTeam_A[i]))

               donde_A <- ifelse(A_idx_A > H_idx_A, "A", "H")

               A_idx_H <- max(which(pp$slugTeam_A == df_wide$slugTeam_H[i])) #Home team, vemos cual es la obs de su ultimo partido

               H_idx_H <- max(which(pp$slugTeam_H == df_wide$slugTeam_H[i]))

               donde_H <- ifelse(A_idx_H > H_idx_H, "A", "H")


               #Contemplamos todos los posibles escenarios pasados. Primero, que ambos jugasen el último partido fuera
               if(donde_A == "A" & donde_H == "A"){

                   ELO_A_prev <- ELO_after_A[A_idx_A]
                   ELO_H_prev <- ELO_after_A[A_idx_H]

               }

               #Visitante jugó su último partido fuera y local jugó su último partido en casa
               else if(donde_A == "A" & donde_H == "H"){

                   ELO_A_prev <- ELO_after_A[A_idx_A]
                   ELO_H_prev <- ELO_after_H[H_idx_H]

               }

               else if(donde_A == "H" & donde_H == "A"){

                   ELO_A_prev <- ELO_after_H[H_idx_A]
                   ELO_H_prev <- ELO_after_A[A_idx_H]

               }

               else if(donde_A == "H" & donde_H == "H"){

                   ELO_A_prev <- ELO_after_H[H_idx_A]
                   ELO_H_prev <- ELO_after_H[H_idx_H]

               }

               #ELO_t+1 = k * (Steam - Eteam + ELO_t)

               ELO_before_A[i] <- ELO_A_prev
               ELO_before_H[i] <- ELO_H_prev

               Eteam_H <- 1/(1+10^((ELO_A_prev-ELO_H_prev)/400)) #Recuerda, NO es su primer partido
               Eteam_A <- 1/(1+10^((ELO_H_prev-ELO_A_prev)/400)) #TAMPOCO ES su primer partido

               if(df_wide$H_Win[i] == 1){

                   Steam_H <- 1
                   Steam_A <- 0

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(ELO_H_prev-ELO_A_prev))

               } else if(df_wide$H_Win[i] == 0){

                   Steam_H <- 0
                   Steam_A <- 1

                   k <- 20*((abs(df_wide$plusminusTeam_A[i]) + 3)^0.8)/(7.5 + 0.006*(ELO_A_prev-ELO_H_prev))
               }

               ELO_after_H[i] <- k*(Steam_H - Eteam_H) + ELO_H_prev
               ELO_after_A[i] <- k*(Steam_A - Eteam_A) + ELO_A_prev

           }

       }

       df_wide$ELO_before_A <- ELO_before_A
       df_wide$ELO_before_H <- ELO_before_H

       df_wide$ELO_after_A <- ELO_after_A
       df_wide$ELO_after_H <- ELO_after_H

       df_wide <- df_wide %>% dplyr::select(1:15, ELO_before_A, ELO_after_A, ELO_before_H, ELO_after_H, everything())

       df_wide$countDaysNextGameTeam_A[is.na(df_wide$countDaysNextGameTeam_A)] <- 0
       df_wide$countDaysNextGameTeam_H[is.na(df_wide$countDaysNextGameTeam_H)] <- 0


    df_date <- reactive({dplyr::filter(df_wide, dateGame <= input$date)})

    season_2021_filt <- reactive({dplyr::filter(season2021, dateGame < input$date)})


    daily_schedule <- reactive({dplyr::filter(schedule, Date == input$date)})

    daily_X_test <- reactive({

        schedule <- dplyr::filter(schedule, Date == input$date)

        X_test <- as.data.frame(matrix(nrow = nrow(schedule), ncol = 48))
        colnames(X_test) <- colnamesxtest


        for(i in 1:nrow(schedule)){

            dfA <- dplyr::filter(season_2021_filt(), nameTeam == schedule$Away[i])
            avg_until_A <- team_averages_last_n_games_df(dfA, 10)
            dfH <- dplyr::filter(season_2021_filt(), nameTeam == schedule$Home[i])
            avg_until_H <- team_averages_last_n_games_df(dfH, 10)


            #Con que obs del df de medias se corresponden
            idx_A <- nrow(avg_until_A)
            idx_H <- nrow(avg_until_H)

            #editamos el df
            X_test[i, 4:24] <- avg_until_A[idx_A, 10:30]
            X_test[i, 3] <- avg_until_A[idx_A, 31]
            X_test[i, 25] <- avg_until_A[idx_A, 32]

            pp_A <- dplyr::filter(df_date(), nameTeam_A == schedule$Away[i] | nameTeam_H == schedule$Away[i])

            A_idx_A <- max(which(pp_A$nameTeam_A == schedule$Away[i])) #Primero con away team, vemos cual es la obs de su ultimo partido

            H_idx_A <- max(which(pp_A$nameTeam_H == schedule$Away[i]))

            donde_A <- ifelse(A_idx_A > H_idx_A, "A", "H")

            #Esto significaría que el último partido del equipo visitante fue EN CASA
            if(donde_A == "H"){

                X_test[i, 1] <- pp_A$ELO_after_H[H_idx_A]
            #Esto significaría que el último partido del equipo visitante fue FUERA
            } else if(donde_A == "A"){

                X_test[i, 1] <- pp_A$ELO_after_A[A_idx_A]

            }


            X_test[i, 27:47] <- avg_until_H[idx_H, 10:30]
            X_test[i, 26] <- avg_until_H[idx_H, 31]
            X_test[i, 48] <- avg_until_H[idx_H, 32]

            pp_H <- dplyr::filter(df_date(), nameTeam_A == schedule$Home[i] | nameTeam_H == schedule$Home[i])

            A_idx_H <- max(which(pp_H$nameTeam_A == schedule$Home[i]))

            H_idx_H <- max(which(pp_H$nameTeam_H == schedule$Home[i]))

            donde_H <- ifelse(A_idx_H > H_idx_H, "A", "H")

            #Esto significaría que el último partido del equipo visitante fue EN CASA
            if(donde_H == "H"){

                X_test[i, 2] <- pp_H$ELO_after_H[H_idx_H]
            #Esto significaría que el último partido del equipo visitante fue FUERA
            } else if(donde_H == "A"){

                X_test[i, 2] <- pp_H$ELO_after_A[A_idx_H]

            }

        }

        return(X_test)

    })

    daily_preds_LR <- reactive({

        load("lr_experimento.RData")

        predsLR_prob <- predict.train(lr, newdata = daily_X_test(), type = "prob")

        colnames(predsLR_prob) <- c("Away", "Home")

        return(predsLR_prob)
    })


    daily_preds_SVM <- reactive({

        load("svm_experimento.RData")

        predsLR_svm <- predict.train(svm, newdata = daily_X_test(), type = "raw")

        return(predsLR_svm)
    })

    output$game1 <- renderUI({

        box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[1])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[1])])), width = 6)

    })

    output$game2 <- renderUI({

        if(nrow(daily_schedule()) >= 2){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[2])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[2])])), width = 6)
        }

        else{NULL}

    })

    output$game3 <- renderUI({

        if(nrow(daily_schedule()) >= 3){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[3])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[3])])), width = 6)

        }

        else{NULL}

    })

    output$game4 <- renderUI({

        if(nrow(daily_schedule()) >= 4){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[4])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[4])])), width = 6)

        }

        else{NULL}

    })

    output$game5 <- renderUI({

        if(nrow(daily_schedule()) >= 5){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[5])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[5])])), width = 6)

        }

        else{NULL}

    })

    output$game6 <- renderUI({

        if(nrow(daily_schedule()) >= 6){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[6])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[6])])), width = 6)

        }

        else{NULL}

    })

    output$game7 <- renderUI({

        if(nrow(daily_schedule()) >= 7){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[7])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[7])])), width = 6)

        }

        else{NULL}

    })

    output$game8 <- renderUI({

        if(nrow(daily_schedule()) >= 8){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[8])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[8])])), width = 6)

        }

        else{NULL}

    })

    output$game9 <- renderUI({

        if(nrow(daily_schedule()) >= 9){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[9])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[9])])), width = 6)

        }

        else{NULL}

    })

    output$game10 <- renderUI({

        if(nrow(daily_schedule()) >= 10){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[10])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[10])])), width = 6)

        }

        else{NULL}

    })

    output$game11 <- renderUI({

        if(nrow(daily_schedule()) >= 11){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[11])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[11])])), width = 6)

        }

        else{NULL}

    })

    output$game12 <- renderUI({

        if(nrow(daily_schedule()) >= 12){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[12])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[12])])), width = 6)

        }

        else{NULL}

    })

    output$game13 <- renderUI({

        if(nrow(daily_schedule()) >= 13){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[13])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[13])])), width = 6)

        }

        else{NULL}

    })

    output$game14 <- renderUI({

        if(nrow(daily_schedule()) >= 14){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[14])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[14])])), width = 6)

        }

        else{NULL}

    })

    output$game15 <- renderUI({

        if(nrow(daily_schedule()) >= 15){

            box(column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[15])])),
            column(2, h1("@")),
            column(5, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[15])])), width = 6)

        }

        else{NULL}

    })

    output$textfardate <- renderUI({

        if(input$date > Sys.Date()){

            HTML("<p><span style='color: rgb(226, 80, 65); font-size: 20px;'>Warning! Predictions are not going to be as accurate, as we only take into account the games played until today. For more accurate predictions, select today&apos;s games or come back in the future.</span></p>")
        } else {NULL}

    })


    output$textfardate2 <- renderUI({

        if(input$date > Sys.Date()){

            HTML("<p><span style='color: rgb(226, 80, 65); font-size: 20px;'>Warning! Predictions are not going to be as accurate, as we only take into account the games played until today. For more accurate predictions, select today&apos;s games or come back in the future.</span></p>")
        } else {NULL}

    })

    output$game1predLR <- renderUI({

        if(input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[1])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[1])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[1])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[1])*100, 2), "%")))), width = 6)

        } else{

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[1] & nameTeam_H == daily_schedule()$Home[1])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[1])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[1])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[1])]),
                   h3(paste("Win probability:", paste0(round(daily_preds_LR()$Home[1]*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)

        }


    })

    output$game2predLR <- renderUI({

        if(nrow(daily_schedule()) >= 2 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[2])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[2])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[2])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[2])*100, 2), "%")))), width = 6)
        }

        else if(nrow(daily_schedule()) >= 2 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[2] & nameTeam_H == daily_schedule()$Home[2])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[2])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[2])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[2])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[2])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else{NULL}

    })

    output$game3predLR <- renderUI({

        if(nrow(daily_schedule()) >= 3 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[3])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[3])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[3])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[3])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 3 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[3] & nameTeam_H == daily_schedule()$Home[3])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[3])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[3])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[3])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[3])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)

        } else {NULL}

    })

    output$game4predLR <- renderUI({

        if(nrow(daily_schedule()) >= 4 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[4])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[4])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[4])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[4])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 4 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[4] & nameTeam_H == daily_schedule()$Home[4])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[4])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[4])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[4])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[4])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game5predLR <- renderUI({

        if(nrow(daily_schedule()) >= 5 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[5])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[5])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[5])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[5])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 5 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[5] & nameTeam_H == daily_schedule()$Home[5])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[5])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[5])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[5])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[5])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game6predLR <- renderUI({

        if(nrow(daily_schedule()) >= 6 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[6])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[6])*100, 2), "%")))),
            column(2, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[6])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[6])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 6 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[6] & nameTeam_H == daily_schedule()$Home[6])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[6])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[6])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[6])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[6])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game7predLR <- renderUI({

        if(nrow(daily_schedule()) >= 7 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[7])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[7])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[7])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[7])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 7 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[7] & nameTeam_H == daily_schedule()$Home[7])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[7])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[7])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[7])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[7])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game8predLR <- renderUI({

        if(nrow(daily_schedule()) >= 8 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[8])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[8])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[8])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[8])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 8 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[8] & nameTeam_H == daily_schedule()$Home[8])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[8])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[8])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[8])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[8])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else{NULL}

    })

    output$game9predLR <- renderUI({

        if(nrow(daily_schedule()) >= 9 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[9])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[9])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[9])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[9])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 9 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[9] & nameTeam_H == daily_schedule()$Home[9])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[9])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[9])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[9])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[9])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game10predLR <- renderUI({

        if(nrow(daily_schedule()) >= 10 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[10])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[10])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[10])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[10])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 10 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[10] & nameTeam_H == daily_schedule()$Home[10])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[10])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[10])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[10])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[10])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else{NULL}

    })

    output$game11predLR <- renderUI({

        if(nrow(daily_schedule()) >= 11 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[11])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[11])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[11])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[11])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 11 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[11] & nameTeam_H == daily_schedule()$Home[11])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[11])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[11])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[11])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[11])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game12predLR <- renderUI({

        if(nrow(daily_schedule()) >= 12 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[12])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[12])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[12])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[12])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 12 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[12] & nameTeam_H == daily_schedule()$Home[12])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[12])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[12])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[12])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[12])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game13predLR <- renderUI({

        if(nrow(daily_schedule()) >= 13 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[13])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[13])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[13])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[13])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 13 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[13] & nameTeam_H == daily_schedule()$Home[13])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[13])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[13])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[13])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[13])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game14predLR <- renderUI({

        if(nrow(daily_schedule()) >= 14 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[14])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[14])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[14])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[14])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 14 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[14] & nameTeam_H == daily_schedule()$Home[14])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[14])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[14])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[14])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[14])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game15predLR <- renderUI({

        if(nrow(daily_schedule()) >= 15 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[15])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[15])*100, 2), "%")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[15])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[15])*100, 2), "%")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 15 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[15] & nameTeam_H == daily_schedule()$Home[15])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[15])]),
                   h3(paste("Win probability:", paste0(round((1-daily_preds_LR()$Home[15])*100, 2), "%"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
            column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[15])]),
                   h3(paste("Win probability:", paste0(round((daily_preds_LR()$Home[15])*100, 2), "%"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })


    output$game1predSVM <- renderUI({

        if(input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[1])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[1] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[1])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[1] == 1, "Win", "Loss")))), width = 6)

        } else{

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[1] & nameTeam_H == daily_schedule()$Home[1])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[1])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[1] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[1])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[1] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)

        }



    })

    output$game2predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 2 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[2])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[2] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[2])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[2] == 1, "Win", "Loss")))), width = 6)
        }

        else if(nrow(daily_schedule()) >= 2 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[2] & nameTeam_H == daily_schedule()$Home[2])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[2])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[2] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[2])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[2] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game3predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 3 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[3])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[3] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[3])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[3] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 3 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[3] & nameTeam_H == daily_schedule()$Home[3])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[3])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[3] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[3])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[3] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game4predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 4 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[4])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[4] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[4])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[4] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 4 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[4] & nameTeam_H == daily_schedule()$Home[4])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[4])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[4] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[4])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[4] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game5predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 5 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[5])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[5] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[5])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[5] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 5 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[5] & nameTeam_H == daily_schedule()$Home[5])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[5])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[5] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[5])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[5] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game6predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 6 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[6])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[6] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[6])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[6] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 6 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[6] & nameTeam_H == daily_schedule()$Home[6])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[6])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[6] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[6])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[6] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game7predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 7 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[7])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[7] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[7])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[7] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 7 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[7] & nameTeam_H == daily_schedule()$Home[7])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[7])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[7] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[7])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[7] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game8predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 8 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[8])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[8] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[8])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[8] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 8 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[8] & nameTeam_H == daily_schedule()$Home[8])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[8])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[8] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[8])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[8] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game9predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 9 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[9])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[9] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[9])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[9] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 9 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[9] & nameTeam_H == daily_schedule()$Home[9])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[9])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[9] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[9])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[9] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game10predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 10 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[10])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[10] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[10])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[10] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 10 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[10] & nameTeam_H == daily_schedule()$Home[10])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[10])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[10] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[10])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[10] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game11predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 11 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[11])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[11] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[11])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[11] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 11 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[11] & nameTeam_H == daily_schedule()$Home[11])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[11])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[11] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[11])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[11] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game12predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 12 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[12])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[12] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[12])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[12] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 12 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[12] & nameTeam_H == daily_schedule()$Home[12])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[12])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[12] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[12])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[12] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game13predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 13 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[13])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[13] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[13])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[13] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 13 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[13] & nameTeam_H == daily_schedule()$Home[13])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[13])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[13] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[13])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[13] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game14predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 14 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[14])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[14] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[14])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[14] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 14 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[14] & nameTeam_H == daily_schedule()$Home[14])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[14])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[14] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[14])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[14] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    output$game15predSVM <- renderUI({

        if(nrow(daily_schedule()) >= 15 & input$date >= Sys.Date()){

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[15])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[15] == 0, "Win", "Loss")))),
            column(2, offset = 1, h1("@")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[15])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[15] == 1, "Win", "Loss")))), width = 6)

        }

        else if(nrow(daily_schedule()) >= 15 & input$date < Sys.Date()){

            pp <- dplyr::filter(df_date(), dateGame == input$date & nameTeam_A == daily_schedule()$Away[15] & nameTeam_H == daily_schedule()$Home[15])

            box(column(4, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Away[15])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[15] == 0, "Win", "Loss"))), br(),
                   h2(paste("Result:  ", pp$ptsTeam_A))),
           column(2, offset = 1, h1("@"), br(), br(), br(), br(), br(), br(), br(), br(), h3("-")),
            column(4, offset = 1, tags$img(src = df_logos$Logo[which(df_logos$Name == daily_schedule()$Home[15])]),
                   h3(paste("Prediction: ", ifelse(daily_preds_SVM()[15] == 1, "Win", "Loss"))), br(),
                   h2(paste(pp$ptsTeam_H))), width = 6)
        } else {NULL}

    })

    df_plots <- as.data.frame(cbind(1:nrow(res_odds), res_odds$totalLR, res_odds$totalSVM))

    colnames(df_plots) <- c("Game", "Earnings_Log_Regr", "Earnings_SVM")

    output$tableodds <- renderDataTable(dplyr::select(res_odds, -c(4,5)))

    output$plot1 <- renderPlotly({

        ggplotly(ggplot(df_plots, aes(Game, Earnings_Log_Regr)) + geom_line() + geom_hline(yintercept = 10000, linetype = "dashed", color = "red") +
                     labs(y = "Earnings", title = "Earnings with the logistic regression model"))
    })

    output$plot2 <- renderPlotly({

        ggplotly(ggplot(df_plots, aes(Game, Earnings_SVM)) + geom_line(color = "blue") + geom_hline(yintercept = 10000, linetype = "dashed", color = "red") +
                     labs(y = "Earnings", title = "Earnings with the support vector machine"))
    })

    output$vb1 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$predsLR == res_odds$result)/nrow(res_odds)*100, 2), "%"),
                subtitle = "Accuracy of Log. Reg. model", color = "blue", width = 12)

    })

    output$vb2 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$predsSVM == res_odds$result)/nrow(res_odds)*100, 2), "%"),
                subtitle = "Accuracy of SVM", color = "blue", width = 12)

    })

    output$vboxresults1 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$predsLR == res_odds$result)/nrow(res_odds)*100, 2), "%"),
                subtitle = "Log. Reg. - Accuracy", color = "blue", width = 12)

    })


    output$vboxresults2 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$predsSVM == res_odds$result)/nrow(res_odds)*100, 2), "%"),
                subtitle = "SVM - Accuracy", color = "blue", width = 12)

    })

    output$vboxresults3 <- renderValueBox({

        valueBox(paste0(res_odds$totalLR[nrow(res_odds)] - 10000, " Points"),
                subtitle = "Log. Reg. - Net earnings", color = "green", width = 12)

    })

    output$vboxresults4 <- renderValueBox({

        valueBox(paste0(res_odds$totalSVM[nrow(res_odds)] - 10000, " Points"),
                subtitle = "SVM - Net earnings", color = "green", width = 12)

    })

    res_odds$aciertoLR <- ifelse(res_odds$predsLR == res_odds$result, 1, 0)
    res_odds$aciertoSVM <- ifelse(res_odds$predsSVM == res_odds$result, 1, 0)

    # Intentando calcular upset ratio

    # Si cuota < 1.9, se ha apostado a favorito

    res_odds$favoritoLR <- ifelse(res_odds$oddsLR < 1.9, "fav", "no")

    # Si se ha apostado al favorito y se ha acertado, ha ganado el favorito
    # o si se ha apostado al no favorito y no se ha acertado

    res_odds$gana_favorito <- case_when(
        res_odds$favoritoLR == "fav" & res_odds$aciertoLR == 1 ~ "si",
        res_odds$favoritoLR == "fav" & res_odds$aciertoLR == 0 ~ "no",
        res_odds$favoritoLR == "no" & res_odds$aciertoLR == 1 ~ "no",
        res_odds$favoritoLR == "no" & res_odds$aciertoLR == 0 ~ "si",
    )

    output$vboxresults5 <- renderValueBox({

        valueBox(paste0(round((1-sum(res_odds$gana_favorito == "si")/nrow(res_odds))*100,2), "%"),
                subtitle = "Upset Ratio (Underdog victories)", color = "black", width = 12)

    })

    output$vboxresults6 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$predsLR != res_odds$predsSVM)/nrow(res_odds)*100,2), "%"),
                subtitle = "Discrepancy Ratio (Different predictions)", color = "black", width = 12)

    })

    acierto_diario <- res_odds %>% group_by(Date) %>% summarise(prop_LR = round(sum(aciertoLR)/n(),4),
                                          prop_SVM = round(sum(aciertoSVM)/n(),4),
                                          earnings_LR = sum(winLR),
                                          earnings_SVM = round(sum(winSVM),1))

    output$dailyaccslr <- renderPlotly({

        ggplotly(ggplot(acierto_diario, aes(Date, prop_LR)) +
                geom_bar(stat = "identity", fill = case_when(acierto_diario$prop_LR <= 0.5 ~ "#c41414",
                                                             acierto_diario$prop_LR > 0.5 &
                                                                 acierto_diario$prop_LR < 0.75 ~ "#f48600",
                                                             acierto_diario$prop_LR >= 0.75 ~ "#00c41d"),
                         color = case_when(acierto_diario$prop_LR <= 0.5 ~ "#c41414",
                                                             acierto_diario$prop_LR > 0.5 &
                                                                 acierto_diario$prop_LR < 0.75 ~ "#f48600",
                                                             acierto_diario$prop_LR >= 0.75 ~ "#00c41d"),
                         alpha = 0.6, width = 0.7) + theme_light() +
                labs(y = "Accuracy - LR", title = "Daily accuracies, Logistic regression"))

    })

    output$dailyvboxLR1 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$prop_LR == min(acierto_diario$prop_LR))],
                subtitle = "Worst day (Accuracy, LR)", color = "red", width = 6)

    })

    output$dailyvboxLR2 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$prop_LR == max(acierto_diario$prop_LR))],
                subtitle = "Best day (Accuracy, LR)", color = "green", width = 6)

    })

    output$dailyearningslr <- renderPlotly({

        ggplotly(ggplot(acierto_diario, aes(Date, earnings_LR)) +
        geom_bar(stat = "identity", fill = case_when(acierto_diario$prop_LR <= 0.5 ~ "#c41414",
                                                     acierto_diario$prop_LR > 0.5 &
                                                         acierto_diario$prop_LR < 0.75 ~ "#f48600",
                                                     acierto_diario$prop_LR >= 0.75 ~ "#00c41d"),
                 color = case_when(acierto_diario$prop_LR <= 0.5 ~ "#c41414",
                                                     acierto_diario$prop_LR > 0.5 &
                                                         acierto_diario$prop_LR < 0.75 ~ "#f48600",
                                                     acierto_diario$prop_LR >= 0.75 ~ "#00c41d"),
                 alpha = 0.6, width = 0.7) + theme_light() +
        labs(y = "Earnings - LR", title = "Daily earnings, LR (colors depend on daily accuracy)"))

    })


    output$dailyvboxLR3 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$earnings_LR == min(acierto_diario$earnings_LR))],
                subtitle = "Worst day (Earnings, LR)", color = "red", width = 6)

    })

    output$dailyvboxLR4 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$earnings_LR == max(acierto_diario$earnings_LR))],
                subtitle = "Best day (Earnings, LR)", color = "green", width = 6)

    })


    output$dailyaccssvm <- renderPlotly({

        ggplotly(ggplot(acierto_diario, aes(Date, prop_SVM)) +
                geom_bar(stat = "identity", fill = case_when(acierto_diario$prop_SVM <= 0.5 ~ "#c41414",
                                                             acierto_diario$prop_SVM > 0.5 &
                                                                 acierto_diario$prop_SVM < 0.75 ~ "#f48600",
                                                             acierto_diario$prop_SVM >= 0.75 ~ "#00c41d"),
                         color = case_when(acierto_diario$prop_SVM <= 0.5 ~ "#c41414",
                                                             acierto_diario$prop_SVM > 0.5 &
                                                                 acierto_diario$prop_SVM < 0.75 ~ "#f48600",
                                                             acierto_diario$prop_SVM >= 0.75 ~ "#00c41d"),
                         alpha = 0.6, width = 0.7) + theme_light() +
                labs(y = "Accuracy - SVM", title = "Daily accuracies, SVM"))

    })

    output$dailyvboxSVM1 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$prop_SVM == min(acierto_diario$prop_SVM))],
                subtitle = "Worst day (Accuracy, SVM)", color = "red", width = 6)

    })

    output$dailyvboxSVM2 <- renderValueBox({

        valueBox("3 times",
                subtitle = "Best days (Accuracy, LR)", color = "green", width = 6)

    })

    output$dailyearningssvm <- renderPlotly({

        ggplotly(ggplot(acierto_diario, aes(Date, earnings_SVM)) +
        geom_bar(stat = "identity", fill = case_when(acierto_diario$prop_SVM <= 0.5 ~ "#c41414",
                                                     acierto_diario$prop_SVM > 0.5 &
                                                         acierto_diario$prop_SVM < 0.75 ~ "#f48600",
                                                     acierto_diario$prop_SVM >= 0.75 ~ "#00c41d"),
                 color = case_when(acierto_diario$prop_SVM <= 0.5 ~ "#c41414",
                                                     acierto_diario$prop_SVM > 0.5 &
                                                         acierto_diario$prop_SVM < 0.75 ~ "#f48600",
                                                     acierto_diario$prop_SVM >= 0.75 ~ "#00c41d"),
                 alpha = 0.6, width = 0.7) + theme_light() +
        labs(y = "Earnings - SVM", title = "Daily earnings, SVM (colors depend on daily accuracy)"))

    })


    output$dailyvboxSVM3 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$earnings_SVM == min(acierto_diario$earnings_SVM))],
                subtitle = "Worst day (Earnings, SVM)", color = "red", width = 6)

    })

    output$dailyvboxSVM4 <- renderValueBox({

        valueBox(acierto_diario$Date[which(acierto_diario$earnings_SVM == max(acierto_diario$earnings_SVM))],
                subtitle = "Best day (Earnings, SVM)", color = "green", width = 6)

    })

    output$betstrat1 <- renderPlotly({

        ggplotly(res_odds %>% ggplot(aes(amountLR)) +
    geom_bar(color = "#d90000", fill = "#d90000",
             alpha = 0.4, width = 75) +
    labs(title = "Frequency of betting amount", x = "Amount", y = "Frequency") +
    theme_light())

    })

    strat_bet <- data.frame(amount = c(100,200,300), prop = c(0.551,0.726,0.816))

    output$betstrat2 <- renderPlotly({

        ggplotly(ggplot(strat_bet, aes(amount, prop)) +
    geom_bar(stat = "identity", color = "#0e00ab", fill = "#0e00ab",
             alpha = 0.4, width = 75) +
    labs(title = "Accuracy by betting amount",
         x = "Amount", y = "Accuracy") + theme_light())

    })

    output$betvbox13 <- renderValueBox({

        valueBox(unlist(res_odds %>% dplyr::filter(aciertoLR == 1) %>% summarise(max(oddsLR))),
                subtitle = "Max. odds hit, Log. Reg.", color = "black", width = 12)

    })


    output$betvbox14 <- renderValueBox({

        valueBox(unlist(res_odds %>% dplyr::filter(aciertoSVM == 1) %>% summarise(max(oddsSVM))),
                subtitle = "Max. odds hit, SVM", color = "black", width = 12)

    })


    output$betvbox1 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsLR > 1.9), "(of 385)"),
                subtitle = "Underdog predictions, LR", color = "blue", width = 12)

    })


    output$betvbox2 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsSVM > 1.9), "(of 385)"),
                subtitle = "Underdog predictions, SVM", color = "blue", width = 12)

    })


    output$betvbox3 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsLR > 1.9 & res_odds$aciertoLR == 1)/sum(res_odds$oddsLR > 1.9)*100,2), "%"),
                subtitle = "Acc. underdog predictions, LR", color = "red", width = 12)

    })


    output$betvbox4 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsSVM > 1.9 & res_odds$aciertoSVM == 1)/sum(res_odds$oddsSVM > 1.9)*100,2), "%"),
                subtitle = "Acc. underdog predictions, SVM", color = "red", width = 12)

    })


    output$betvbox5 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsLR > 2.5), "(of 385)"),
                subtitle = "Predictions w/ odds > 2.5, LR", color = "blue", width = 12)

    })


    output$betvbox6 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsSVM > 2.5), "(of 385)"),
                subtitle = "Predictions w/ odds > 2.5, SVM", color = "blue", width = 12)

    })


    output$betvbox7 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsLR > 2.5 & res_odds$aciertoLR == 1)/sum(res_odds$oddsLR > 2.5)*100,2), "%"),
                subtitle = "Acc. > 2.5 predictions, LR", color = "red", width = 12)

    })


    output$betvbox8 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsSVM > 2.5 & res_odds$aciertoSVM == 1)/sum(res_odds$oddsSVM > 2.5)*100,2), "%"),
                subtitle = "Acc. > 2.5 predictions, SVM", color = "red", width = 12)

    })

    output$betvbox9 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsLR > 3), "(of 385)"),
                subtitle = "Predictions w/ odds > 3, LR", color = "blue", width = 12)

    })


    output$betvbox10 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsSVM > 3), "(of 385)"),
                subtitle = "Predictions w/ odds > 3, SVM", color = "blue", width = 12)

    })


    output$betvbox11 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsLR > 3 & res_odds$aciertoLR == 1)/sum(res_odds$oddsLR > 3)*100,2), "%"),
                subtitle = "Acc. > 3 predictions, LR", color = "red", width = 12)

    })


    output$betvbox12 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsSVM > 3 & res_odds$aciertoSVM == 1)/sum(res_odds$oddsSVM > 3)*100,2), "%"),
                subtitle = "Acc. > 3 predictions, SVM", color = "red", width = 12)

    })

    output$betvbox15 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsLR > 4), "(of 385)"),
                subtitle = "Predictions w/ odds > 4, LR", color = "blue", width = 12)

    })


    output$betvbox16 <- renderValueBox({

        valueBox(paste(sum(res_odds$oddsSVM > 4), "(of 385)"),
                subtitle = "Predictions w/ odds > 4, SVM", color = "blue", width = 12)

    })


    output$betvbox17 <- renderValueBox({

        valueBox(paste0(round(sum(res_odds$oddsLR > 4 & res_odds$aciertoLR == 1)/sum(res_odds$oddsLR > 4)*100,2), "%"),
                subtitle = "Acc. > 4 predictions, LR", color = "red", width = 12)

    })


    output$betvbox18 <- renderValueBox({

        valueBox(paste0("-"),
                subtitle = "Acc. > 4 predictions, SVM", color = "red", width = 12)

    })

    prop_aciertos_LR <- NULL
    prop_aciertos_SVM <- NULL
    earnings_LR <- NULL
    earnings_SVM <- NULL

    for(i in 1:30){

        prop_aciertos_LR[i] <- unlist(filter(res_odds,
                                          Away == sort(unique(res_odds$Away))[i] |
               Home == sort(unique(res_odds$Away))[i]) %>%
        summarise(sum(aciertoLR == 1)/n()))

        prop_aciertos_SVM[i] <- unlist(filter(res_odds,
                                          Away == sort(unique(res_odds$Away))[i] |
               Home == sort(unique(res_odds$Away))[i]) %>%
        summarise(sum(aciertoSVM == 1)/n()))

        earnings_LR[i] <- unlist(filter(res_odds,
                                          Away == sort(unique(res_odds$Away))[i] |
               Home == sort(unique(res_odds$Away))[i]) %>%
        summarise(sum(winLR)/2))

        earnings_SVM[i] <- unlist(filter(res_odds,
                                          Away == sort(unique(res_odds$Away))[i] |
               Home == sort(unique(res_odds$Away))[i]) %>%
        summarise(sum(winSVM)/2))

    }

    equipos <- data.frame(Team = sort(unique(res_odds$Away)),
                          prop_LR = prop_aciertos_LR,
                          prop_SVM = prop_aciertos_SVM,
                          earnings_LR = earnings_LR,
                          earnings_SVM = earnings_SVM)


    output$teamsplot1 <- renderPlotly({

        ggplotly(ggplot(equipos, aes(reorder(Team, prop_LR), prop_LR)) +
        geom_bar(stat = "identity", color = "#0e00ab", fill = "#0e00ab", alpha = 0.6) +
        labs(title = "Better predicted teams, LR", x = "Team",
             y = "Proportion of correct predictions") +
        coord_flip() + theme_light())

    })

    output$teamsplot2 <- renderPlotly({

     ggplotly(ggplot(equipos, aes(reorder(Team, earnings_LR), earnings_LR)) +
        geom_bar(stat = "identity", color = ifelse(earnings_LR < 0, "#db0000", "#00ad14"), fill = ifelse(earnings_LR < 0, "#db0000", "#00ad14"), alpha = 0.6) +
        labs(title = "Earnings by team, LR", x = "Team",
             y = "Earnings") +
        coord_flip() + theme_light())


    })

    output$teamsplot3 <- renderPlotly({

        ggplotly(ggplot(equipos, aes(reorder(Team, prop_SVM), prop_SVM)) +
        geom_bar(stat = "identity", color = "#0e00ab", fill = "#0e00ab", alpha = 0.6) +
        labs(title = "Better predicted teams, SVM", x = "Team",
             y = "Proportion of correct predictions") +
        coord_flip() + theme_light())

    })

    output$teamsplot4 <- renderPlotly({

        ggplotly(ggplot(equipos, aes(reorder(Team, earnings_SVM), earnings_SVM)) +
        geom_bar(stat = "identity", color = ifelse(earnings_SVM < 0, "#db0000", "#00ad14"), fill = ifelse(earnings_SVM < 0, "#db0000", "#00ad14"), alpha = 0.6) +
        labs(title = "Earnings by team, SVM", x = "Team",
             y = "Earnings") +
        coord_flip() + theme_light())

    })







}
