;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; ABM - Covid19 ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [csv]
breed [humans human]
breed [statistic_agents statistic_agent]

globals [
  age_group_0_9     ;; Rango etario entre 0 y 9
  age_group_10_19   ;; Rango etario entre 10 y 19
  age_group_20_29   ;; Rango etario entre 20 y 29
  age_group_30_39   ;; Rango etario entre 30 y 39
  age_group_40_49   ;; Rango etario entre 40 y 49
  age_group_50_59   ;; Rango etario entre 50 y 59
  age_group_60_69   ;; Rango etario entre 60 y 69
  age_group_70_79   ;; Rango etario entre 70 y 79
  age_group_80      ;; Rango etario mayor a 80
  elapsed-day-hours ;; Horas transcurridas
  medical_care_used ;; Atención médica utilizada
  number_of_deaths  ;; Número de muertos
  death_list        ;; Lista de muertos
  city_area_patches   ;; Parches del área de la ciudad
  ;roads_area_patches   ;; Parches de área de la parcela
  cumulative_infected   ;; Infectados acumulados
  last_cumulative   ;; Ultimo acumulado
  cumulative_aware_of_infection   ;; Conocimiento de infección acumulativo
  last_cumulative_aware_of_infection   ;; Última acumulación acumulativa de infección
  logged_transmitters   ;; Transmisores registrados
  R0_global   ;; Número de reproducción
  prioritise_elderly?   ;; Priorizar a los ancianos?
  week  ;; Semana
  filename ;; Nombre del archivo CSV
]

humans-own [
  infected?  ;; Contagiado?
  infection-length ;; Longitud de la infección
  aggravated_symptoms_day ;; Días de sintomas graves
  age-group  ;; Grupo de edad
  ontreatment? ;; En tratamiento?
  gotinfection? ;; Se contagió?
  contagion-chance ;; Posibilidad de contagio
  death-chance ;; Posibilidad de muerte
  ongoing-infection-hours ;; Horas de infección en curso
  symptoms_delay ;; Retraso de los sintomas
  aware_of_infection? ;; Consciente de estar contagiado?
  infectedby ;; Contagiado por
  infection_detected? ;; Contagiado con infeccion detectada
  days_to_detect ;; Días que pasan desde el dia 1 a que es detectado
  effective_isolation? ;; Contagiado con aislamiento efectivo
  group ;; Grupo de cuarentena al que pertenece
  can_move? ;; Si se puede mover segun su grupo
]

statistic_agents-own [
  age-group  ;; Grupo etario
  recovered  ;; Recuperados
  deaths     ;; Muertos
]

patches-own [
  original_map_color ;; Color original del mapa
]

to-report calculate_R0 ;; Calcular el número de reproducciones
  let list_of_transmitters remove-duplicates logged_transmitters
  let current_case 0
  let sum_repetitions 0
  foreach list_of_transmitters [
    patient -> set current_case patient
    let transmitter_repeated length filter [i -> i = current_case] logged_transmitters
    set sum_repetitions sum_repetitions + transmitter_repeated
  ]
  ifelse length list_of_transmitters > 0 [
    report sum_repetitions / ( length list_of_transmitters )
  ]
  [
    report 0
  ]
end

;; Procedimientos de humanos

to infection_exposure
  if (not gotinfection?) [
    let people_around humans-on neighbors
    let infected_around people_around with [infected? = true and not effective_isolation? and not ontreatment? and can_move? and ( ongoing-infection-hours > (average_days_for_contagion * 24 )) ]
    let number_of_infected_around count infected_around
    if number_of_infected_around > 0 [
      let within_contagion_distance (random(metres_per_patch) + 1) ;; Asumiendo que cada patch representa los metros por patch.

      ;; Filtrar la distancia social por rango etario
      if age-group = 9 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_0-9 )
      ] if age-group = 19 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_10-19 )
      ] if age-group = 29 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_20-29 )
      ] if age-group = 39 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_30-39 )
      ] if age-group = 49 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_40-49 )
      ] if age-group = 59 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_50-59 )
      ] if age-group = 69 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_60-69 )
      ] ifelse age-group = 79 [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_70-79 )
      ] [
        set within_contagion_distance random-float within_contagion_distance + ( keep_social_distancing_80 )
      ]

      ;; Posibilidad de contagio según grupo etario.
      if (contagion-chance >= (random(100) + 1) and within_contagion_distance <= maximum_contagion_distance) [
        let transmitter_person nobody
        ask one-of infected_around [ set transmitter_person who ]
        set logged_transmitters lput transmitter_person logged_transmitters
        if length ( logged_transmitters ) > 800 [ ;; No permita que la lista crezca sin fin, elimine los elementos más antiguos.
          set logged_transmitters but-first logged_transmitters
        ]
       get_infected
      ]
    ]
  ]
end

to get_infected
  set color red
  set size 4
  set infected? true
  set gotinfection? true
  set infection-length 24 * ( random-normal average_infection_length 5.0 ) ;; Media de la duración de la infección y la desviación estándar multiplicada por 24 horas
  set aggravated_symptoms_day round (infection-length / 2.5) ;; La infección agravada puede ocurrir después de la primera semana.
  set symptoms_delay 24 * ( random-normal average-symptoms-show 4.0)
  set ongoing-infection-hours 0
  set cumulative_infected cumulative_infected + 1
  set days_to_detect (random(15) + 1)
end

to get-healthy
  set infected? false
  set gotinfection? true
  set infection-length 0
  set ongoing-infection-hours 0
  set aggravated_symptoms_day 0
  if ontreatment? [ free-medical-care set ontreatment? false ]
  set color blue
  set size 4
  set aware_of_infection? false
  set infection_detected? false
  set days_to_detect 0
  set effective_isolation? false
  update-recovered-statistics age-group
end

to check_health_state
  if infected? [
    if ongoing-infection-hours >= symptoms_delay and not ontreatment? [
      if not aware_of_infection? [
        set aware_of_infection? true
        set cumulative_aware_of_infection cumulative_aware_of_infection + 1
      ]
      ifelse prioritise_elderly? [
        ifelse age-group >= age_group_60_69 [
          if get-medical-care = true [
            set ontreatment? true
          ]
        ]
        [
          if %medical-care-availability >= 25 [ ;; Si no es una persona mayor, solo reciba atención médica si la disponibilidad es> = 25%
            if get-medical-care = true [
              set ontreatment? true
            ]
          ]
        ]
      ]
      [
        if get-medical-care = true [
          set ontreatment? true
        ]
      ]
    ]
    if (ongoing-infection-hours / 24 = days_to_detect) [
      set infection_detected? true
      set color violet
      let infected_count count humans with [infected?]
      let detected_count count humans with [infection_detected?]
      let isolated_count count humans with [effective_isolation?]
      if (isolated_count <= detected_count * (%isolated_detected / 100)) [ ;; Si aislados <= %detectados, dar aislamiento efectivo
        set effective_isolation? true
        ;show detected_count
        ;show isolated_count
      ]
    ]
    if (ongoing-infection-hours = aggravated_symptoms_day) [;; Verifica si el paciente va a morir
      let chance_to_die 0
      let severity_factor 1
      ifelse (ontreatment?) [
        set chance_to_die ((death-chance * 1000) * severity_factor) * 0.5 ;; La probabilidad de muerte se reduce al 50%, la probabilidad de supervivencia aumenta en un 50%
      ]
      [
        set chance_to_die (death-chance * 1000) * severity_factor
      ]

      if (chance_to_die >= (random(100000) + 1)) [
        update-death-statistics age-group
        set number_of_deaths number_of_deaths + 1
        delete-person
      ]
    ]

    ifelse (ongoing-infection-hours >= infection-length) [
      set ongoing-infection-hours 0
      get-healthy
    ]
    [
      set ongoing-infection-hours ongoing-infection-hours + 1
    ]
  ]
end

to move [ #speed ]
  if (can_move? and not ontreatment? and not effective_isolation?) [
    rt random-float 360
    fd #speed
  ]
end

to delete-person
  if ontreatment? [ free-medical-care ]
  die
end

;; Setup

to create_city_map
  ask patches with [pxcor > -100 and pxcor < 100 and pycor > -100 and pycor < 100 ] [ set pcolor white ]
end

to setup-globals

  create_city_map
  ask patches [ set original_map_color pcolor ]
  set age_group_0_9 9
  set age_group_10_19 19
  set age_group_20_29 29
  set age_group_30_39 39
  set age_group_40_49 49
  set age_group_50_59 59
  set age_group_60_69 69
  set age_group_70_79 79
  set age_group_80  80
  set elapsed-day-hours 0
  set medical_care_used 0
  set number_of_deaths 0
  set cumulative_infected 0
  set last_cumulative 0
  set city_area_patches patches with [ pcolor != black ]
  set prioritise_elderly? false
  set cumulative_aware_of_infection 0
  set last_cumulative_aware_of_infection 0
  set logged_transmitters[]
  set week 0
end

to setup_statistic_agent [ #age-group ]
  create-statistic_agents 1 [
    set age-group #age-group
    set recovered 0
    set deaths 0
    ht
  ]
end

to setup-people [#number #age-group #group]
  create-humans #number [
      let random_x 0
      let random_y 0
      ask one-of city_area_patches [ set random_x pxcor set random_y pycor ]
      setxy random_x random_y
      set shape "person"
      set infected? false
      set aggravated_symptoms_day 0
      set ongoing-infection-hours 0
      set age-group #age-group
      set ontreatment? false
      set gotinfection? false
      set symptoms_delay 0
      set aware_of_infection? false
      set infectedby nobody
      set size 4
      set infection_detected? false
      set days_to_detect 0
      set effective_isolation? false
      set group #group
      set can_move? false


      ifelse age-group <= age_group_0_9 [
       ifelse (move_0-9 > 0) [
        ifelse (use_mask_0-9) [
          set contagion-chance 3.0
        ]
        [
          set contagion-chance 17.0
        ]
       ]
       [
        set contagion-chance 0.0
       ]
        set death-chance 0.0
        set color black
      ]
      [
        ifelse age-group <= age_group_10_19 [
          ifelse (move_10-19 > 0) [
           ifelse (use_mask_10-19) [
            set contagion-chance 3.0
           ]
           [
            set contagion-chance 17.0
           ]
          ]
          [
           set contagion-chance 0.0
          ]
          set death-chance 0.0
          set color gray
        ]
        [
          ifelse age-group <= age_group_20_29 [
           ifelse (move_20-29 > 0) [
            ifelse (use_mask_20-29) [
             set contagion-chance 3.0
            ]
            [
             set contagion-chance 17.0
            ]
           ]
           [
            set contagion-chance 0.0
           ]
            set death-chance 0.0
            set color pink
          ]
          [
            ifelse age-group <= age_group_30_39 [
             ifelse (move_30-39 > 0) [
              ifelse (use_mask_30-39) [
                set contagion-chance 3.0
              ]
              [
                set contagion-chance 17.0
              ]
            ]
            [
              set contagion-chance 0.0
            ]
              set death-chance 0.3
              set color orange
            ]
            [
              ifelse age-group <= age_group_40_49 [
               ifelse (move_40-49 > 0) [
                ifelse (use_mask_40-49) [
                  set contagion-chance 3.0
                ]
                [
                  set contagion-chance 17.0
                ]
              ]
              [
                set contagion-chance 0.0
              ]
                set death-chance 0.4
                set color brown
              ]
              [
                ifelse age-group <= age_group_50_59 [
                  ifelse (move_50-59 > 0) [
                   ifelse (use_mask_50-59) [
                    set contagion-chance 3.0
                  ]
                  [
                    set contagion-chance 17.0
                  ]
                ]
                [
                  set contagion-chance 0.0
                ]
                  set death-chance 1.0
                  set color yellow
                ]
                [
                  ifelse age-group <= age_group_60_69 [
                   ifelse (move_60-69 > 0) [
                    ifelse (use_mask_60-69) [
                      set contagion-chance 3.0
                    ]
                    [
                      set contagion-chance 17.0
                    ]
                  ]
                  [
                    set contagion-chance 0.0
                  ]
                    set death-chance 3.5
                    set color green
                  ]
                  [
                    ifelse age-group <= age_group_70_79 [
                     ifelse (move_70-79 > 0) [
                      ifelse (use_mask_70-79) [
                        set contagion-chance 3.0
                      ]
                      [
                        set contagion-chance 17.0
                      ]
                    ]
                    [
                      set contagion-chance 0.0
                    ]
                      set death-chance 12.8
                      set color lime
                    ]
                    [
                        ifelse (move_80 > 0) [
                         ifelse (use_mask_80) [

                          set contagion-chance 3.0
                         ]
                         [
                          set contagion-chance 17.0
                         ]
                        ]
                        [
                         set contagion-chance 0.0
                        ]
                        set death-chance 20.2
                        set color turquoise
                    ]
                  ]
                ]
              ]
            ]
          ]

        ]
      ]
     ]
end

to group-divider[#number #age-group]
  let total_group #number
  let group1 0
  let group2 0
  let group3 0
  if (remainder  total_group 3 = 0) [
    set group1 total_group / 3
    set group2 total_group / 3
    set group3 total_group / 3
    setup-people group1 #age-group 1
    setup-people group2 #age-group 2
    setup-people group3 #age-group 3
  ]
  if (remainder  total_group 3 = 1) [
    set group1 floor (total_group / 3) + 1
    set group2 floor (total_group / 3)
    set group3 floor (total_group / 3)
    setup-people group1 #age-group 1
    setup-people group2 #age-group 2
    setup-people group3 #age-group 3
  ]
  if (remainder  total_group 3 = 2) [
    set group1 floor (total_group / 3) + 1
    set group2 floor (total_group / 3) + 1
    set group3 floor (total_group / 3)
    setup-people group1 #age-group 1
    setup-people group2 #age-group 2
    setup-people group3 #age-group 3
  ]
end

to setup
  clear-all

  set filename (word name_of_experiment ".csv")

  if not file-exists? filename [
    file-open filename
    start-output-file
  ]

  setup-globals
  group-divider population_0-9 age_group_0_9
  group-divider population_0-9 age_group_0_9
  group-divider population_10-19 age_group_10_19
  group-divider population_20-29 age_group_20_29
  group-divider population_30-39 age_group_30_39
  group-divider population_40-49 age_group_40_49
  group-divider population_50-59 age_group_50_59
  group-divider population_60-69 age_group_60_69
  group-divider population_70-79 age_group_70_79
  group-divider population_80 age_group_80

  let affected_number round (count humans * (initial_infected_population / 100))
  infect_people affected_number

  ask humans with [group = 1] [set can_move? true]
  show "Comienza a moverse grupo 1"

  setup_statistic_agent age_group_0_9
  setup_statistic_agent age_group_10_19
  setup_statistic_agent age_group_20_29
  setup_statistic_agent age_group_30_39
  setup_statistic_agent age_group_40_49
  setup_statistic_agent age_group_50_59
  setup_statistic_agent age_group_60_69
  setup_statistic_agent age_group_70_79
  setup_statistic_agent age_group_80
  reset-ticks
end

to start-output-file
   file-open filename
   file-type "Días transcurridos,"
   file-type "Muertes de 0-9,"
   file-type "Muertes de 10-19,"
   file-type "Muertes de 20-29,"
   file-type "Muertes de 30-39,"
   file-type "Muertes de 40-49,"
   file-type "Muertes de 50-59,"
   file-type "Muertes de 60-69,"
   file-type "Muertes de 70-79,"
   file-type "Muertes de 80,"
   file-type "Total muertes,"
   file-type "Infectados de 0-9,"
   file-type "Infectados de 10-19,"
   file-type "Infectados de 20-29,"
   file-type "Infectados de 30-39,"
   file-type "Infectados de 40-49,"
   file-type "Infectados de 50-59,"
   file-type "Infectados de 60-69,"
   file-type "Infectados de 70-79,"
   file-type "Infectados de 80,"
   file-type "Total infectados,"
   file-type "Infectados sin tratamientos 0-9,"
   file-type "Infectados sin tratamientos 10-19,"
   file-type "Infectados sin tratamientos 20-29,"
   file-type "Infectados sin tratamientos 30-39,"
   file-type "Infectados sin tratamientos 40-49,"
   file-type "Infectados sin tratamientos 50-59,"
   file-type "Infectados sin tratamientos 60-69,"
   file-type "Infectados sin tratamientos 70-79,"
   file-type "Infectados sin tratamientos 80,"
   file-type "Total Infectados sin tratamientos,"
   ;; Etc., rest of column headers.
   file-print ""
end

; Environment - Statistic_agents procedures

to update-recovered-statistics [ #age-group ]

  ask statistic_agents with [ age-group = #age-group ] [ set recovered recovered + 1 ]

end

to update-death-statistics [ #age-group ]

  ask statistic_agents with [ age-group = #age-group ] [ set deaths deaths + 1 ]

end

; Environment - Human Procedures


to infect_people [#affected_number]
  ask n-of #affected_number humans with [ not gotinfection? ] [ get_infected ]
end


to-report get-medical-care
  if medical_care_used < medical_care_capacity [
    set medical_care_used medical_care_used + 1
    report true
  ]
  report false
end

to-report %medical-care-availability
  report ( (medical_care_capacity - medical_care_used) / medical_care_capacity ) * 100
end

to free-medical-care
  set medical_care_used medical_care_used - 1
end


; Go procedures
to go

  if iterations = 0 [
    stop
  ]

  if (count humans with [infected?] = 0) [
    ; Variables temporales relacionadas a las muertes
    let deaths_0-9 [deaths] of one-of statistic_agents with [age-group = age_group_0_9]
    let deaths_10-19 [deaths] of one-of statistic_agents with [age-group = age_group_10_19]
    let deaths_20-29 [deaths] of one-of statistic_agents with [age-group = age_group_20_29]
    let deaths_30-39 [deaths] of one-of statistic_agents with [age-group = age_group_30_39]
    let deaths_40-49 [deaths] of one-of statistic_agents with [age-group = age_group_40_49]
    let deaths_50-59 [deaths] of one-of statistic_agents with [age-group = age_group_50_59]
    let deaths_60-69 [deaths] of one-of statistic_agents with [age-group = age_group_60_69]
    let deaths_70-79 [deaths] of one-of statistic_agents with [age-group = age_group_70_79]
    let deaths_80 [deaths] of one-of statistic_agents with [age-group = age_group_80]
    let totalDeaths number_of_deaths

    ; Variables temporales relacionadas a los infectados
    let infecteds_0-9 count humans with [age-group = age_group_0_9 and gotinfection?]
    let infecteds_10-19 count humans with [age-group = age_group_10_19 and gotinfection?]
    let infecteds_20-29 count humans with [age-group = age_group_20_29 and gotinfection?]
    let infecteds_30-39 count humans with [age-group = age_group_30_39 and gotinfection?]
    let infecteds_40-49 count humans with [age-group = age_group_40_49 and gotinfection?]
    let infecteds_50-59 count humans with [age-group = age_group_50_59 and gotinfection?]
    let infecteds_60-69 count humans with [age-group = age_group_60_69 and gotinfection?]
    let infecteds_70-79 count humans with [age-group = age_group_70_79 and gotinfection?]
    let infecteds_80 count humans with [age-group = age_group_80 and gotinfection?]
    let totalInfecteds count humans with [not infected? and gotinfection?]

    ; Variables relacionadas a las personas infectadas sin tratamiento
    let notTreatment_0-9 count humans with [age-group = age_group_0_9 and not ontreatment?]
    let notTreatment_10-19 count humans with [age-group = age_group_10_19 and not ontreatment?]
    let notTreatment_20-29 count humans with [age-group = age_group_20_29 and not ontreatment?]
    let notTreatment_30-39 count humans with [age-group = age_group_30_39 and not ontreatment?]
    let notTreatment_40-49 count humans with [age-group = age_group_40_49 and not ontreatment?]
    let notTreatment_50-59 count humans with [age-group = age_group_50_59 and not ontreatment?]
    let notTreatment_60-69 count humans with [age-group = age_group_60_69 and not ontreatment?]
    let notTreatment_70-79 count humans with [age-group = age_group_70_79 and not ontreatment?]
    let notTreatment_80 count humans with [age-group = age_group_80 and not ontreatment?]
    let totalNotTreatments count humans with [not ontreatment?]

    let days ceiling (ticks / 24)

    file-open filename

    file-type (word days ", ")

    file-type (word deaths_0-9 ", ")
    file-type (word deaths_10-19 ", ")
    file-type (word deaths_20-29 ", ")
    file-type (word deaths_30-39 ", ")
    file-type (word deaths_40-49 ", ")
    file-type (word deaths_50-59 ", ")
    file-type (word deaths_60-69 ", ")
    file-type (word deaths_70-79 ", ")
    file-type (word deaths_80 ", ")
    file-type (word totalDeaths ", ")

    file-type (word infecteds_0-9 ", ")
    file-type (word infecteds_10-19 ", ")
    file-type (word infecteds_20-29 ", ")
    file-type (word infecteds_30-39 ", ")
    file-type (word infecteds_40-49 ", ")
    file-type (word infecteds_50-59 ", ")
    file-type (word infecteds_60-69 ", ")
    file-type (word infecteds_70-79 ", ")
    file-type (word infecteds_80 ", ")
    file-type (word totalInfecteds ", ")

    file-type (word notTreatment_0-9 ", ")
    file-type (word notTreatment_10-19 ", ")
    file-type (word notTreatment_20-29 ", ")
    file-type (word notTreatment_30-39 ", ")
    file-type (word notTreatment_40-49 ", ")
    file-type (word notTreatment_50-59 ", ")
    file-type (word notTreatment_60-69 ", ")
    file-type (word notTreatment_70-79 ", ")
    file-type (word notTreatment_80 ", ")
    file-type (word totalNotTreatments ", ")

    file-print ""

    file-close

    set iterations iterations - 1
    go
  ]

  ifelse prioritise_elderly? [
    foreach sort-on [(- age-group)] humans
    [ patient -> ask patient [ check_health_state ] ]
  ]
  [
      ask humans [ check_health_state ]
  ]

  let temp_weeker week
  set week floor (ticks / 168)

  if (temp_weeker != week) [
      ifelse (remainder week 3 = 0) [
      show "Se mueve grupo 1"
      ask humans with [group = 1] [set can_move? true]
      ask humans with [group = 2] [set can_move? false]
      ask humans with [group = 3] [set can_move? false]
      ][
      ifelse (remainder week 3 = 1) [
        show "Se mueve grupo 2"
        ask humans with [group = 1] [set can_move? false]
        ask humans with [group = 2] [set can_move? true]
        ask humans with [group = 3] [set can_move? false]
      ][
        show "Se mueve grupo 3"
        ask humans with [group = 1] [set can_move? false]
        ask humans with [group = 2] [set can_move? false]
        ask humans with [group = 3] [set can_move? true]
      ]
    ]
  ]




  ask humans [

        ifelse age-group <= 9 [
         let mage_group_0-9 humans with [age-group = 9]
         ask one-of mage_group_0-9 [ move move_0-9 ]
        ]
        [
          ifelse age-group <= 19 [
           let mage_group_10-19 humans with [age-group = 19]
           ask one-of mage_group_10-19 [ move move_10-19 ]
          ]
          [
            ifelse age-group <= 29 [
              let mage_group_20-29 humans with [age-group = 29]
              ask one-of mage_group_20-29 [ move move_20-29 ]
            ]
            [
              ifelse age-group <= 39 [
                let mage_group_30-39 humans with [age-group = 39]
                ask one-of mage_group_30-39 [ move move_30-39 ]
              ]
              [
                ifelse age-group <= 49 [
                  let mage_group_40-49 humans with [age-group = 49]
                  ask one-of mage_group_40-49 [ move move_40-49 ]
                ]
                [
                  ifelse age-group <= 59 [
                    let mage_group_50-59 humans with [age-group = 59]
                    ask one-of mage_group_50-59 [ move move_50-59 ]
                  ]
                  [
                    ifelse age-group <= 69 [
                      let mage_group_60-69 humans with [age-group = 69]
                      ask one-of mage_group_60-69 [ move move_60-69 ]
                    ]
                    [
                    ifelse age-group <= 79 [
                      let mage_group_70-79 humans with [age-group = 79]
                      ask one-of mage_group_70-79 [ move move_70-79 ]
                    ]
                    [
                      let mage_group_80 humans with [age-group = 80 ]
                      ask one-of mage_group_80 [ move move_80 ]
                    ]
                    ]
                  ]
                ]
              ]
            ]
          ]

        ]
      ]

  ask humans with [can_move?] [ infection_exposure ]

  ifelse elapsed-day-hours >= 24
  [
    if log_infection_data? [
      let delta_cumulative cumulative_aware_of_infection / (last_cumulative_aware_of_infection + 1)
      set last_cumulative_aware_of_infection cumulative_aware_of_infection
      set last_cumulative cumulative_infected
    ]
    set elapsed-day-hours 1
  ]
  [
    set elapsed-day-hours elapsed-day-hours + 1
  ]

  tick
end


to-report %infected
  ifelse any? humans
    [ report (count humans with [infected?] / count humans) * 100 ]
    [ report 0 ]
end
@#$#@#$#@
GRAPHICS-WINDOW
1255
73
1810
629
-1
-1
2.7214
1
10
1
1
1
0
1
1
1
-100
100
-100
100
0
0
1
Hours
60.0

SLIDER
974
252
1234
285
average_days_for_contagion
average_days_for_contagion
0
100
2.0
1
1
Days
HORIZONTAL

SLIDER
972
378
1229
411
metres_per_patch
metres_per_patch
1
40
1.0
1
1
NIL
HORIZONTAL

SLIDER
687
78
944
111
keep_social_distancing_0-9
keep_social_distancing_0-9
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
973
296
1235
329
maximum_contagion_distance
maximum_contagion_distance
1
3
2.0
0.5
1
mts
HORIZONTAL

TEXTBOX
981
54
1131
72
Parametros medicos
12
0.0
1

TEXTBOX
688
40
838
70
Parametros interacciones sociales
12
0.0
1

SLIDER
975
167
1231
200
average_infection_length
average_infection_length
1
60
37.0
1
1
Days
HORIZONTAL

SLIDER
973
211
1232
244
average-symptoms-show
average-symptoms-show
4
15
15.0
1
1
Days
HORIZONTAL

SLIDER
972
79
1230
112
medical_care_capacity
medical_care_capacity
0
250
11.0
1
1
Beds
HORIZONTAL

SLIDER
223
76
487
109
move_0-9
move_0-9
0
10
10.0
1
1
vel
HORIZONTAL

TEXTBOX
227
36
466
81
Velocidad de movimiento (Repeticion de un salto de 1mt n veces)
12
0.0
1

TEXTBOX
31
33
181
63
Población \n(Por rango etario)
12
0.0
1

SLIDER
16
78
188
111
population_0-9
population_0-9
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
17
122
189
155
population_10-19
population_10-19
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
16
166
192
199
population_20-29
population_20-29
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
14
210
191
243
population_30-39
population_30-39
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
14
258
193
291
population_40-49
population_40-49
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
15
299
191
332
population_50-59
population_50-59
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
11
342
193
375
population_60-69
population_60-69
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
10
382
192
415
population_70-79
population_70-79
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
12
428
193
461
population_80
population_80
0
150
100.0
1
1
NIL
HORIZONTAL

SLIDER
973
124
1230
157
initial_infected_population
initial_infected_population
0
1
1.0
0.01
1
%
HORIZONTAL

BUTTON
1254
29
1532
69
Reiniciar
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1573
27
1806
70
Comenzar
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
2036
18
2625
237
Muertes por rango etario
Days
People
0.0
90.0
0.0
20.0
true
true
"" "if not show_plot_3? [stop]"
PENS
"0-9" 0.04 0 -16777216 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_0_9]"
"10-19" 0.04 0 -7500403 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_10_19]"
"20-29" 0.04 0 -2064490 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_20_29]"
"30-39" 0.04 0 -955883 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_30_39]"
"40-49" 0.04 0 -6459832 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_40_49]"
"50-59" 0.04 0 -1184463 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_50_59]"
"60-69" 0.04 0 -10899396 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_60_69]"
"70-79" 0.04 0 -13840069 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_70_79]"
">=80" 0.04 0 -14835848 true "" "plot [deaths] of one-of statistic_agents with [age-group = age_group_80]"

SWITCH
2036
239
2187
272
show_plot_3?
show_plot_3?
1
1
-1000

PLOT
2035
282
2627
504
Infectados por rango etario
Days
People
0.0
90.0
0.0
20.0
true
true
";set-plot-y-range 0 ((count humans / 9) + 50)" "if not show_plot_2? [stop]"
PENS
"0-9" 0.04 0 -16777216 true "" "plot count humans with [age-group = age_group_0_9 and infected?]"
"10-19" 0.04 0 -7500403 true "" "plot count humans with [age-group = age_group_10_19 and infected?]"
"20-29" 0.04 0 -2064490 true "" "plot count humans with [age-group = age_group_20_29 and infected?]"
"30-39" 0.04 0 -955883 true "" "plot count humans with [age-group = age_group_30_39 and infected?]"
"40-49" 0.04 0 -6459832 true "" "plot count humans with [age-group = age_group_40_49 and infected?]"
"50-59" 0.04 0 -1184463 true "" "plot count humans with [age-group = age_group_50_59 and infected?]"
"60-69" 0.04 0 -10899396 true "" "plot count humans with [age-group = age_group_60_69 and infected?]"
"70-79" 0.04 0 -13840069 true "" "plot count humans with [age-group = age_group_70_79 and infected?]"
">=80" 0.04 0 -14835848 true "" "plot count humans with [age-group = age_group_80 and infected?]"

SWITCH
2033
507
2184
540
show_plot_2?
show_plot_2?
0
1
-1000

SLIDER
223
122
488
155
move_10-19
move_10-19
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
223
165
487
198
move_20-29
move_20-29
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
223
209
487
242
move_30-39
move_30-39
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
224
252
485
285
move_40-49
move_40-49
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
223
294
482
327
move_50-59
move_50-59
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
223
333
482
366
move_60-69
move_60-69
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
222
375
481
408
move_70-79
move_70-79
0
10
10.0
1
1
vel
HORIZONTAL

SLIDER
222
416
481
449
move_80
move_80
0
10
10.0
1
1
vel
HORIZONTAL

TEXTBOX
1828
45
1978
63
Resultados Generales\n
12
0.0
1

MONITOR
1825
68
1909
129
Días
ceiling (ticks / 24)
0
1
15

MONITOR
1920
70
2006
131
R0
calculate_R0
2
1
15

MONITOR
1826
136
2006
177
% Personas infectadas
%infected
2
1
10

MONITOR
1827
183
2005
224
# Personas infectadas
count humans with [infected?]
0
1
10

MONITOR
1827
232
2006
273
# Número de muertos
number_of_deaths
0
1
10

MONITOR
1827
280
2007
321
# Número de recuperados
count humans with [not infected? and gotinfection?]
0
1
10

TEXTBOX
516
53
666
71
Autocuidado
12
0.0
1

SWITCH
510
75
663
108
use_mask_0-9
use_mask_0-9
1
1
-1000

SWITCH
508
116
663
149
use_mask_10-19
use_mask_10-19
1
1
-1000

SWITCH
509
160
663
193
use_mask_20-29
use_mask_20-29
1
1
-1000

SWITCH
509
205
663
238
use_mask_30-39
use_mask_30-39
1
1
-1000

SWITCH
508
252
666
285
use_mask_40-49
use_mask_40-49
1
1
-1000

SWITCH
508
294
664
327
use_mask_50-59
use_mask_50-59
1
1
-1000

SWITCH
506
336
664
369
use_mask_60-69
use_mask_60-69
1
1
-1000

SWITCH
505
376
666
409
use_mask_70-79
use_mask_70-79
1
1
-1000

SWITCH
504
415
667
448
use_mask_80
use_mask_80
1
1
-1000

SLIDER
687
119
943
152
keep_social_distancing_10-19
keep_social_distancing_10-19
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
684
162
942
195
keep_social_distancing_20-29
keep_social_distancing_20-29
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
685
207
944
240
keep_social_distancing_30-39
keep_social_distancing_30-39
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
684
253
945
286
keep_social_distancing_40-49
keep_social_distancing_40-49
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
686
295
946
328
keep_social_distancing_50-59
keep_social_distancing_50-59
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
687
341
948
374
keep_social_distancing_60-69
keep_social_distancing_60-69
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
686
384
948
417
keep_social_distancing_70-79
keep_social_distancing_70-79
0
4
0.0
0.5
1
NIL
HORIZONTAL

SLIDER
687
422
948
455
keep_social_distancing_80
keep_social_distancing_80
0
4
0.0
0.5
1
NIL
HORIZONTAL

TEXTBOX
974
350
1124
368
Parametro adicional
12
0.0
1

PLOT
2033
548
2631
748
Salud de la población
Días
Personas
0.0
90.0
0.0
350.0
true
true
"set-plot-y-range 0 ((count humans) + 50)" "if not show_plot_1? [stop]"
PENS
"Covid-19 Negativo" 0.04 0 -10899396 true "" "plot count humans with [not infected?]"
"Covid-19 Positivo" 0.04 0 -2674135 true "" "plot count humans with [infected?]"
"Recuperados" 0.04 0 -7500403 true "" "plot count humans with [not infected? and gotinfection?]"

SWITCH
2033
756
2184
789
show_plot_1?
show_plot_1?
0
1
-1000

PLOT
2034
808
2631
1003
Cantidad de personas infectadas sin tratamiento por rango etario
NIL
NIL
0.0
90.0
0.0
90.0
true
true
"" "if not show_plot0? [stop]"
PENS
"0-9" 0.04 0 -16777216 true "" "plot count humans with [age-group = age_group_0_9 and infected? and not ontreatment?]"
"10-19" 0.04 0 -7500403 true "" "plot count humans with [age-group = age_group_10_19 and infected? and not ontreatment?]"
"20-29" 0.04 0 -2674135 true "" "plot count humans with [age-group = age_group_20_29 and infected? and not ontreatment?]"
"30-39" 0.04 0 -955883 true "" "plot count humans with [age-group = age_group_30_39 and infected? and not ontreatment?]"
"40-49" 0.04 0 -6459832 true "" "plot count humans with [age-group = age_group_40_49 and infected? and not ontreatment?]"
"50-59" 0.04 0 -1184463 true "" "plot count humans with [age-group = age_group_0_9 and infected? and not ontreatment?]"
"60-69" 0.04 0 -10899396 true "" "plot count humans with [age-group = age_group_60_69 and infected? and not ontreatment?]"
"70-79" 0.04 0 -13840069 true "" "plot count humans with [age-group = age_group_70_79 and infected? and not ontreatment?]"
">=80" 0.04 0 -14835848 true "" "plot count humans with [age-group = age_group_80 and infected? and not ontreatment?]"

SWITCH
2033
1010
2158
1043
show_plot0?
show_plot0?
0
1
-1000

INPUTBOX
1826
345
1987
405
iterations
0.0
1
0
Number

INPUTBOX
1827
418
1990
478
name_of_experiment
prueba7
1
0
String

SLIDER
685
492
944
525
%isolated_detected
%isolated_detected
0
100
50.0
1
1
%
HORIZONTAL

SWITCH
686
541
881
574
log_infection_data?
log_infection_data?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

Se comienza con una población inicial, donde cada agente pertenece a un grupo etario. Cada grupo etario puede desplazarse a cierta velocidad, utilizar o no mascarillas y respetar o no el distanciamiento social. Al inicio de cada simulación tambien se presenta una cantidad de agentes infectados los los cuales tienen la posibilidad de infectar al resto de agentes. 

Cada agente presenta las siguientes características:

  infected?  ;; Contagiado?
  infection-length ;; Longitud de la infección
  aggravated_symptoms_day ;; Días de sintomas graves
  age-group  ;; Grupo de edad
  ontreatment? ;; En tratamiento?
  gotinfection? ;; Se contagió?
  contagion-chance ;; Posibilidad de contagio
  death-chance ;; Posibilidad de muerte
  ongoing-infection-hours ;; Horas de infección en curso
  symptoms_delay ;; Retraso de los sintomas
  aware_of_infection? ;; Consciente de estar contagiado?
  infectedby ;; Contagiado por
  infection_detected? ;; Contagiado con infeccion detectada
  days_to_detect ;; Días que pasan desde el dia 1 a que es detectado
  effective_isolation? ;; Contagiado con aislamiento efectivo

La capacidad de moverse del grupo etario, el uso de mascarilla y distanciamiento social, definen la probabilidad de ser contagiado por otro agente.

Los días de sitomas graves, si recibe o no tratamiento y la tasa de mortalidad del grupo etario define si el paciente puede morir por la enfermedad.

Los agentes contagiados, pueden ser detectados de forma aleatoria entre el día 1 al día 15 de su contagio, por lo que una vez detectado y dependiendo del parametro "%isolated_detected" puede asignarse una población infectada aislada la cual no podra moverse y contagiar al resto.

Constantemente se realiza una revisión del estado de salud de la poblacion, donde se verifica la capacidad para tratar a los contagiados, la detección del contagio y su posibilidad de muerte por la enfermedad.


Desplazamiento de los agentes:

*Cuarentena Intermitente
La población total de agentes se encuentra dividida en tres grupos, donde cada grupo tiene la posibilidad de desplazarse durante una semana cada dos semanas, por lo que todas las semanas se mueve un grupo distinto.

*Por parámetro:
Los agenets podrán moverse siempre y cuando la velocidad de movimiento de su grupo etario se lo permita, si su velocidad es 0 se asume un aislamiento del agente, por lo cual su probabilidad de contagiarse es 0. 

*Por caracteristicas del agente contagiado:
Si el agente contagiado se encuentra en tratamiento este tiene mayor posibilidad de sobrevivir a la enfermedad, además no cantaiga al resto y no puede desplazarse. La cantidad de agentes en tratamiento depende de la capacidad hospitalaria.

Si el agente contagiado es un contagiado detectado y está dentro de la población infectada aislada el agente no se podrá desplazar y no podrá contagiar a otros. La cantidad de agentes en aislamiento efectivo depende del parametro "%isolated_detected" el cual representa el porcentaje de agentes detectados que pasarán a aislamiento efectivo.


Código de Colores:

Cada grupo etario posee un color característico el cual se puede ver tambien en los gráficos, aparte de esos colores tambien existen estados que alteran el color del agente:

-Si esta contagiado y no es detectado su color cambia a rojo.
-Si es un contagiado detectado (detectado con o sin aislamiento efectivo) su color cambia a violeta.
-Si es un agente recuperado su color cambia a azul.


## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
