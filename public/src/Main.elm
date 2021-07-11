module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text, input, label, h1, table, thead, tbody, tr, th, td)
import Html.Attributes exposing (value, class, maxlength, autocomplete)
import Html.Events exposing (onClick, onInput)

import Http as Http
import Data.Api as Api

type Model 
  = Initialization
  | Ready (List Api.Patient)
  | Failed Http.Error

type Message
  = PatientsFetched (Result Http.Error (List Api.Patient))

main = Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = (\model -> Sub.none) 
  }

init: () -> (Model, Cmd Message)
init _ = (Initialization, Api.fetchPatients |> Cmd.map PatientsFetched)

update: Message -> Model -> (Model, Cmd Message)
update msg model = case (model, msg) of
  (_, PatientsFetched (Ok patients)) -> (Ready patients, Cmd.none)
  (_, PatientsFetched (Err err))     -> (Failed err, Cmd.none)

view : Model -> Html Message
view model = case model of 
  Initialization ->
    text "initializing"

  Ready patients -> 
    div [] 
    [ h1 [] [text "Patients"]
    , viewPatientsTable patients
    ]

  Failed problem ->
    text "there was a problem fetching patients"

viewPatientsTable: List Api.Patient -> Html Message
viewPatientsTable patients = 
  table []
  [ thead []
    [ tr []
      [ th [] [text "Id"]
      , th [] [text "Name"]
      , th [] [text "Phone number"]
      , th [] [text "Email address"]
      , th [] [text "Insurances"]
      ]
    ]
  , tbody [] (List.map (\patient -> 
      tr [] 
      [ td [] [text patient.id]
      , td [] [text (patient.person.firstName ++ " " ++ patient.person.lastName)]
      , td [] [text (Maybe.withDefault "-" <| patient.person.phoneNumber)]
      , td [] [text (Maybe.withDefault "-" <| patient.person.emailAddress)]
      , td [] [text (String.fromInt <| List.length <| patient.insurances)]
      ]) patients)
  ]
