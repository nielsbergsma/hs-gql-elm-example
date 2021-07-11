module Data.Api exposing (..)

import Http as Http
import Json.Encode as Encode
import Json.Decode as Decode
import Data.Decode as Decode
import Json.Decode.Pipeline as Decode

-- model
type alias Patient =
  { id:         String
  , person:     Person
  , insurances: List Insurance
  }

type alias Person = 
  { firstName:    String
  , lastName:     String
  , phoneNumber:  Maybe String
  , emailAddress: Maybe String
  }

type alias Insurance =
  { code:       String
  , plan:       Plan
  , isExpired:  Bool
  }

type Plan = Primary | Secondary | Tertiary

-- http
fetchPatients: Cmd (Result Http.Error (List Patient))
fetchPatients = Http.post
  { url    = "/api"
  , body   = Http.jsonBody patientsRequest
  , expect = Http.expectJson identity decodePatientsResponse
  }

-- graphql
patientsQuery: String
patientsQuery = 
  """
  {
    patients {
      id
      person {
        firstName
        lastName
        phoneNumber
        emailAddress
      }
      insurances {
        code
        plan
        isExpired
      }
    }
  }
  """

-- encoders
patientsRequest: Encode.Value
patientsRequest = 
   Encode.object
   [ ("query", Encode.string patientsQuery)
   ]

-- decoders
decodePatientsResponse: Decode.Decoder (List Patient)
decodePatientsResponse = Decode.at ["data", "patients"] (Decode.list decodePatient)

decodePatient: Decode.Decoder Patient
decodePatient =
  Decode.succeed Patient
    |> Decode.required "id" Decode.string
    |> Decode.required "person" decodePerson
    |> Decode.required "insurances" (Decode.list decodeInsurance)

decodePerson: Decode.Decoder Person
decodePerson =
  Decode.succeed Person
    |> Decode.required "firstName" Decode.string
    |> Decode.required "lastName" Decode.string
    |> Decode.optMaybe "phoneNumber" Decode.string
    |> Decode.optMaybe "emailAddress" Decode.string

decodeInsurance: Decode.Decoder Insurance
decodeInsurance =
  Decode.succeed Insurance
    |> Decode.required "code" Decode.string
    |> Decode.required "plan" decodePlan
    |> Decode.required "isExpired" Decode.bool

decodePlan: Decode.Decoder Plan
decodePlan = 
  Decode.string |> Decode.andThen (\value -> case value of
   "Primary"   -> Decode.succeed Primary
   "Secondary" -> Decode.succeed Secondary
   "Tertiary"  -> Decode.succeed Tertiary 
   _           -> Decode.fail "invalid plan")