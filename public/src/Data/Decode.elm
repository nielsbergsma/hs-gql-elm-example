module Data.Decode exposing (optMaybe, always)

import Json.Decode as Decode 
import Json.Decode.Pipeline as Decode

optMaybe : String -> Decode.Decoder a -> Decode.Decoder (Maybe a -> b) -> Decode.Decoder b
optMaybe property decoder = Decode.optional property (Decode.map Just decoder) Nothing

always: a -> Decode.Decoder a
always value = Decode.map (\_ -> value) Decode.value