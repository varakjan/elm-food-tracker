port module Main exposing (Food, Model, Msg(..), foodItem, foodItems, getCalories, getTotalCalories, getTotalCarbohydrates, getTotalFats, getTotalProteins, init, inputsFilled, main, setStorage, update, updateFood, updateWithStorage, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Food Tracker", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- model


type alias Food =
    { name : String
    , calories : String
    , proteins : String
    , carbohydrates : String
    , fats : String
    }


type alias Model =
    { totalCalories : String
    , calories : String
    , inputName : String
    , inputProteins : String
    , inputCarbohydrates : String
    , inputFats : String
    , totalProteins : String
    , totalCarbohydrates : String
    , totalFats : String
    , food : Food
    , foodList : List Food
    , error : String
    }


emptyModel : Model
emptyModel =
    { totalCalories = ""
    , calories = ""
    , inputName = ""
    , inputProteins = ""
    , inputCarbohydrates = ""
    , inputFats = ""
    , totalProteins = ""
    , totalCarbohydrates = ""
    , totalFats = ""
    , food = { name = "", calories = "", proteins = "", carbohydrates = "", fats = "" }
    , foodList = []
    , error = ""
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



-- update


inputsFilled model =
    let
        filled =
            model.inputName /= "" && model.inputProteins /= "" && model.inputCarbohydrates /= "" && model.inputFats /= ""
    in
    if filled then
        { model
            | calories = getCalories model.inputProteins model.inputCarbohydrates model.inputFats
            , totalCalories = getTotalCalories model
            , foodList = model.food :: model.foodList
            , totalProteins = getTotalProteins model
            , totalCarbohydrates = getTotalCarbohydrates model
            , totalFats = getTotalFats model
            , inputName = ""
            , inputProteins = ""
            , inputCarbohydrates = ""
            , inputFats = ""
            , error = ""
        }

    else
        { model | error = "You must fill all fields!" }


getCalories proteins carbohydrates fats =
    String.fromInt
        (Maybe.withDefault 0 (String.toInt proteins)
            * 4
            + Maybe.withDefault 0 (String.toInt carbohydrates)
            * 4
            + Maybe.withDefault 0 (String.toInt fats)
            * 9
        )


getTotalCalories : Model -> String
getTotalCalories model =
    String.fromInt
        (Maybe.withDefault 0 (String.toInt model.calories)
            + (Maybe.withDefault 0 (String.toInt model.inputProteins)
                * 4
                + Maybe.withDefault 0 (String.toInt model.inputCarbohydrates)
                * 4
                + Maybe.withDefault 0 (String.toInt model.inputFats)
                * 9
              )
        )


getTotalProteins : Model -> String
getTotalProteins model =
    String.fromInt
        (Maybe.withDefault 0 (String.toInt model.totalProteins)
            + Maybe.withDefault 0 (String.toInt model.inputProteins)
        )


getTotalCarbohydrates : Model -> String
getTotalCarbohydrates model =
    String.fromInt
        (Maybe.withDefault 0 (String.toInt model.totalCarbohydrates)
            + Maybe.withDefault 0 (String.toInt model.inputCarbohydrates)
        )


getTotalFats : Model -> String
getTotalFats model =
    String.fromInt
        (Maybe.withDefault 0 (String.toInt model.totalFats)
            + Maybe.withDefault 0 (String.toInt model.inputFats)
        )


updateFood : String -> String -> String -> String -> Food
updateFood name proteins carbohydrates fats =
    { name = name
    , calories = getCalories proteins carbohydrates fats
    , proteins = proteins
    , carbohydrates = carbohydrates
    , fats = fats
    }


type Msg
    = ValidateInputs
    | InputName String
    | InputProteins String
    | InputCarbohydrates String
    | InputFats String
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ValidateInputs ->
            ( inputsFilled model, Cmd.none )

        InputName val ->
            ( { model
                | inputName = val
                , food = updateFood val model.inputProteins model.inputCarbohydrates model.inputFats
              }
            , Cmd.none
            )

        InputProteins val ->
            ( { model
                | inputProteins = val
                , food = updateFood model.inputName val model.inputCarbohydrates model.inputFats
              }
            , Cmd.none
            )

        InputCarbohydrates val ->
            ( { model
                | inputCarbohydrates = val
                , food = updateFood model.inputName model.inputProteins val model.inputFats
              }
            , Cmd.none
            )

        InputFats val ->
            ( { model
                | inputFats = val
                , food = updateFood model.inputName model.inputProteins model.inputCarbohydrates val
              }
            , Cmd.none
            )

        Clear ->
            ( emptyModel, Cmd.none )



-- view


foodItem : Food -> Html Msg
foodItem food =
    li []
        [ p [] [ b [] [ text (String.concat [ food.name, ": ", food.calories, " kcal" ]) ] ]
        , p [] [ text (String.concat [ "proteins: ", food.proteins ]) ]
        , p [] [ text (String.concat [ "carbohydrates: ", food.carbohydrates ]) ]
        , p [] [ text (String.concat [ "fats: ", food.fats ]) ]
        ]


foodItems : List Food -> Html Msg
foodItems foodList =
    let
        child =
            List.map foodItem foodList
    in
    ul
        []
        child


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Food Tracker" ]
        , h3 [ class "error" ] [ text model.error ]
        , h3 []
            [ text
                (String.concat
                    [ "Total kcal: ", model.totalCalories ]
                )
            ]
        , h3 []
            [ text
                (String.concat
                    [ "Total Proteins: ", model.totalProteins ]
                )
            ]
        , h3 []
            [ text
                (String.concat
                    [ "Total Carbohydrates: ", model.totalCarbohydrates ]
                )
            ]
        , h3 []
            [ text
                (String.concat
                    [ "Total Fats: ", model.totalFats ]
                )
            ]
        , p
            []
            [ text "Name" ]
        , input
            [ type_ "text"
            , size 10
            , onInput InputName
            , value model.inputName
            ]
            []
        , p
            []
            [ text "Proteins" ]
        , input
            [ type_ "text"
            , maxlength 4
            , size 4
            , onInput InputProteins
            , value model.inputProteins
            ]
            []
        , p
            []
            [ text "Carbohydrates" ]
        , input
            [ type_ "text"
            , maxlength 4
            , size 4
            , onInput InputCarbohydrates
            , value model.inputCarbohydrates
            ]
            []
        , p
            []
            [ text "Fats" ]
        , input
            [ type_ "text"
            , maxlength 4
            , size 4
            , onInput InputFats
            , value model.inputFats
            ]
            []
        , br [] []
        , button
            [ type_ "button"
            , onClick ValidateInputs
            ]
            [ text "Add" ]
        , button
            [ type_ "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        , foodItems model.foodList
        ]
