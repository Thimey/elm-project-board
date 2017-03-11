module WidgetGroup exposing (Model, view)

import Html exposing (..)
import Html.Attributes exposing (class, target, href, property, defaultValue, value, type_)
import Html.Events exposing (..)


type alias Model =
    { widgets : List Widget
    , titleInput : String
    , contentInput : String
    , lastId : Int
    }


type alias Widget =
    { title : String
    , content : String
    , id : Int
    }


initialModel : Model
initialModel =
    { widgets = []
    , titleInput = ""
    , contentInput = ""
    , lastId = 0
    }


type Msg
    = SetTitleInput String
    | SetContentInput String
    | AddWidget Widget


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTitleInput title ->
            { model | titleInput = title }

        SetContentInput content ->
            { model | contentInput = content }

        AddWidget widget ->
            { model | widgets = model.widgets ++ [ widget ], lastId = widget.id }


renderWidget : Widget -> Html msg
renderWidget widget =
    div [ class "card" ]
        [ header [ class "card-header" ] [ p [ class "card-header-title" ] [ text widget.title ] ]
        , div [ class "card-content" ] [ div [ class "content" ] [ text widget.content ] ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ p [ class "control" ]
                [ input
                    [ class "input is-primary"
                    , onInput SetTitleInput
                    , type_ "text"
                    ]
                    []
                ]
            , p [ class "control" ]
                [ textarea
                    [ class "textarea"
                    , onInput SetContentInput
                    ]
                    []
                ]
            ]
        , div
            [ class "button is-primary"
            , onClick (AddWidget (Widget model.titleInput model.contentInput (model.lastId + 1)))
            ]
            [ text "Add" ]
        , ul [] (List.map renderWidget model.widgets)
        ]
