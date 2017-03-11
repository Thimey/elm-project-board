module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, target, href, property, defaultValue, value, type_, placeholder)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { widgets : List Widget
    , categories : List Category
    , categoryInput : String
    , lastId : Int
    }


type alias Category =
    { id : Int
    , label : String
    , titleInput : String
    , contentInput : String
    }


type WidgetInput
    = TitleInput
    | ContentInput


type alias Widget =
    { id : Int
    , category : Int
    , title : String
    , content : String
    }


initialModel : Model
initialModel =
    { widgets = []
    , categories = []
    , categoryInput = ""
    , lastId = 0
    }


type Msg
    = SetTitleInput Int String
    | SetContentInput Int String
    | AddWidget Widget
    | AddCategory Category
    | SetCategoryInput String


updateCategoryInput : WidgetInput -> String -> Int -> Category -> Category
updateCategoryInput input value id cat =
    case input of
        TitleInput ->
            if cat.id == id then
                { cat | titleInput = value }
            else
                cat

        ContentInput ->
            if cat.id == id then
                { cat | contentInput = value }
            else
                cat


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTitleInput id title ->
            { model
                | categories =
                    List.map
                        (updateCategoryInput TitleInput title id)
                        model.categories
            }

        SetContentInput id content ->
            { model
                | categories =
                    List.map
                        (updateCategoryInput ContentInput content id)
                        model.categories
            }

        AddWidget widget ->
            { model
                | widgets = widget :: model.widgets
                , lastId = model.lastId + 1
                , categories =
                    List.map
                        (\cat ->
                            if cat.id == widget.category then
                                { cat | titleInput = "", contentInput = "" }
                            else
                                cat
                        )
                        model.categories
            }

        AddCategory category ->
            { model
                | categories = category :: model.categories
                , categoryInput = ""
                , lastId = model.lastId + 1
            }

        SetCategoryInput category ->
            { model | categoryInput = category }


renderWidget : Widget -> Html msg
renderWidget widget =
    div [ class "card" ]
        [ header [ class "card-header" ] [ p [ class "card-header-title" ] [ text widget.title ] ]
        , div [ class "card-content" ] [ div [ class "content" ] [ text widget.content ] ]
        ]


textInput : (String -> Msg) -> String -> String -> Html Msg
textInput msg label inputValue =
    p [ class "control" ]
        [ input
            [ class "input is-primary"
            , onInput msg
            , type_ "text"
            , placeholder label
            , value inputValue
            ]
            []
        ]


textAreaInput : (String -> Msg) -> String -> String -> Html Msg
textAreaInput msg label inputValue =
    p [ class "control" ]
        [ textarea
            [ class "textarea"
            , onInput msg
            , placeholder label
            , value inputValue
            ]
            []
        ]


button : String -> msg -> Html msg
button label msg =
    div
        [ class "button is-primary"
        , onClick msg
        ]
        [ text label ]


renderCreateWidget : Category -> Model -> Html Msg
renderCreateWidget category model =
    div []
        [ textInput (SetTitleInput category.id) "Title" category.titleInput
        , textAreaInput (SetContentInput category.id) "Content" category.contentInput
        , button "Add"
            (AddWidget
                (Widget model.lastId category.id category.titleInput category.contentInput)
            )
        ]


renderCategoryWidgets : Int -> Model -> Html Msg
renderCategoryWidgets category model =
    ul []
        (model.widgets
            |> List.filter (\widget -> widget.category == category)
            |> List.map renderWidget
        )


renderWidgetGroup : Model -> Category -> Html Msg
renderWidgetGroup model category =
    div [ class "column" ]
        [ h3 [] [ text category.label ]
        , renderCreateWidget category model
        , renderCategoryWidgets category.id model
        ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ textInput SetCategoryInput "New Category" model.categoryInput
            , button "New Category" (AddCategory (Category model.lastId model.categoryInput "" ""))
            ]
        , div [ class "columns" ]
            (model.categories
                |> List.map (renderWidgetGroup model)
            )
        ]
