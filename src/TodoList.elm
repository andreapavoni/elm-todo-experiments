module TodoList exposing (main)

import Browser
import Html exposing (Html, button, div, h2, h3, input, label, text)
import Html.Attributes exposing (checked, classList, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, map3, string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


type alias Model =
    { todos : List Todo
    , currentTodo : Todo
    }



-- type ApiStatus
--     = Loading
--     | Failure
--     | Success String


emptyTodo : Todo
emptyTodo =
    Todo 0 "" False


initialModel : Model
initialModel =
    Model [] emptyTodo


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getTodos )



-- UPDATE


type Msg
    = AddTodo
    | UpdateTitle String
    | ToggleDone Int
    | ClearDone
    | LoadedFromApi (Result Http.Error (List Todo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            let
                newId =
                    List.length model.todos
            in
            ( { model | todos = List.append model.todos [ setTodoId newId model.currentTodo ], currentTodo = emptyTodo }, Cmd.none )

        UpdateTitle title ->
            ( { model | currentTodo = setTodoTitle title model.currentTodo }, Cmd.none )

        ToggleDone id ->
            ( { model | todos = List.map (updateToggleDone id) model.todos }, Cmd.none )

        ClearDone ->
            ( { model | todos = List.filter (\todo -> not todo.completed) model.todos }, Cmd.none )

        LoadedFromApi result ->
            case result of
                Ok todos ->
                    ( { model | todos = todos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateToggleDone : Int -> Todo -> Todo
updateToggleDone idToUpdate todo =
    if idToUpdate == todo.id then
        { todo | completed = not todo.completed }

    else
        todo


setTodoId : Int -> Todo -> Todo
setTodoId id todo =
    { todo | id = id }


setTodoTitle : String -> Todo -> Todo
setTodoTitle title todo =
    { todo | title = title }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h2 [] [ text "New Todo" ]
            , viewInput "Title" model.currentTodo.title UpdateTitle
            , button [ onClick AddTodo ] [ text "Add" ]
            ]
        , div [] (List.map viewTodo model.todos)
        , button [ onClick ClearDone ] [ text "Clear completed" ]
        ]


viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput labelText val onInputMsg =
    div []
        [ label []
            [ text labelText
            , input [ value val, onInput onInputMsg ] []
            ]
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    div [ onClick (ToggleDone todo.id) ]
        [ h3 [ classList [ ( "red", todo.completed ) ] ] [ text todo.title ]
        , input [ type_ "checkbox", checked todo.completed ] []
        ]



-- HTTP


getTodos : Cmd Msg
getTodos =
    Http.get
        { url = "http://localhost:3000/todos"
        , expect = Http.expectJson LoadedFromApi todoDecoder
        }


todoDecoder : Decoder (List Todo)
todoDecoder =
    list
        (map3 Todo
            (field "id" int)
            (field "title" string)
            (field "completed" bool)
        )
