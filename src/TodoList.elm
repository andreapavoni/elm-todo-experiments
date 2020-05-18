module TodoList exposing (main)

import Browser
import Html exposing (Html, button, div, h2, h3, input, label, text)
import Html.Attributes exposing (checked, classList, type_, value)
import Html.Events exposing (onClick, onInput)



-- MODEL


type alias Todo =
    { title : String
    , done : Bool
    , id : Int
    }


type alias Model =
    { todos : List Todo
    , newTodo : Todo
    }


emptyTodo : Todo
emptyTodo =
    Todo "" False 0


initModel : Model
initModel =
    Model [] emptyTodo



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h2 [] [ text "New Todo" ]
            , viewInput "Title" model.newTodo.title UpdateTitle
            , button [ onClick AddTodo ] [ text "Add" ]
            ]
        , div [] (List.map viewTodo model.todos)
        , button [ onClick ClearDone ] [ text "Clear done" ]
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
        [ h3 [ classList [ ( "red", todo.done ) ] ] [ text todo.title ]
        , input [ type_ "checkbox", checked todo.done ] []
        ]



-- UPDATE


type Msg
    = AddTodo
    | UpdateTitle String
    | ToggleDone Int
    | ClearDone


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            let
                newId =
                    List.length model.todos
            in
            { model | todos = List.append model.todos [ setTodoId newId model.newTodo ], newTodo = emptyTodo }

        UpdateTitle title ->
            { model | newTodo = setTodoTitle title model.newTodo }

        ToggleDone id ->
            { model | todos = List.map (updateToggleDone id) model.todos }

        ClearDone ->
            { model | todos = List.filter (\todo -> not todo.done) model.todos }


updateToggleDone : Int -> Todo -> Todo
updateToggleDone idToUpdate todo =
    if idToUpdate == todo.id then
        { todo | done = not todo.done }

    else
        todo



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = initModel, view = view, update = update }



-- HELPERS


setTodoId : Int -> Todo -> Todo
setTodoId id newTodo =
    { newTodo | id = id }


setTodoTitle : String -> Todo -> Todo
setTodoTitle title newTodo =
    { newTodo | title = title }
