module Main exposing (main)

import Browser
import Html exposing ( Html, button, div, text, input, span, form )
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick, onInput, onSubmit, onCheck )

-- MAIN

main = Browser.sandbox {init=init, update=update, view=view}

-- MODEL

type alias Todo = 
  {todo: String
  , done: Bool}

type alias Model = 
  {todos: List Todo
  ,inTodo: String
  }

init : Model
init = {todos=[],inTodo=""}

-- UPDATE

type Msg = AddTodo
  | TodoChange String
  | ToggleTodo String Bool
update : Msg -> Model -> Model
update msg model = 
  case msg of
    AddTodo -> 
      if model.inTodo /= "" && List.all (\x -> x.todo /= model.inTodo) model.todos then 
        { model | todos = model.todos ++ [{todo=model.inTodo, done=False}], inTodo = ""}
      else 
        {model | inTodo=""}

    TodoChange todo -> {model | inTodo = todo}

    ToggleTodo todo done -> {model | todos = List.filter (\x -> x.todo /= todo) model.todos ++ [{todo=todo, done=not done}]}

-- VIEW

todoItem : Todo -> Html Msg
todoItem todo = 
  div []
      [
        input [ type_ "checkbox", checked todo.done, onClick(ToggleTodo todo.todo todo.done) ] []
        ,(if todo.done then 
            span [style "text-decoration" "line-through"] [text todo.todo]
          else
            span [] [text todo.todo]
          )
      ]

view : Model -> Html Msg
view model = 
  div [style "padding" "20px"]
    [ Html.form [onSubmit AddTodo, style "margin-bottom" "15px"] [
        input [ type_ "text", value model.inTodo, autofocus True, placeholder "What todo?", onInput TodoChange] []
        ,button [] [text "+"]
      ]
    ,div [] (List.map todoItem model.todos)
    ]
