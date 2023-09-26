module Main

open Flutter.Foundation
open Flutter.Widgets
open Flutter.Material
open Flutter.Services

open Elmish
open Elmish.Flutter

module App =
    type Todo = { Id: int; Title: string }
    type Model = { Todos: Todo list }

    type Msg =
        | AddTodo of title: string
        | DeleteTodo of id: int
        | EditTodo of id: int * text: string

    let init () = { Todos = [] }, Cmd.none

    let update msg model =
        match msg with
        | AddTodo title ->
            let newTodo =
                { Id =
                    match model.Todos with
                    | [] -> 0
                    | _ -> model.Todos |> List.map (fun t -> t.Id) |> List.max |> (+) 1

                  Title = title }

            { model with Todos = newTodo :: model.Todos }, Cmd.none

        | DeleteTodo id ->
            let todos = model.Todos |> List.filter (fun t -> t.Id <> id)
            { model with Todos = todos }, Cmd.none

        | EditTodo (id, title) ->
            let todos =
                model.Todos
                |> List.map (fun t -> if t.Id = id then { t with Title = title } else t)

            { model with Todos = todos }, Cmd.none

    let displayDialog context text onSubmitted =
        showDialog (
            context = context,
            builder =
                (fun context ->
                    AlertDialog(
                        title = Text "Add a task to your list",
                        content =
                            TextFormField(
                                initialValue = text,
                                autofocus = true,
                                textInputAction = TextInputAction.``done``,
                                onFieldSubmitted =
                                    (fun text ->
                                        Navigator.``of``(context).pop ()
                                        text |> onSubmitted),
                                decoration = InputDecoration(hintText = "Enter task here")
                            ),
                        actions =
                            [| MaterialButton(
                                   child = Text "Cancel",
                                   onPressed = fun () -> Navigator.``of``(context).pop ()
                               ) |]
                    ))
        )

    let buildTodos model dispatch context =
        let todos: Widget[] =
            [| for t in model.Todos do
                   ListTile(
                       title =
                           Row
                               [| Expanded(Text(t.Title))
                                  IconButton(
                                      icon = Icon(Icons.edit),
                                      onPressed =
                                          fun () ->
                                              displayDialog context t.Title (fun text ->
                                                  EditTodo(t.Id, text) |> dispatch)
                                              |> ignore
                                  )
                                  IconButton(
                                      icon = Icon(Icons.delete),
                                      onPressed = fun () -> DeleteTodo t.Id |> dispatch
                                  ) |]
                   ) |]

        ListView(children = todos)

    let view (model: Model) (dispatch: Msg -> unit) context : Widget =
        Scaffold(
            appBar = AppBar(title = Text "F# Flutter"),
            body =
                Row
                    [| Expanded(flex = 1, child = SizedBox.shrink ())
                       Expanded(flex = 2, child = buildTodos model dispatch context)
                       Expanded(flex = 1, child = SizedBox.shrink ()) |],
            floatingActionButton =
                FloatingActionButton(
                    child = Icon Icons.task,
                    tooltip = "Add Item",
                    onPressed = fun () -> displayDialog context "" (AddTodo >> dispatch) |> ignore
                )
        )

open App

type MyApp(?key: Key) =
    inherit StatelessWidget(?key = key)

    override _.build(context) =
        MaterialApp(title = "Welcome to Flutter", home = ElmishWidget.From(init, update, view))

let main () = MyApp() |> runApp
