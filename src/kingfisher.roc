app [init_model, update_model, handle_request!, Model] {
    webserver: platform "../../kingfisher/platform/main.roc",
    html: "https://github.com/Hasnep/roc-html/releases/download/v0.6.0/IOyNfA4U_bCVBihrs95US9Tf5PGAWh3qvrBN4DRbK5c.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.11.0/z45Wzc-J39TLNweQUoLw3IGZtkQiEN3lTBv3BXErRjQ.tar.br",
}

import webserver.Http exposing [Request, Response]
import html.Html
import Models.Session exposing [Session, User]
import Models.Todo exposing [Todo]
import json.Json
import "site.css" as stylesFile : List U8
import "site.js" as siteFile : List U8
import "../vendor/bootstrap.bundle-5-3-2.min.js" as bootstrapJSFile : List U8
import "../vendor/bootstrap-5-3-2.min.css" as bootsrapCSSFile : List U8
import "../vendor/htmx-2-0-3.min.js" as htmxJSFile : List U8
import Views.Home
import Views.Login
import Views.Register
import Views.Todo
import Views.UserList
import Url

Model : {
    sessions : List Session,
    users : List User,
    todos : List Todo,
}

init_model = {
    sessions: [],
    users: [],
    todos: [],
}

update_model : Model, List (List U8) -> Result Model _
update_model = \old_model, event_list ->
    event_list
    |> List.walkTry old_model \model, encoded_event ->
        when Decode.fromBytes encoded_event Json.utf8 is
            Ok typer ->
                when typer.type is
                    "new_user" ->
                        encoded_event
                        |> Decode.fromBytes Json.utf8

                        |> Result.map \u ->
                            { model & users: List.append model.users u.user }
                        |> Result.mapErr \_ -> InvalidCreateUserEvent

                    "login" ->
                        encoded_event
                        |> Decode.fromBytes Json.utf8
                        |> Result.map \{ session } ->
                            { model & sessions: List.append model.sessions { id: session.id, user: LoggedIn session.user } }
                        |> Result.mapErr \_ -> InvalidLoginEvent

                    _ ->
                        # Unknown event. There is no way to log this :(
                        model |> Ok

            Err _ -> Err EventWithoutType

handle_request! : Request, Model => Result Response _
handle_request! = \req, model ->

    # TODO
    # logRequest! req # Log the date, time, method, and url to stdout

    session = parseSession req model.sessions

    urlSegments =
        req.url
        |> Url.fromStr
        |> Url.path
        |> Str.splitOn "/"
        |> List.dropFirst 1

    when (req.method, urlSegments) is
        (Get, [""]) -> Views.Home.page { session } |> htmlResponse
        (Get, ["robots.txt"]) -> staticReponse robotsTxt
        (Get, ["styles.css"]) -> staticContentTypeReponse stylesFile "text/css"
        (Get, ["site.js"]) -> staticReponse siteFile
        (Get, ["bootstrap.bundle.min.js"]) -> staticReponse bootstrapJSFile
        (Get, ["bootstrap.min.css"]) -> staticContentTypeReponse bootsrapCSSFile "text/css"
        (Get, ["htmx.min.js"]) -> staticReponse htmxJSFile
        (Get, ["register"]) ->
            Views.Register.page { user: Fresh, email: Valid } |> htmlResponse

        (Post save_event!, ["register"]) ->
            params = parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})
            usernameResult = Dict.get params "user"
            emailResult = Dict.get params "email"
            when (usernameResult, emailResult) is
                (Ok username, Ok email) ->
                    when List.findFirst model.users (\u -> u.name == username) is
                        Ok _user -> Views.Register.page { user: UserAlreadyExists username, email: Valid } |> htmlResponse
                        Err _ ->
                            newUser = {
                                id: List.len model.users |> Num.toI64,
                                email: email,
                                name: username,
                            }

                            Encode.toBytes
                                {
                                    type: "new_user",
                                    user: newUser,
                                }
                                Json.utf8
                            |> save_event!

                            redirect "/login"

                _ ->
                    Views.Register.page { user: UserNotProvided, email: NotProvided } |> htmlResponse

        (Get, ["login"]) ->
            Views.Login.page { session, user: Fresh } |> htmlResponse

        (Post save_event!, ["login"]) ->
            params = parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})

            when Dict.get params "user" is
                Err _ -> Views.Login.page { session, user: UserNotProvided } |> htmlResponse
                Ok username ->
                    when List.findFirst model.users (\u -> u.name == username) is
                        Ok _user ->
                            sessionID = List.len model.sessions |> Num.toI64

                            Encode.toBytes
                                {
                                    type: "login",
                                    session: { id: sessionID, user: username },
                                }
                                Json.utf8
                            |> save_event!

                            Ok {
                                status: 303,
                                headers: [
                                    { name: "Set-Cookie", value: "$(cookieName)=$(Num.toStr sessionID)" },
                                    { name: "Location", value: "/" },
                                ],
                                body: [],
                            }

                        Err NotFound -> Views.Login.page { session, user: UserNotFound username } |> htmlResponse

        (Get, ["task", "new"]) -> redirect "/task"
        (Get, ["task", "list"]) ->
            Views.Todo.listTodoView { todos: model.todos, filterQuery: "" } |> htmlResponse

        (Get, ["task"]) ->
            Views.Todo.page { todos: model.todos, filterQuery: "", session } |> htmlResponse

        # (Get, ["treeview"]) ->
        #     nodes <- Sql.Todo.tree { path: dbPath, userId: 1 } |> Task.await
        #     Views.TreeView.page { session, nodes } |> htmlResponse |> Task.ok
        (Get, ["user"]) ->
            Views.UserList.page { users: model.users, session } |> htmlResponse

        _ -> handleErr (URLNotFound req.url)

# handleWriteRequest : Request, Model -> (Response, Model)
# handleWriteRequest = \req, model ->
#    session = parseSession req model.sessions

#    urlSegments =
#        req.url
#        |> Url.fromStr
#        |> Url.path
#        |> Str.splitOn "/"
#        |> List.dropFirst 1

#    when (req.method, urlSegments) is

#        (Post, ["logout"]) ->
#            newmodel = { model & sessions: List.update model.sessions (session.id |> Num.toU64) (\s -> { s & user: Guest }) }

#            (
#                {
#                    status: 303,
#                    headers: [
#                        { name: "Set-Cookie", value: Str.toUtf8 "$(cookieName)=deleted;  path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT" },
#                        { name: "Location", value: Str.toUtf8 "/" },
#                    ],
#                    body: [],
#                },
#                newmodel,
#            )

#        (Post, ["task", taskIdStr, "delete"]) ->
#            newModel =
#                when Str.toI64 taskIdStr |> Result.try \id -> findIndex model.todos id is
#                    Ok index ->
#                        { model & todos: List.dropAt model.todos index }

#                    Err _ -> model

#            (Views.Todo.listTodoView { todos: newModel.todos, filterQuery: "" } |> htmlResponse, newModel)

#        (Post, ["task", "search"]) ->
#            params = parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})
#            filterQuery = Dict.get params "filterTasks" |> Result.withDefault ""
#            todos = model.todos |> List.keepIf \todo -> Str.contains todo.task filterQuery
#            (Views.Todo.listTodoView { todos, filterQuery } |> htmlResponse, model)

#        (Post, ["task", "new"]) ->
#            when parseTodo req.body is
#                Ok newTodo ->
#                    nextID = (List.map model.todos \todo -> todo.id) |> List.max |> Result.withDefault 0 |> Num.add 1
#                    newModel = { model & todos: List.append model.todos { newTodo & id: nextID } }
#                    (redirect "/task", newModel)

#                Err err -> (handleErr err, model)

#        (Put, ["task", taskIdStr, "complete"]) ->
#            newModel =
#                when Str.toI64 taskIdStr |> Result.try \id -> findIndex model.todos id is
#                    Ok id ->
#                        { model & todos: List.update model.todos id \old -> { old & status: "Completed" } }

#                    Err _ -> model
#            (triggerResponse "todosUpdated", newModel)

#        (Put, ["task", taskIdStr, "in-progress"]) ->
#            newModel =
#                when Str.toI64 taskIdStr |> Result.try \id -> findIndex model.todos id is
#                    Ok id ->
#                        { model & todos: List.update model.todos id \old -> { old & status: "In-Progress" } }

#                    Err _ -> model
#            (triggerResponse "todosUpdated", newModel)

#        _ -> (handleErr (URLNotFound req.url), model)

findIndex = \list, id ->
    List.findFirstIndex list (\e -> e.id == id)

# parseTodo : List U8 -> Result Todo [UnableToParseBodyTask _]_
# parseTodo = \bytes ->
#    dict = parseFormUrlEncoded bytes |> Result.withDefault (Dict.empty {})

#    task <-
#        Dict.get dict "task"
#        |> Result.mapErr \_ -> UnableToParseBodyTask bytes
#        |> Result.try

#    status <-
#        Dict.get dict "status"
#        |> Result.mapErr \_ -> UnableToParseBodyTask bytes
#        |> Result.try

#    Ok { id: 0, task, status }

triggerResponse : Str -> Result Response _
triggerResponse = \trigger ->
    Ok {
        status: 200,
        headers: [
            { name: "HX-Trigger", value: trigger },
        ],
        body: [],
    }

staticReponse : List U8 -> Result Response _
staticReponse = \bytes ->
    Ok {
        status: 200,
        headers: [
            { name: "Cache-Control", value: "max-age=120" },
        ],
        body: bytes,
    }

staticContentTypeReponse = \bytes, content_type ->
    Ok {
        status: 200,
        headers: [
            { name: "Cache-Control", value: "max-age=120" },
            { name: "Content-Type", value: content_type },
        ],
        body: bytes,
    }

htmlResponse : Html.Node -> Result Response _
htmlResponse = \node ->
    Ok {
        status: 200,
        headers: [
            { name: "Content-Type", value: "text/html; charset=utf-8" },
        ],
        body: Str.toUtf8 (Html.render node),
    }

redirect : Str -> Result Response _
redirect = \next ->
    Ok {
        status: 303,
        headers: [
            { name: "Location", value: next },
        ],
        body: [],
    }

handleErr : _ -> Result Response _
handleErr = \err ->

    code =
        when err is
            URLNotFound _url -> 404
            _ -> 500

    Ok {
        status: code,
        headers: [],
        body: [],
    }

robotsTxt : List U8
robotsTxt =
    """
    User-agent: *
    Disallow: /
    """
    |> Str.toUtf8

anonymousSession = {
    id: 0 |> Num.toI64,
    user: Guest,
}

cookieName = "sessionId"

parseSession : Request, List Session -> Session
parseSession = \req, sessions ->
    mayID =
        req.headers
        |> List.findFirst \reqHeader -> reqHeader.name == "Cookie"
        |> Result.mapErr \_ -> CookieHeaderNotFound
        |> Result.try \reqHeader ->
            reqHeader.value
            |> Str.splitOn ";"
            |> List.findFirst \v -> v |> Str.trim |> Str.startsWith "$(cookieName)="
            |> Result.mapErr \_ -> CookieNameNotFound cookieName reqHeader.value
            |> Result.try \w ->
                w
                |> Str.splitOn "="
                |> List.get 1
                |> Result.mapErr \_ -> NoEqualFound
                |> Result.try \v ->
                    v
                    |> Str.toU64
                    |> Result.mapErr \_ -> ValueNoInt v

    when mayID is
        Ok id -> List.get sessions id |> Result.withDefault anonymousSession
        Err _ -> anonymousSession

# From basic-webserver 0.10
parseFormUrlEncoded : List U8 -> Result (Dict Str Str) [BadUtf8]
parseFormUrlEncoded = \bytes ->

    chainUtf8 = \bytesList, tryFun -> Str.fromUtf8 bytesList |> mapUtf8Err |> Result.try tryFun

    # simplify `BadUtf8 Utf8ByteProblem ...` error
    mapUtf8Err = \err -> err |> Result.mapErr \_ -> BadUtf8

    parse = \bytesRemaining, state, key, chomped, dict ->
        tail = List.dropFirst bytesRemaining 1

        when bytesRemaining is
            [] if List.isEmpty chomped -> dict |> Ok
            [] ->
                # chomped last value
                key
                |> chainUtf8 \keyStr ->
                    chomped
                    |> chainUtf8 \valueStr ->
                        Dict.insert dict keyStr valueStr |> Ok

            ['=', ..] -> parse tail ParsingValue chomped [] dict # put chomped into key
            ['&', ..] ->
                key
                |> chainUtf8 \keyStr ->
                    chomped
                    |> chainUtf8 \valueStr ->
                        parse tail ParsingKey [] [] (Dict.insert dict keyStr valueStr)

            ['%', secondByte, thirdByte, ..] ->
                hex = Num.toU8 (hexBytesToU32 [secondByte, thirdByte])

                parse (List.dropFirst tail 2) state key (List.append chomped hex) dict

            [firstByte, ..] -> parse tail state key (List.append chomped firstByte) dict

    parse bytes ParsingKey [] [] (Dict.empty {})

hexBytesToU32 : List U8 -> U32
hexBytesToU32 = \bytes ->
    bytes
    |> List.reverse
    |> List.walkWithIndex 0 \accum, byte, i -> accum + (Num.powInt 16 (Num.toU32 i)) * (hexToDec byte)
    |> Num.toU32

hexToDec : U8 -> U32
hexToDec = \byte ->
    when byte is
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'A' -> 10
        'B' -> 11
        'C' -> 12
        'D' -> 13
        'E' -> 14
        'F' -> 15
        _ -> crash "Impossible error: the `when` block I'm in should have matched before reaching the catch-all `_`."
