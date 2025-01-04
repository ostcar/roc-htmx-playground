app [server, Model] {
    # webserver: platform "https://github.com/ostcar/kingfisher/releases/download/v0.0.3/e8Mu5IplmOnXPU9VgpTCT6kyB463gX-SDC2nnMfAq7M.tar.br",
    pf: platform "../../kingfisher/platform/main.roc",
    html: "https://github.com/Hasnep/roc-html/releases/download/v0.6.0/IOyNfA4U_bCVBihrs95US9Tf5PGAWh3qvrBN4DRbK5c.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.1.1/cPHdNPNh8bjOrlOgfSaGBJDz6VleQwsPdW0LJK6dbGQ.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.2/FH4N0Sw-JSFXJfG3j54VEDPtXOoN-6I9v_IA8S18IGk.tar.br",
}

import pf.Http exposing [Request, Response]
# import pf.Stdout
import pf.Stderr
# import pf.Utc
import ansi.Color
import Models.Session exposing [Session, User]
import Models.Todo exposing [Todo]
import json.Json
import "site.css" as stylesFile : List U8
import "site.js" as siteFile : List U8
import Views.Home
# import Views.Unauthorised
import Views.Login
import Views.Register
import Views.Todo
import Views.UserList

import Helpers exposing [respondHtml]

Model : {
    sessions : List Session,
    users : List User,
    todos : List Todo,
}

server = {
    updateModel,
    respond,
}

updateModel = \eventList, initOrModel ->
    initModel =
        when initOrModel is
            Init ->
                {
                    sessions: [],
                    users: [],
                    todos: [],
                }

            Existing m -> m

    List.walkTry
        eventList
        initModel
        \model, encodedEvent ->
            when Decode.fromBytes encodedEvent Json.utf8 is
                Ok typer ->
                    when typer.type is
                        "new-user" ->
                            # TODO: Why is this necessary?
                            userEvent : Result { user : User } _
                            userEvent = Decode.fromBytes encodedEvent Json.utf8

                            userEvent
                            |> Result.map \u ->
                                { model & users: List.append model.users u.user }
                            |> Result.mapErr \_ -> "Can not encode new-user event"

                        "login" ->
                            loginEvent : Result {userID: I64, sessionID: I64} _ 
                            loginEvent = Decode.fromBytes encodedEvent Json.utf8

                            loginEvent
                            |> Result.try \e -> 
                                List.findFirst model.users \u -> u.id == e.userID
                                |> Result.map \user ->
                                    session ={id: e.sessionID, user: LoggedIn user.name}
                                    {model & sessions: List.append model.sessions session}
                                |> Result.mapErr \_ -> Foo "invalid userid in event"
                            |> Result.mapErr \err -> 
                                eventStr = Str.fromUtf8 encodedEvent|> Result.withDefault "invalid utf8"
                                "can not decode event $(eventStr): $(Inspect.toStr err)"

                        "logout" ->
                            logoutEvent : Result {sessionID: I64} _
                            logoutEvent = Decode.fromBytes encodedEvent Json.utf8

                            logoutEvent
                            |> Result.try \e ->
                                List.findFirst model.sessions \s -> s.id == e.sessionID
                                |> Result.map \session -> 
                                    { model & sessions: List.update model.sessions (session.id |> Num.toU64) (\s -> { s & user: Guest }) }
                                |> Result.mapErr \_ -> Foo "invalid session id in event"
                            |> Result.mapErr \ err ->
                                "can not decode event"
                        
                        "task-delete" ->
                            event : Result {index: U64} _
                            event = Decode.fromBytes encodedEvent Json.utf8

                            event
                            |> Result.map \e ->
                                { model & todos: List.dropAt model.todos e.index }
                            |> Result.mapErr \ err ->
                                "can not decode event"

                        _ ->
                            Err "Unknown event $(typer.type)"

                Err err -> Err "can not encode event type: $(Inspect.toStr err)"

respond : Request, Model -> Task Response []
respond = \req, model ->
    Task.onErr (handleReq req model) \err ->
        when err is
            # BadRequest inner ->
            #     Stderr.line! (Inspect.toStr err)
            #     Task.ok {
            #         status: 400,
            #         headers: [],
            #         body: Str.toUtf8 (Inspect.toStr inner),
            #     }
            # Unauthorized ->
            #     Views.Unauthorised.page {} |> respondHtml []
            URLNotFound url -> respondCodeLogError (Str.joinWith ["404 NotFound" |> Color.fg Red, url] " ") 404
            # _ -> respondCodeLogError (Str.joinWith ["SERVER ERROR" |> Color.fg Red, Inspect.toStr err] " ") 500
            _ -> crash (Inspect.toStr err)

handleReq : Request, Model -> Task Response _
handleReq = \req, model ->
    # TODO logRequest

    session = parseSession req model.sessions

    urlSegments =
        req.url
        |> Str.split "/"
        |> List.dropFirst 1

    when (req.method, urlSegments) is
        (Get, [""]) -> Views.Home.page { session } |> respondHtml []
        (Get, ["robots.txt"]) -> respondStatic robotsTxt
        (Get, ["styles.css"]) -> respondStatic stylesFile
        (Get, ["site.js"]) -> respondStatic siteFile
        (Get, ["register"]) ->
            Views.Register.page { user: Fresh, email: Valid } |> respondHtml []

        (Post saveEvent, ["register"]) ->
            params = Http.parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})

            when (Dict.get params "user", Dict.get params "email") is
                (Ok username, Ok email) ->
                    when List.findFirst model.users (\u -> u.name == username) is
                        Ok _user -> Views.Register.page { user: UserAlreadyExists username, email: Valid } |> respondHtml []
                        Err _ ->
                            newUser = {
                                id: List.len model.users |> Num.toI64,
                                email: email,
                                name: username,
                            }
                            event = Encode.toBytes
                                {
                                    type: "new-user",
                                    user: newUser,
                                }
                                Json.utf8

                            saveEvent event
                                |> Task.mapErr! \err -> ServeErr err

                            Helpers.respondRedirect "/login" ## Redirect to login page after successful registration

                _ ->
                    Views.Register.page { user: UserNotProvided, email: NotProvided } |> respondHtml []

        (Get, ["login"]) ->
            Views.Login.page { session, user: Fresh } |> respondHtml []

        (Post saveEvent, ["login"]) ->
            params = Http.parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})

            when Dict.get params "user" is
                Err _ -> Views.Login.page { session, user: UserNotProvided } |> respondHtml []
                Ok username ->
                    when List.findFirst model.users (\u -> u.name == username) is
                        Ok user ->
                            # TODO: get a random session id from a Task
                            sessionID = List.len model.sessions |> Num.toI64
                            event = Encode.toBytes
                                {
                                    type: "login",
                                    sessionID: sessionID,
                                    userID: user.id,
                                }
                                Json.utf8

                            saveEvent event
                                |> Task.mapErr! \err -> ServeErr err

                            Task.ok {
                                status: 303,
                                headers: [
                                    { name: "Set-Cookie", value:  "$(cookieName)=$(Num.toStr sessionID)" },
                                    { name: "Location", value:  "/" },
                                ],
                                body: [],
                            }

                        Err NotFound -> Views.Login.page { session, user: UserNotFound username } |> respondHtml []

        (Post saveEvent, ["logout"]) ->
            event = Encode.toBytes
                {
                    type: "logout",
                    sessionID: session.id,
                }
                Json.utf8

            saveEvent event
                |> Task.mapErr! \err -> ServeErr err
                            
            Task.ok {
                status: 303,
                headers: [
                    { name: "Set-Cookie", value:  "$(cookieName)=deleted;  path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT" },
                    { name: "Location", value:  "/" },
                ],
                body: [],
            }

        (Get, ["task", "new"]) -> Helpers.respondRedirect "/task"

        (Post saveEvent, ["task", idStr, "delete"]) ->
            when Str.toI64 idStr |> Result.try \id -> findIndex model.todos id is
                Ok index ->
                    event = Encode.toBytes
                        {
                            type: "task-delete",
                            index: index,
                        }
                        Json.utf8
        
                    saveEvent event
                        |> Task.mapErr! \err -> ServeErr err

                    when updateModel [event] (Existing model) is
                        Err _ -> Task.err (ServeErr "can not update model after update")
                        Ok newModel -> Views.Todo.listTodoView { todos: newModel.todos, filterQuery: "" } |> respondHtml []

                Err _ -> 
                    Views.Todo.listTodoView { todos: model.todos, filterQuery: "" } |> respondHtml []

            

        (Post _, ["task", "search"]) ->
            params = parseFormUrlEncoded req.body |> Result.withDefault (Dict.empty {})

            filterQuery = Dict.get params "filterTasks" |> Result.withDefault ""

            tasks = model.todos |> List.keepIf \todo -> Str.contains todo.task filterQuery

            Views.Todo.listTodoView { todos: tasks, filterQuery } |> respondHtml []

        (Post saveEvent, ["task", "new"]) ->
            newTodoResult = parseTodo req.body

            when newTodoResult is
                Ok newTodo ->
                    nextID = (List.map model.todos \todo -> todo.id) |> List.max |> Result.withDefault 0 |> Num.add 1
                    newModel = { model & todos: List.append model.todos { newTodo & id: nextID } }
                    # TODO Save new Task
                    Helpers.respondRedirect "/task"

                Err err -> crash "fix task new"

        (Post saveEvent, ["task", taskIdStr, "complete"]) ->
            newModel =
                when Str.toI64 taskIdStr |> Result.try \id -> findIndex model.todos id is
                    Ok id ->
                        { model & todos: List.update model.todos id \old -> { old & status: "Completed" } }

                    Err _ -> model
            # TODO Update model
            respondHxTrigger "todosUpdated"

        (Post saveEvent, ["task", taskIdStr, "in-progress"]) ->
            newModel =
                when Str.toI64 taskIdStr |> Result.try \id -> findIndex model.todos id is
                    Ok id ->
                        { model & todos: List.update model.todos id \old -> { old & status: "In-Progress" } }

                    Err _ -> model

            # TODO Update model
            respondHxTrigger "todosUpdated"

        (Get, ["task", "list"]) ->
            tasks = model.todos

            Views.Todo.listTodoView { todos: tasks, filterQuery: "" } |> respondHtml []

        (Get, ["task"]) ->
            tasks = model.todos

            Views.Todo.page { todos: tasks, filterQuery: "", session } |> respondHtml []

        (Get, ["user"]) ->
            users = model.users

            Views.UserList.page { users, session } |> respondHtml []

        _ -> Task.err (URLNotFound req.url)

findIndex = \list, id ->
    List.findFirstIndex list (\e -> e.id == id)

parseTodo : List U8 -> Result Todo _
parseTodo = \bytes ->
    dict = Http.parseFormUrlEncoded bytes |> Result.withDefault (Dict.empty {})

    when (Dict.get dict "task", Dict.get dict "status") is
        (Ok task, Ok status) -> Ok { id: 0, task, status }
        _ -> Err (UnableToParseBodyTask bytes)

respondHxTrigger : Str -> Task Response []_
respondHxTrigger = \trigger ->
    Task.ok {
        status: 200,
        headers: [
            { name: "HX-Trigger", value: trigger },
        ],
        body: [],
    }

respondStatic : List U8 -> Task Response []_
respondStatic = \bytes ->
    Task.ok {
        status: 200,
        headers: [
            { name: "Cache-Control", value: "max-age=120" },
        ],
        body: bytes,
    }

respondCodeLogError = \msg, code ->
    # TODO: why does it not work with this line?
    # Stderr.line! msg
    Task.ok! {
        status: code,
        headers: [],
        body: [],
    }

# logRequest : Request -> Task {} *
# logRequest = \req ->
#     date = "Why does this not work??" # TODO Utc.now |> Task.map! Utc.toIso8601Str
#     method = Http.methodToStr req.method
#     url = req.url
#     body = req.body |> Str.fromUtf8 |> Result.withDefault "<invalid utf8 body>"
#     Stdout.line! "$(date) $(method) $(url) $(body)"
#     |> Task.onErr \_ -> crash "this should not happen"

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
            |> Str.split ";"
            |> List.findFirst \v -> v |> Str.trim |> Str.startsWith "$(cookieName)="
            |> Result.mapErr \_ -> CookieNameNotFound cookieName reqHeader.value
            |> Result.try \w ->
                w
                |> Str.split "="
                |> List.get 1
                |> Result.mapErr \_ -> NoEqualFound
                |> Result.try \v ->
                    v
                    |> Str.toU64
                    |> Result.mapErr \_ -> ValueNoInt v

    when mayID is
        Ok id -> List.get sessions id |> Result.withDefault anonymousSession
        Err _ -> anonymousSession

# From basic-webserver
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
                keyStr <- key |> chainUtf8
                valueStr <- chomped |> chainUtf8

                Dict.insert dict keyStr valueStr |> Ok

            ['=', ..] -> parse tail ParsingValue chomped [] dict # put chomped into key
            ['&', ..] ->
                keyStr <- key |> chainUtf8
                valueStr <- chomped |> chainUtf8

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
