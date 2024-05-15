module [
    Session,
    Todo,
    User,
]

Session : {
    id : I64,
    user : [Guest, LoggedIn Str],
}

Todo : {
    id : I64,
    task : Str,
    status : Str,
}

User : {
    id : I64,
    email : Str,
    name : Str,
}
