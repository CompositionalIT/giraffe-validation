open Giraffe
open Microsoft.AspNetCore.Http
open Saturn


let routes = router {
    post "/bar" ModelA.handler
    post "/foo" ModelB.handler }

let app =
    application {
        url "http://localhost:8080"
        use_router routes
    }

run app
