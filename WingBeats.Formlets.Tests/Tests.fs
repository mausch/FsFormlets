// Learn more about F# at http://fsharp.net

module Tests

open Xunit
open Formlets
open WingBeats
open WingBeats.Xml
open WingBeats.Xhtml
open WingBeats.Formlets

let e = XhtmlElement()
let f = e.Formlets
let s = e.Shortcut

let internal css = s.Stylesheet "all"

let internal form httpMethod action =
    e.Form ["action",action; "method",httpMethod]

let internal formGet x = form "get" x
let internal formPost x = form "post" x

let layout (head: #seq<Xml.Node>) (body: #seq<Xml.Node>) = 
    e.Html [
        e.Head [
            yield css "/Content/css/main.css"
            yield! head
        ]
        e.Body body
    ]

[<Fact>]
let ``first``() =
    let formlet = f.TextBox("a default value", ["class","nice"])
    let template form = 
        layout [] [
            formPost "actionUrl" [ yield!!+form ]
        ]
    let html = renderToXml formlet |> template |> Renderer.RenderToString
    Assert.Equal("<html xmlns=\"http://www.w3.org/1999/xhtml\"><head><link type=\"text/css\" rel=\"stylesheet\" media=\"all\" href=\"/Content/css/main.css\" /></head><body><form action=\"actionUrl\" method=\"post\"><input name=\"f0\" value=\"a default value\" class=\"nice\" /></form></body></html>", html)

[<Fact>]
let ``render error form``() =
    let formlet = f.TextBox("a default value", ["class","nice"])
    let formlet = formlet |> Validate.isInt |> map int
    let template form =
        e.Html [ yield!!+form ]
    let env = EnvDict.fromValueSeq ["f0","abc"]
    let errorForm,_,_ = run formlet env
    let html = template errorForm |> Renderer.RenderToString
    Assert.Equal("<html xmlns=\"http://www.w3.org/1999/xhtml\"><span class=\"errorinput\"><input name=\"f0\" value=\"abc\" class=\"nice\" /></span><span class=\"error\">abc is not a valid number</span></html>", html)