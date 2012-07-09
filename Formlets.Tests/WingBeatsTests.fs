module Formlets.Tests.WingBeats

open Fuchu
open Formlets.XmlWriter
open Formlets
open Formlets.Helpers
open WingBeats
open WingBeats.Xml
open WingBeats.Xhtml
open WingBeats.Formlets
open System.Xml.Linq

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

open Formlets.Tests.TestHelpers

[<Tests>]
let tests = 
    testList "WingBeats integration" [
        testCase "compare xnodes with different attribute order" <| fun _ ->
            let x = XNode.Parse "<input name='f0' value='abc' type='number' maxlength='4' required='required' class='nice' />"
            let y = XNode.Parse "<input type='number' maxlength='4' required='required' name='f0' value='abc' class='nice' />"
            Assert.XmlEqual(x,y)

        testCase "simple" <| fun _ ->
            let formlet = f.Text("a default value", ["class","nice"])
            let template form = 
                layout [] [
                    formPost "actionUrl" [ yield!!+form ]
                ]
            let html = renderToXml formlet |> template |> Renderer.RenderToString
            let expected = @"<html xmlns='http://www.w3.org/1999/xhtml'>
            <head>
                <link type='text/css' rel='stylesheet' media='all' href='/Content/css/main.css' />
            </head>
            <body>
                <form action='actionUrl' method='post'>
                    <input name='f0' value='a default value' class='nice' />
                </form>
            </body>
            </html>"
            Assert.XmlEqual(expected, html)

        testCase "render error form" <| fun _ ->
            let formlet = f.Text("a default value", ["class","nice"])
            let formlet = formlet |> Validate.Default.Int
            let template form =
                e.Html [ yield!!+form ]
            let env = EnvDict.fromValueSeq ["f0","abc"]
            let { Form = errorForm } = run formlet env
            let html = template errorForm |> Renderer.RenderToString
            let expected = @"<html xmlns='http://www.w3.org/1999/xhtml'>
                <span class='errorinput'>
                    <input name='f0' value='abc' class='nice' />
                </span>
                <span class='error'>abc is not a valid number</span>
            </html>"
            Assert.XmlEqual(expected, html)

        testCase "combine with wingbeats" <| fun _ ->
            let formlet =
                let id = "abc"
                s.Label id "a label" +> f.Text("a default value", ["id",id])
                <+ e.Br()
            let html = render formlet
            let expected = @"<label for='abc'>a label</label>
            <input name='f0' value='a default value' id='abc' />
            <br />"
            Assert.XmlEqual(expected, html)

        testCase "float render" <| fun _ ->
            let formlet = f.Float(required = true, maxlength = 4, attributes = ["class","nice"])
            let html = render formlet
            let expected = "<input type='number' maxlength='4' required='required' name='f0' value='' class='nice' />"
            Assert.XmlEqual(expected, html)

        testCase "float run failure" <| fun _ ->
            let formlet = f.Float(required = true, maxlength = 4, attributes = ["class","nice"])
            let env = EnvDict.fromValueSeq ["f0","abc"]
            match run formlet env with
            | Failure(errorForm, _) -> 
                let expected = @"<span class='errorinput'>
                <input name='f0' value='abc' type='number' maxlength='4' required='required' class='nice' />
                </span>
                <span class='error'>Invalid value</span>"
                Assert.XmlEqual(expected, errorForm)
            | _ -> failtest "Formlet should not have succeeded"

        testCase "int doesn't accept float" <| fun _ ->
            let formlet = f.Int()
            let env = EnvDict.fromValueSeq ["f0","1.3"]
            let html = render formlet
            //printfn "%s" html
            match run formlet env with
            | Failure(errorForm, _) ->
                let expected = @"<span class='errorinput'>
                <input name='f0' value='1.3' type='number' />
                </span>
                <span class='error'>1.3 is not a valid number</span>"
                Assert.XmlEqual(expected, errorForm)
            | _ -> failtest "Formlet should not have succeeded"

        testCase "int failure with range" <| fun _ ->
            let formlet = f.Int(min = 5, max = 10)
            let env = EnvDict.fromValueSeq ["f0","3"]
            match run formlet env with
            | Failure(errorForm, _) ->
                let expected = @"<span class='errorinput'>
                <input min='5' max='10' name='f0' value='3' type='number' />
                </span>
                <span class='error'>Value must be between 5 and 10</span>"
                Assert.XmlEqual(expected, errorForm)
            | _ -> failtest "Formlet should not have succeeded"

        testCase "int with validation error" <| fun _ ->
            let formlet = f.Int(min = 18, max = 100)
            let env = EnvDict.fromValueSeq ["f0", "abc"]
            match run formlet env with
            | Success v -> ()
            | Failure(errorForm, errorList) ->
                let expected = @"<span class='errorinput'>
                <input name='f0' value='' type='number' max='100' min='18' />
                </span>
                <span class='error'> is not a valid number</span>"
                Assert.XmlEqual(expected, errorForm)

        testCase "two formlets with validators and errors" <| fun _ ->
            let formlet = 
                let age = f.Int(min = 18, max = 100) // |> f.Validate.Required
                let name = f.Text(maxlength = 25) |> f.Validate.Required
                div [] (pair age name)
            let env = EnvDict.fromValueSeq ["f0",""; "f1",""]
            match run formlet env with
            | Success v -> ()
            | Failure(errorForm, errorList) -> 
                let expected = @"<div>
                <span class='errorinput'>
                    <input required='required' max='100' min='18' type='number' name='f0' value='3' />
                </span>
                <span class='error'>Value must be between 18 and 100</span>
                <span class='errorinput'>
                    <input required='required' maxlength='25' name='f1' value='' />
                </span>
                <span class='error'>Required field</span>
                </div>"
                Assert.XmlEqual(expected, errorForm)
    ]
