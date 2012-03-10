module Tests

open TestHelpers
open Xunit
open Formlets.XmlWriter
open Formlets
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

[<Fact>]
let ``compare xnodes with different attribute order``() =
    let x = XNode.Parse "<input name='f0' value='abc' type='number' maxlength='4' required='required' class='nice' />"
    let x = x.[0]
    let y = XNode.Parse "<input type='number' maxlength='4' required='required' name='f0' value='abc' class='nice' />"
    let y = y.[0]
    Assert.True (x =. y)

[<Fact>]
let ``first``() =
    let formlet = f.Text("a default value", ["class","nice"])
    let template form = 
        layout [] [
            formPost "actionUrl" [ yield!!+form ]
        ]
    let html = renderToXml formlet |> template |> Renderer.RenderToString
    Assert.Equal("<html xmlns=\"http://www.w3.org/1999/xhtml\"><head><link type=\"text/css\" rel=\"stylesheet\" media=\"all\" href=\"/Content/css/main.css\" /></head><body><form action=\"actionUrl\" method=\"post\"><input name=\"f0\" value=\"a default value\" class=\"nice\" /></form></body></html>", html)

[<Fact>]
let ``render error form``() =
    let formlet = f.Text("a default value", ["class","nice"])
    let formlet = formlet |> Validate.Default.Int
    let template form =
        e.Html [ yield!!+form ]
    let env = EnvDict.fromValueSeq ["f0","abc"]
    let errorForm,_,_ = run formlet env
    let html = template errorForm |> Renderer.RenderToString
    Assert.Equal("<html xmlns=\"http://www.w3.org/1999/xhtml\"><span class=\"errorinput\"><input name=\"f0\" value=\"abc\" class=\"nice\" /></span><span class=\"error\">abc is not a valid number</span></html>", html)

[<Fact>]
let ``combine with wingbeats``() =
    let formlet =
        let id = "abc"
        s.Label id "a label" +> f.Text("a default value", ["id",id])
        <+ e.Br()
    let html = render formlet
    Assert.Equal("<label for=\"abc\">a label</label><input name=\"f0\" value=\"a default value\" id=\"abc\" /><br />", html)

[<Fact>]
let ``numbox render``() =
    let formlet = f.Float(required = true, maxlength = 4, attributes = ["class","nice"])
    let html = render formlet
    Assert.Equal("<input type=\"number\" maxlength=\"4\" required=\"required\" name=\"f0\" value=\"\" class=\"nice\" />", html)

[<Fact>]
let ``numbox run failure``() =
    let formlet = f.Float(required = true, maxlength = 4, attributes = ["class","nice"])
    let env = EnvDict.fromValueSeq ["f0","abc"]
    match run formlet env with
    | Failure(errorForm, _) -> 
        let html = XmlWriter.render errorForm
        let xml = XDocument.Parse "<r><span class='errorinput'><input name='f0' value='abc' type='number' maxlength='4' required='required' class='nice' /></span><span class='error'>Invalid value</span></r>"
        Assert.XmlEqual(xml.Root, XmlHelpers.xelem "r" [] errorForm)
        printfn "%s" html
    | _ -> failwith "Formlet should not have succeeded"

[<Fact>]
let ``intbox doesn't accept float``() =
    let formlet = f.Int()
    let env = EnvDict.fromValueSeq ["f0","1.3"]
    let html = render formlet
    printfn "%s" html
    match run formlet env with
    | Failure(errorForm, _) ->
        let html = XmlWriter.render errorForm
        let xml = XDocument.Parse "<r><span class='errorinput'><input name='f0' value='1.3' type='number' /></span><span class='error'>1.3 is not a valid number</span></r>"
        Assert.XmlEqual(xml.Root, XmlHelpers.xelem "r" [] errorForm)
        printfn "%s" html
    | _ -> failwith "Formlet should not have succeeded"

[<Fact>]
let ``intbox failure with range``() =
    let formlet = f.Int(min = 5, max = 10)
    let env = EnvDict.fromValueSeq ["f0","3"]
    match run formlet env with
    | Failure(errorForm, _) ->
        let html = XmlWriter.render errorForm
        let xml = XDocument.Parse "<r><span class='errorinput'><input min='5' max='10' name='f0' value='3' type='number' /></span><span class='error'>Value must be between 5 and 10</span></r>"
        Assert.XmlEqual(xml.Root, XmlHelpers.xelem "r" [] errorForm)
        printfn "%s" html
    | _ -> failwith "Formlet should not have succeeded"

[<Fact>]
let ``int with validation``() =
    let formlet = f.Int(min = 18, max = 100)
    let env = EnvDict.fromValueSeq ["f0", ""]
    match run formlet env with
    | Success v -> ()
    | Failure(errorForm, errorList) ->
        let expected = @"<span class='errorinput'>
        <input name='f0' value='' type='number' max='100' min='18' />
        </span>
        <span class='error'> is not a valid number</span>"
        Assert.XmlEqual(expected, errorForm)

[<Fact>]
let ``two formlets with validators and errors`` () =
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