module Formlets.Tests.Formlets

open System
open System.Collections.Specialized
open System.Drawing
open System.Globalization
open System.Web
open System.Xml.Linq
open Formlets.XmlWriter
open Fuchu
open Formlets
open FSharpx
open XmlHelpers

let e = FormElements(Validate.Default)

let input = input "" [] // no additional attributes

let inputInt = Validate.Default.Int input

let dateFormlet =
    let baseFormlet = 
        div ["style","padding:8px"] (
            span ["style", "border: 2px solid; padding: 4px"] (
                yields tuple2 <*>
                text "Month: " *> inputInt <*>
                text "Day: " *> inputInt
                <* br <* submit "Send" []
            )
        )
    let isDate (month,day) = 
        sprintf "%d%d%d" 2010 month day |> DateTime.parseExact [| "yyyyMMdd" |] |> Option.isSome
    let dateValidator = err isDate (fun (month,day) -> sprintf "%d/%d is not a valid date" month day)
    let validatingFormlet = baseFormlet |> satisfies dateValidator
    map (fun (month,day) -> DateTime(2010, month, day)) validatingFormlet

let fullFormlet =
    let inline tuple8 a b c d e f g h = a,b,c,d,e,f,g,h
    span [] (
        yields tuple8
        <*> dateFormlet
        <*> password
        <*> checkbox false []
        <*> radio "1" ["1","uno"; "2","dos"]
        <*> select "a" ["a","uno"; "b","dos"] []
        <*> textarea "" []
        <*> selectMulti ["a";"b"] ["a","uno"; "b","dos"] []
        <*> file []
    )

let manualNameFormlet =
    assignedInput "somename" "" []

let radioFormlet = 
    div [] (radio "1" ["1","uno"; "2","dos"])

open Formlets.Helpers
open Formlets.Tests.TestHelpers

[<Tests>]
let tests = 
    TestList [
        testList "Applicatives" [
            testCase "NameGen" <| fun _ ->
                let t = 
                    NameGen.puree tuple2 
                    |> NameGen.ap NameGen.nextName 
                    |> NameGen.ap NameGen.nextName
                    |> NameGen.run
                Assert.Equal("two name generations", ("f0", "f1"), t)

            testCase "XmlWriter" <| fun _ ->
                let a = XmlWriter.applicative
                let nodes, v = 
                    a.puree (+)
                    |> a.ap ([XText "a"],2)
                    |> a.ap ([XText "b"],3)
                Assert.Equal("sum", 5, v)
                Assert.Equal("list length", 2, nodes.Length)
                match nodes with
                | [TextV "a"; TextV "b"] -> ()
                | _ -> failtestf "Unexpected %A" nodes

            testCase "ErrorList" <| fun _ ->
                let a = ErrorList.applicative
                let errors, v = 
                    a.puree (+)
                    |> a.ap (["a"; "b"], 2)
                    |> a.ap (["c"; "d"], 3)
                Assert.Equal("sum", 5, v)
                Assert.Equal("list length", 4, errors.Length)
                Assert.Equal("errors", ["a";"b";"c";"d"], errors)

            testCase "XmlWriter composed with ErrorList" <| fun _ ->
                let puree x = x |> ErrorList.applicative.puree |> XmlWriter.applicative.puree
                let ap x f = flip (XmlWriter.applicative.lift2 (flip ErrorList.applicative.ap)) x f
                let nodes, (errors, v) =
                    puree (+)
                    |> ap ([XText "a"], (["a";"b"],2))
                    |> ap ([XText "b"], (["c";"d"],3))
                Assert.Equal("sum", 5, v)
                Assert.Equal("errors length", 4, errors.Length)
                Assert.Equal("nodes length", 2, nodes.Length)
                Assert.Equal("errors", ["a";"b";"c";"d"], errors)
                match nodes with
                | [TextV "a"; TextV "b"] -> ()
                | _ -> failtestf "Unexpected %A" nodes
        ]

        testList "Formlets" [
            testList "Radio" [
                testCase "render" <| fun _ ->
                    let html = render radioFormlet
                    //printfn "%s" html
                    ()

                testCase "run" <| fun _ ->
                    let env = EnvDict.fromValueSeq ["f0", "2"]
                    match run radioFormlet env with
                    | Success r -> Assert.Equal("collected formlet value", "2", r)
                    | _ -> failtest "shouldn't have failed"

                testCase "refill" <| fun _ ->
                    let env = EnvDict.fromValueSeq ["f0", "2"]
                    let nth a b = List.nth b a
                    let getChildren (n: XNode) =
                        match n with
                        | Tag e -> e.Nodes() |> Seq.toArray
                        | _ -> failtest "Expected tag, got text"
                    let { FormletResult.Form = form } = run radioFormlet env
                    let r = form |> nth 0 |> getChildren
                    //printfn "%A" r

                    let input1 = r.[0]
                    let input2 = r.[2]

                    match input1 with
                    | TagA(_,attr,_) -> 
                        if Seq.exists (fst >> (=) "checked") attr
                            then failtest "unexpected checked attribute found"
                    | _ -> failtest "err"

                    match input2 with
                    | TagA(_,attr,_) -> 
                        if not (Seq.exists (fst >> (=) "checked") attr)
                            then failtest "checked attribute not found"
                    | _ -> failtest "err"

            ]

            testCase "checkbox refill" <| fun _ ->
                let formlet = checkbox false []
                let env = EnvDict.fromValueSeq ["f0", "on"]
                let { FormletResult.Form = form } = run formlet env
                //printfn "%A" r
                match form.[0] with
                | TagA(_,attr,_) ->     
                    if not (Seq.exists (fst >> (=) "checked") attr)
                        then failtest "checked attribute not found"
                | _ -> failtest "err"

            testCase "input refill" <| fun _ ->
                let env = EnvDict.fromValueSeq ["f0", "pepe"]
                let { FormletResult.Form = form } = run input env
                //printfn "%A" r
                match form.[0] with
                | TagA(_,attr,_) -> 
                    if not (Seq.exists ((=)("value","pepe")) attr)
                        then failtestf "Expected value=pepe, found %A" attr
                | _ -> failtest "err"

            testCase "textarea refill" <| fun _ ->
                let env = EnvDict.fromValueSeq ["f0", "pepe"]
                let formlet = textarea "" []
                let { FormletResult.Form = form } = run formlet env
                //printfn "%A" r
                match form.[0] with
                | TagA(_,_,content) -> 
                    match content with
                    | [TextV t] -> Assert.Equal("populated formlet value", "pepe", t)
                    | _ -> failtestf "Unexpected content %A" content
                | _ -> failtest "err"

            testCase "optionalInput refill with value" <| fun _ ->
                let f = Formlet.optionalInput "ovalue" []
                let env = ["f0", "pepe"] |> EnvDict.fromValueSeq
                let { FormletResult.Form = form } = run f env
                let errorForm = XmlWriter.render form
                //printfn "%s" errorForm
                Assert.StringContains("error form", "name=\"f0\" value=\"pepe\"", errorForm)

            testCase "optionalInput refill without value" <| fun _ ->
                let f = Formlet.optionalInput "ovalue" []
                let result = run f []
                Assert.None("collected value", result.Value.Value)
                let errorForm = XmlWriter.render result.Form
                //printfn "%s" errorForm
                Assert.StringContains("error form", "name=\"f0\" value=\"ovalue\"", errorForm)

            testCase "select refill" <| fun _ ->
                let f = select "a" ["a","a"; "b","b"] []
                let env = EnvDict.fromValueSeq ["f0", "b"]
                let result = run f env
                Assert.Equal("collected value", Some "b", result.Value)
                Assert.Equal("error count", 0, result.Errors.Length)
                let errorForm = XmlWriter.render result.Form
                //printfn "%s" errorForm
                Assert.StringContains("error form", "value=\"b\" selected=\"selected\"", errorForm)

            testCase "image with values" <| fun _ ->
                let f = Formlet.image "src" "alt" []
                let env = EnvDict.fromValueSeq ["f0.x","12"; "f0.y","23"]
                match run f env with
                | Success (Some p) ->
                    Assert.Equal("x", 12, p.X)
                    Assert.Equal("y", 23, p.Y)
                | _ -> failtest "Should not have failed"

            testCase "image without values" <| fun _ ->
                let f = Formlet.image "src" "alt" []
                let env = EnvDict.fromValueSeq []
                match run f env with
                | Success None -> ()
                | _ -> failtest "Should not have failed"

            testCase "manual name formlet render" <| fun _ ->
                let html = render manualNameFormlet
                //printfn "%s" html
                Assert.Equal("html", "<input name=\"somename\" value=\"\" />", html)

            testCase "manual name formlet process" <| fun _ ->
                let env = ["somename", "somevalue"]
                let env = EnvDict.fromValueSeq env
                let result = run manualNameFormlet env
                Assert.Equal("collected value", Some "somevalue", result.Value)
                match result.Form with
                | [TagA(_,attr,_)] -> Assert.Equal("element attributes", ["name","somename"; "value","somevalue"], attr)
                | _ -> failtestf "Unexpected content %A" result.Form

            testCase "render" <| fun _ ->
                let html = render fullFormlet
                //printfn "%s" html
                ()

            testCase "process" <| fun _ ->
                let env = EnvDict.fromValueSeq [
                            "f0", "12"
                            "f1", "22"
                            "f2", ""
                            "f3", ""
                            "f5", "1"
                            "f6", "b"
                            "f7", "blah blah"
                            "f8", "a"
                            "f8", "b"
                          ]
                let filemock = { new HttpPostedFileBase() with
                                    member x.ContentLength = 2
                                    member x.ContentType = "" }
                let env = env |> EnvDict.addFromFileSeq ["f9", filemock]
                match run fullFormlet env with
                | Success(dt,pass,chk,n,opt,t,many,f) ->
                    Assert.Equal("date", DateTime(2010, 12, 22), dt)
                    Assert.Equal("password", "", pass)
                    Assert.Equal("checkbox", false, chk)
                    Assert.Equal("", "1", n)
                    Assert.Equal("", "b", opt)
                    Assert.Equal("", "blah blah", t)
                    Assert.Equal("", 2, many.Length)
                    Assert.Equal("", true, f.IsSome)
                | x -> failtestf "Shouldn't have failed. Actual result: %A" x

            testCase "process with invalid int" <| fun _ ->
                let env = [
                            "f0", "aa"
                            "f1", "22"
                          ]
                let env = EnvDict.fromValueSeq env
                let { FormletResult.Value = value } = run dateFormlet env
                //printfn "Error form:\n%s" (XmlWriter.render err)
                Assert.None("collected value", value)

            testCase "process with invalid ints" <| fun _ ->
                let env = [
                            "f0", "aa"
                            "f1", "bb"
                          ]
                let env = EnvDict.fromValueSeq env
                let { FormletResult.Value = value } = run dateFormlet env
                //printfn "Error form:\n%s" (XmlWriter.render err)
                Assert.None("collected value", value)

            testCase "process with invalid date" <| fun _ ->
                let env = [
                            "f0", "22"
                            "f1", "22"
                          ]
                let env = EnvDict.fromValueSeq env
                let { FormletResult.Value = value } = run dateFormlet env
                //printfn "Error form:\n%s" (XmlWriter.render err)
                Assert.None("collected value", value)

            testCase "process with missing field" <| fun _ ->
                let env = ["f0", "22"] |> EnvDict.fromValueSeq
                Assert.Raise("", typeof<ArgumentException>, (fun() -> run dateFormlet env |> ignore))

            testCase "NameValueCollection to seq does not ignore duplicate keys" <| fun _ ->
                let e = NameValueCollection()
                e.Add("1", "one")
                e.Add("1", "uno")
                let values = NameValueCollection.toSeq e
                let values = values |> Seq.filter (fst >> (=) "1") |> Seq.toList
                Assert.Equal("collection count", 2, values.Length)

            testCase "input encoded" <| fun _ ->
                let formlet = Formlet.input "<script>" []
                let html = render formlet
                //printfn "%s" html
                Assert.StringContains("", "&lt;script&gt;", html)

            testCase "textarea encoded" <| fun _ ->
                let formlet = textarea "<script>" []
                let html = render formlet
                //printfn "%s" html
                Assert.StringContains("", "&lt;script&gt;", html)

            testCase "addClass with no previous class" <| fun _ ->
                let before = ["something","value"]
                let after = before |> addClass "aclass"
                Assert.Equal("element attributes", ["class","aclass"; "something","value"], after)

            testCase "addClass with existing class" <| fun _ ->
                let before = ["something","value"; "class","class1"]
                let after = before |> addClass "aclass"
                Assert.Equal("element attributes", ["something","value"; "class","class1 aclass"], after)

            testCase "addStyle with no previous style" <| fun _ ->
                let before = ["something","value"]
                let after = before |> addStyle "border: 1px"
                Assert.Equal("element attributes", ["style","border: 1px"; "something","value"], after)

            testCase "addStyle with existing style" <| fun _ ->
                let before = ["something","value"; "style","color:red"]
                let after = before |> addStyle "border: 1px"
                Assert.Equal("element attributes", ["something","value"; "style","color:red;border: 1px"], after)

            testCase "mergeAttr with no dups" <| fun _ ->
                let a1 = ["something","value"]
                let a2 = ["style","color:red"]
                let r = mergeAttr a1 a2
                //printfn "%A" r
                Assert.Equal("merged length", 2, r.Length)
                Assert.ListExists(("something","value"), r)
                Assert.ListExists(("style","color:red"), r)

            testCase "mergeAttr with dups" <| fun _ ->
                let a1 = ["something","value"; "else","1"]
                let a2 = ["something","red"]
                let r = a1 |> mergeAttr a2
                //printfn "%A" r
                Assert.Equal("attribute count", 2, r.Length)
                Assert.ListExists(("something","red"), r)
                Assert.ListExists(("else","1"), r)

            testCase "mergeAttr with dup class" <| fun _ ->
                let a1 = ["something","value"; "class","1"]
                let a2 = ["something","red"; "class","bla"]
                let r = a1 |> mergeAttr a2
                //printfn "%A" r
                Assert.Equal("attribute count", 2, r.Length)
                Assert.ListExists(("something","red"), r)
                Assert.ListExists(("class","1 bla"), r)

            testCase "mergeAttr with dup style" <| fun _ ->
                let a1 = ["something","value"; "style","1"]
                let a2 = ["something","red"; "style","bla"]
                let r = a1 |> mergeAttr a2
                //printfn "%A" r
                Assert.Equal("attribute count", 2, r.Length)
                Assert.ListExists(("something","red"), r)
                Assert.ListExists(("style","1;bla"), r)

            testCase "from XElement" <| fun _ ->
                let div = !"div"
                let x = div.[div.["hello", div.[null]], div.["world"]]
                let formlet = xnode x
                let html = render formlet
                Assert.XmlEqual(x, renderToXml formlet)

            testCase "radio with int values" <| fun _ ->
                let formlet = radioA 5 [2,"dos"; 5,"cinco"]
                let html = render formlet
                //printfn "%s" html
                let env = EnvDict.fromValueSeq ["f0","2"]
                match run formlet env with
                | Success v -> Assert.Equal("collected value", 2, v)
                | _ -> failtest "Shouldn't have failed"

            testCase "radio with record values" <| fun _ ->
                let r1 = { PublicKey = "123123"; PrivateKey = "456456"; MockedResult = None }
                let r2 = { PublicKey = "abc"; PrivateKey = "def"; MockedResult = Some false }
                let formlet = radioA r1 [r1,"dos"; r2,"cinco"]
                let html = render formlet
                //printfn "%s" html
                let env = EnvDict.fromValueSeq ["f0", hashs r2]
                match run formlet env with
                | Success v -> Assert.Equal("collected value", r2, v)
                | _ -> failtest "Shouldn't have failed"

            testCase "validation without xml and with string" <| fun _ ->
                let validator = 
                    { IsValid = Int32.parse >> Option.isSome
                      ErrorForm = fun _ b -> b
                      ErrorList = fun v -> [sprintf "'%s' is not a valid number" v] }
                let formlet = 
                    input 
                    |> satisfies validator
                    |> map int
                let env = EnvDict.fromValueSeq ["f0","abc"]
                match run formlet env with
                | Success _ -> failtest "Formlet shouldn't have succeeded"
                | Failure(errorForm,errorMsg) -> 
                    //printfn "Error form: %s" (XmlWriter.render errorForm)
                    //printfn "%A" errorMsg
                    Assert.Equal("error count", 1, errorMsg.Length)
                    Assert.Equal("Error message", "'abc' is not a valid number", errorMsg.[0])

            testCase "validation without xml and with string with multiple formlets" <| fun _ ->
                let validator = 
                    { IsValid = Int32.parse >> Option.isSome
                      ErrorForm = fun _ b -> b
                      ErrorList = fun v -> [sprintf "'%s' is not a valid number" v] }
                let inputInt = 
                    input 
                    |> satisfies validator
                    |> map int
                let formlet = yields tuple2 <*> inputInt <*> inputInt
                let env = EnvDict.fromValueSeq ["f0","abc"; "f1","def"]
                match run formlet env with
                | Success _ -> failtest "Formlet shouldn't have succeeded"
                | Failure(errorForm,errorMsg) -> 
                    //printfn "Error form: %s" (XmlWriter.render errorForm)
                    //printfn "%A" errorMsg
                    Assert.Equal("error count", 2, errorMsg.Length)
                    Assert.Equal("First error message", "'abc' is not a valid number", errorMsg.[0])
                    Assert.Equal("second error message", "'def' is not a valid number", errorMsg.[1])

            testCase "parse raw xml" <| fun _ ->
                let formlet = rawXml "something <a href='someurl'>a link</a>"
                let html = render formlet
                //printfn "%s" html
                Assert.Equal("html", "something <a href=\"someurl\">a link</a>", html)

            testCase "non-rendering field render" <| fun _ ->
                Assert.Equal("render result", "", render field)

            testCase "two different formlets" <| fun _ ->
                let formlet = pair input (Formlet.input "value" ["class","red"])
                let expected = XmlWriter.parseRawXml "<input name='f0' value='' /><input name='f1' value='value' class='red' />"
                Assert.XmlEqual(expected, renderToXml formlet)

            testCase "non-rendering field rendered with another formlet" <| fun _ ->
                let formlet = yields tuple2 <*> input <*> field
                let html = render formlet
                Assert.Equal("render result", "<input name=\"f0\" value=\"\" />", html)

            testCase "non-rendering field run" <| fun _ ->
                let env = EnvDict.fromValueSeq ["f0","def"]
                match run field env with
                | Success v -> Assert.Equal("collected value", "def", v)
                | _ -> failtest "failed"

            testCase "validation error in non-rendering field" <| fun _ ->
                let fieldInt = field |> Validate.Default.Int
                let env = EnvDict.fromValueSeq ["f0","def"]
                match run fieldInt env with
                | Success _ -> failtest "Should not have succeeded"
                | Failure(errorForm,errorList) ->
                    Assert.Equal("error count", 1, errorList.Length)
                    Assert.Equal("error message", "def is not a valid number", errorList.[0])
                    //printfn "%s" (XmlWriter.render errorForm)

            testCase "merge attributes" <| fun _ ->
                let formlet = input |> mergeAttributes ["id","pepe"]
                let html = render formlet
                Assert.Equal("render result", "<input id=\"pepe\" name=\"f0\" value=\"\" />", html)

            testCase "merge attributes in error form" <| fun _ ->
                let formlet = input |> mergeAttributes ["id","pepe"] |> Validate.Default.Int
                let env = EnvDict.fromValueSeq ["f0","a"]
                match run formlet env with
                | Failure(errorForm,_) -> 
                    let html = XmlWriter.render errorForm
                    Assert.Equal("html", "<span class=\"errorinput\"><input id=\"pepe\" name=\"f0\" value=\"a\" /></span><span class=\"error\">a is not a valid number</span>", html)
                | _ -> failtest "Should not have succeeded"

            testCase "serialize DateTime" <| fun _ ->
                let v = DateTimeOffset(2011,1,1, 12,34,56, TimeSpan(0L)) |> dateTimeSerializer.Serialize
                Assert.Equal("", "2011-01-01T12:34:56.00Z", v)

            testCase "DateTime ok" <| fun _ ->
                let f = e.DateTime()
                let env = EnvDict.fromValueSeq ["f0","0037-12-13T02:10:33.00Z"]
                match run f env with
                | Success v -> Assert.Equal("", DateTimeOffset(37,12,13,2,10,33, TimeSpan(0L)), v)
                | _ -> failtest "should not have failed"

            testCase "DateTime min error" <| fun _ ->
                let f = e.DateTime(min = DateTimeOffset(2010,1,1, 0,0,0, TimeSpan(0L)))
                let env = EnvDict.fromValueSeq ["f0","0037-12-13T02:10:33.00Z"]
                match run f env with
                | Success v -> failtestf "should not have succeeded %A" v
                | _ -> ()

            testCase "Date ok" <| fun _ ->
                let f = e.Date()
                let env = EnvDict.fromValueSeq ["f0","0037-12-13"]
                match run f env with
                | Success v -> Assert.Equal("", DateTime(37,12,13), v)
                | _ -> failtest "should not have failed"
            
            testCase "Week serialization" <| fun _ ->
                let w = DateTime(2011,4,3) |> weekSerializer.Serialize
                Assert.Equal("", "2011-W13", w)

            testCase "Week serialization first day of year" <| fun _ ->
                let w = DateTime(2011,1,1) |> weekSerializer.Serialize
                Assert.Equal("", "2010-W52", w)

            testCase "Week serialization padded" <| fun _ ->
                let w = DateTime(2011,2,1) |> weekSerializer.Serialize
                Assert.Equal("", "2011-W04", w)

            testCase "Week deserialization" <| fun _ ->
                let dt = weekSerializer.Deserialize "2011-W13"
                Assert.Equal("", DateTime(2011,4,2), dt)

            testCase "Week tryDeserialize fail" <| fun _ ->
                let r = weekSerializer.TryDeserialize "pepe"
                match r with
                | false, _ -> ()
                | _ -> failtest "Should have failed"

            testCase "Time deserialize without second fraction" <| fun _ ->
                let dt = timeSerializer.Deserialize "23:45:56"
                Assert.Equal("hour", 23, dt.Hour)
                Assert.Equal("minute", 45, dt.Minute)
                Assert.Equal("second", 56, dt.Second)
                Assert.Equal("ms", 00, dt.Millisecond)

            testCase "Time deserialize without second" <| fun _ ->
                let dt = timeSerializer.Deserialize "23:45"
                Assert.Equal("hour", 23, dt.Hour)
                Assert.Equal("minute", 45, dt.Minute)
                Assert.Equal("second", 00, dt.Second)
                Assert.Equal("ms", 0, dt.Millisecond)

            testCase "Color serialize" <| fun _ ->
                let color = colorSerializer.Serialize Color.Red
                Assert.Equal("", "#FF0000", color)

            testCase "Color deserialize ok" <| fun _ ->
                let ok,color = colorSerializer.TryDeserialize "#FF3A3B"
                Assert.Equal("ok", true, ok)
                Assert.Equal("red", 0xFFuy, color.R)
                Assert.Equal("green", 0x3Auy, color.G)
                Assert.Equal("blue", 0x3Buy, color.B)

            testCase "hidden with initial value" <| fun _ ->
                let f = hidden "blabla"
                let html = render f
                Assert.StringContains("", "value=\"blabla\"", html)

            testCase "function pickle" <| fun _ ->
                let afunction a b = a + b
                let f = pickler afunction
                let html = render f
                //printfn "%s" html
                let bin = losSerializer.Serialize afunction
                let env = EnvDict.fromValueSeq ["f0",bin]
                match run f env with
                | Success ff -> Assert.Equal("Unpickled function result", 5, ff 2 3)
                | _ -> failtest "should not have failed"

            testCase "bin serializer string" <| fun _ ->
                let s = "toto"
                let r = binSerializer.Serialize s
                //printfn "length: %d, content: %s" r.Length r
                let dr = binSerializer.Deserialize r |> string
                Assert.Equal("Deserialized string", s, dr)

            testCase "bin serializer fun" <| fun _ ->
                let f a b = a + b
                let r = binSerializer.Serialize f
                //printfn "length: %d, content: %s" r.Length r
                ()

            testCase "bin serializer xtext" <| fun _ ->
                let x = XText("something")
                let r = binSerializer.Serialize x
                //printfn "length: %d, content: %s" r.Length r
                let x2 = binSerializer.Deserialize r :?> XText
                Assert.Equal("deserialized xtext", x.Value, x2.Value)

            testCase "los serializer string" <| fun _ ->
                let s = "toto"
                let r = losSerializer.Serialize s
                //printfn "length: %d, content: %s" r.Length r
                let dr = losSerializer.Deserialize r |> string
                Assert.Equal("deserialized string", s, dr)

            testCase "los serializer function" <| fun _ ->
                let f a b = a + b
                let r = losSerializer.Serialize f
                //printfn "length: %d, content: %s" r.Length r
                ()

            testCase "regex with empty value is valid" <| fun _ ->
                let f = Validate.Default.Regex "\\d" input
                let env = EnvDict.fromValueSeq ["f0",""]
                match run f env with
                | Success _ -> ()
                | Failure _ -> failtest "formlet should not have failed"

            testCase "email with empty value is valid" <| fun _ ->
                let f = Validate.Default.Email input
                let env = EnvDict.fromValueSeq ["f0",""]
                match run f env with
                | Success _ -> ()
                | Failure _ -> failtest "formlet should not have failed"
        ]
    ]