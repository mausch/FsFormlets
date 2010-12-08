module Formlets

type xml_item = 
    | Text of string 
    | Tag of string * (string*string) list * xml_item list 

/// Compose two applicatives
module ApplicCompose =
    let puree (f_pure, g_pure) x = f_pure (g_pure x)
    let ap (f_ap, f_pure, g_ap) f x =
        let (<*>) a b = f_ap a b
        f_pure g_ap <*> f <*> x
    let refine (f_ap, f_pure, g_pure) v = f_ap (f_pure g_pure) v

module XmlWriter =
    let puree v = [],v
    let ap (x,f) (y,a) = x @ y, f a
    let plug k (x,v) = k x, v
    let xml e = plug (fun _ -> e) (puree ())
    let text s = xml [Text s]
    let tag t ats v = plug (fun x -> [Tag (t, ats, x)]) v
    let run = id

module NameGen =
    let puree v gen = v,gen
    let ap f a gen =
        let v,gen = f gen
        let w,gen = a gen
        v w, gen
    let nextName gen = "input_" + gen.ToString(), gen+1
    let run c = fst (c 0)

module Environ =
    let puree v env = v
    let ap f a env = f env (a env)
    let rec lookup n = function
    | []                    -> failwith ("Not found : " + n)
    | (m,v)::_   when n = m -> v
    | _    ::env            -> lookup n env
    let run = id

module Formlet =
    let XmlEnv_refine v = ApplicCompose.refine (XmlWriter.ap, XmlWriter.puree, Environ.puree) v
    let xml x = NameGen.puree (XmlEnv_refine (XmlWriter.xml x))
    let text s = NameGen.puree (XmlEnv_refine (XmlWriter.text s))
    let tag t ats f = NameGen.ap (NameGen.puree (XmlWriter.tag t ats)) f
    let run v = 
        let xml, collector = XmlWriter.run (NameGen.run v)
        xml, Environ.run collector
    let input x =
        NameGen.ap (NameGen.puree (fun n -> XmlWriter.tag "input" [("name", n)] (XmlWriter.puree (Environ.lookup n)))) NameGen.nextName x


