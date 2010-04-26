open System
open System.IO
open System.Diagnostics
open System.Reflection

[<EntryPoint>]
let main args =
    // code adapted from FSharp.PowerPack's AspNetTester
    let progfile = 
        let prg = Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles)
        if Environment.Is64BitOperatingSystem && Environment.Is64BitProcess
            then prg + " (x86)"
            else prg
            
    let webserver90 = Path.Combine(progfile, @"Common Files\microsoft shared\DevServer\9.0\WebDev.WebServer.EXE")
    let webserver100v2 = Path.Combine(progfile, @"Common Files\microsoft shared\DevServer\10.0\WebDev.WebServer20.EXE")
    let webserver100v4 = Path.Combine(progfile, @"Common Files\microsoft shared\DevServer\10.0\WebDev.WebServer40.EXE")
    let webserver = 
        if File.Exists(webserver100v2) then 
            if Environment.Version.Major = 2 
                then webserver100v2
                else webserver100v4
        elif File.Exists(webserver90) then webserver90 
        else failwith "No ASP.NET dev web server found."
    
    let webSitePath = Directory.GetParent(Directory.GetCurrentDirectory()).FullName
    let port = Random().Next(10000, 65535)
    let pathArg = sprintf "/path:%s" webSitePath
    let portArg = sprintf "/port:%d" port
    
    let asm = Assembly.LoadFile webserver
    let run (args: string[]) = asm.EntryPoint.Invoke(null, [| args |]) :?> int

    Process.Start (sprintf "http://localhost:%d" port) |> ignore
    run [| pathArg; portArg |]    