namespace Formlets

type 'a ErrorList = string list * 'a

module ErrorList =
    let applicative = ListPairApplicative<string>()
