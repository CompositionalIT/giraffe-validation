module ModelB

open Giraffe
open Validation

[<CLIMutable>]
type Person =
    { Forename : string
      Surname : string
      MiddleName : string option
      Age : int }

    interface ModelValidation.IModelValidation<Person> with
        member this.Validate() =
            validate [|
                validateField "Forename" (fun x -> x.Forename) [|
                    Text.isNotEmpty
                    Text.minLength 3
                    Text.maxLength 20
                |]
                validateField "Age" (fun x -> x.Age) [|
                    Numeric.between 18 65
                    Numeric.noneOf [ 39; 40; 41 ]
                |]
                validateOptionalField "Middle Name" (fun x -> x.MiddleName) [|
                    Text.minLength 5
                |]
            |] this

let handler next = bindForm<Person> None (validateModel Successful.OK) next
