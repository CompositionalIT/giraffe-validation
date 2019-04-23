module ModelA

open Giraffe
open Validation

[<AutoOpen>]
module ValidationCe =
    type ValidationCe() =
        member __.Bind(validationResult : ValidationResult, binder) =
            match validationResult with
            | Ok() ->
                binder()
            | Error err ->
                Error(RequestErrors.BAD_REQUEST err)

        member __.Return(v) = Ok v
    let validator = ValidationCe()

[<CLIMutable>]
type Person =
    { Forename : string
      Surname : string
      MiddleName : string option
      Age : int }

    interface ModelValidation.IModelValidation<Person> with
        member this.Validate() =
            validator {
                do! Text.IsNotEmpty(this.Forename)
                do! Text.MinLength(3, this.Forename)
                do! Text.MaxLength(20, this.Forename)
                do! this.Age |> Numeric.between 18 65 "Age"
                do! this.Age |> Numeric.noneOf [ 39; 40; 41 ] "Age"
                do!
                    match this.MiddleName with
                    | Some middleName -> Text.MinLength(5, middleName)
                    | None -> Result.Ok()

                return this
            }

let handler next = bindForm<Person> None (validateModel Successful.OK) next
