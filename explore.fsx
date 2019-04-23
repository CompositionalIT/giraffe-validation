#load @".paket\load\netstandard2.0\main.group.fsx"
#r "netstandard"

open Giraffe
open Microsoft.AspNetCore.Http

module Validation =
    open System

    type ValidationResult = Result<unit, string>

    type ModelValidationResult<'Model> = Result<'Model, string>

    type HttpValidationResult<'Model> = Result<'Model, HttpFunc -> HttpContext -> HttpFuncResult>

    let buildValidationResult<'a> predicate (getError : unit -> string) : ValidationResult =
        if predicate then Error(getError())
        else Ok()

    let validateField fieldName getValue validators model : _ ModelValidationResult =
        let errors =
            validators
            |> Seq.choose (fun validator ->
                   match validator (getValue model) with
                   | Ok _ -> None
                   | Error errText -> Some errText)
            |> Seq.toArray
        match errors with
        | [||] -> Ok model
        | errors ->
            Error(sprintf "Field '%s' is invalid: %s." fieldName (String.concat ". " errors))

    let validateOptionalField fieldName getValue validators model : _ ModelValidationResult =
        match getValue model with
        | None -> Ok model
        | Some value -> validateField fieldName (fun _ -> value) validators model

    let validate fieldValidators model : _ HttpValidationResult =
        fieldValidators
        |> Array.choose (fun validator ->
               match validator model with
               | Ok _ -> None
               | Error errs -> Some errs)
        |> function
        | [||] -> Ok model
        | errors ->
            errors
            |> String.concat " "
            |> RequestErrors.BAD_REQUEST
            |> Error

    module Text =
        let isNotEmpty value =
            buildValidationResult (String.IsNullOrWhiteSpace value) (fun _ -> "Field is empty")
        let minLength minLength (value : string) =
            buildValidationResult (value.Length < minLength)
                (fun _ -> sprintf "Minimum length is %d, but was %d" minLength value.Length)
        let maxLength maxLength (value : string) =
            buildValidationResult (value.Length > maxLength)
                (fun _ -> sprintf "Maximum length is %d, but was %d" maxLength value.Length)

    module Numeric =
        let min minValue value =
            buildValidationResult (value < minValue)
                (fun _ -> sprintf "Minimum value is %d, but was %d" minValue value)
        let max maxValue value =
            buildValidationResult (value > maxValue)
                (fun _ -> sprintf "Maximum value is %d, but was %d" maxValue value)

        let between minValue maxValue value =
            min minValue value
            |> Result.bind (fun () -> max maxValue value)
            |> Result.mapError
                   (fun _ ->
                   sprintf "Value must be between %d and %d, but was %d" minValue maxValue value)

        let notEqual neqValue value =
            buildValidationResult (value = neqValue)
                (fun _ -> sprintf "Value must not be %d" neqValue)
        let noneOf blacklistValues value =
            buildValidationResult (blacklistValues |> Seq.exists ((=) value))
                (fun _ -> sprintf "Value must not be any of %s" (blacklistValues
                                                                 |> Seq.map string
                                                                 |> String.concat ", "))

    let optional validator value =
        match value with
        | None -> Ok()
        | Some value -> validator value

open Validation

// OPTION 2

type MyValidator<'Model>(model : 'Model) =
    member __.Yield _ : string list = []
    [<CustomOperation"rule">]
    member __.Rule<'Field>(state, getter, rule : 'Field -> Result<unit, string>) =
        match getter model |> rule with
        | Error err -> err :: state
        | Ok _ -> state

type Person = { Name : string }

let model = { Name = "Isaac" }

let x =
    MyValidator model {
        rule (fun x -> x.Name) (Text.minLength 10)
        rule (fun x -> x.Name) (Text.maxLength 10)
        rule (fun x -> x.Name) Text.isNotEmpty
    }


// OPTION 3

type TwoValidator() =
    member __.Bind(validationResult : ValidationResult, binder) =
        match validationResult with
        | Ok() -> binder()
        | Error err -> Error(RequestErrors.BAD_REQUEST err)
    member __.Return(v) = Ok v

let tv = TwoValidator()



let z : Person HttpValidationResult =
    tv {
        do! Text.minLength 10 model.Name
        do! Text.maxLength 20 model.Name
        return model
    }
    