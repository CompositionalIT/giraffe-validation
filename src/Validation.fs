module Validation

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open Giraffe
open Microsoft.AspNetCore.Http
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
               match validator fieldName (getValue model) with
               | Ok _ -> None
               | Error errText -> Some errText)
        |> Seq.toArray
    match errors with
    | [||] ->
        Ok model
    | errors ->
        Error(sprintf "Field '%s' is invalid: %s." fieldName (String.concat ". " errors))

let validateOptionalField fieldName getValue validators model : _ ModelValidationResult =
    match getValue model with
    | None ->
        Ok model
    | Some value ->
        validateField fieldName (fun _ -> value) validators model

let validate fieldValidators model : _ HttpValidationResult =
    fieldValidators
    |> Array.choose (fun validator ->
        match validator model with
        | Ok _ ->
            None
        | Error errs ->
            Some errs)
    |> function
    | [||] ->
        Ok model
    | errors ->
        errors
        |> String.concat " "
        |> RequestErrors.BAD_REQUEST
        |> Error


/// An experimental function to convert an expression into a validation result.
let parseExpr predicate getError (expr : string Expr) =
    match expr with
    | WithValue(:? string as value, _, PropertyGet(_, prop, _)) ->
        buildValidationResult (predicate value) (getError (prop.Name, value))
    | WithValue (:? string as value, _, ValueWithName(_, _, field)) ->
        buildValidationResult (predicate value) (getError (field, value))
    | _ ->
        Ok()

/// Text validators helpers. Notice that these make use of the ReflectedDefinitionAttribute so 
/// that they can infer the field name with the funky Expr code above. The downside is that
/// this doesn't appear to work with let-bound functions, so we have to use a class with static
/// members and no curried functions.
type Text() =
    static member IsNotEmpty([<ReflectedDefinition(true)>] expr) =
        expr |> parseExpr String.IsNullOrWhiteSpace (fun (name, _) _ -> sprintf "%s is empty" name)
    static member MinLength(minLength, [<ReflectedDefinition(true)>] expr) =
        expr
        |> parseExpr
            (fun v -> v.Length < minLength)
            (fun (name, value) _ -> sprintf "Minimum length of %s is %d, but was %d" name minLength value.Length)
    static member MaxLength(maxLength, [<ReflectedDefinition(true)>] expr) =
        expr
        |> parseExpr
            (fun v -> v.Length > maxLength)
            (fun (name, value) _ -> sprintf "Maximum length of %s is %d, but was %d" name maxLength value.Length)

/// Here's the same text validators but as regular let-bound curried functions.
module Text =
    let isNotEmpty fieldName value =
        buildValidationResult (String.IsNullOrWhiteSpace value) (fun _ -> sprintf "%s is empty" fieldName)

    let minLength minLength fieldName (value:string) =
        buildValidationResult (value.Length < minLength) (fun _ -> sprintf "Minimum length of %s is %d, but was %d" fieldName minLength value.Length)

    let maxLength maxLength fieldName (value:string) =
        buildValidationResult (value.Length > maxLength) (fun _ -> sprintf "Maximum length of %s is %d, but was %d" fieldName maxLength value.Length)


/// Numeric validators. These don't use ReflectedDefinition, so either these would need to be
/// supplied with the field name as a string, or simply not supply the name of the field being
/// validated.
module Numeric =
    let min minValue fieldName value =
        buildValidationResult
            (value < minValue)
            (fun _ -> sprintf "Minimum value of %s is %d, but was %d" fieldName  minValue value)
    let max maxValue fieldName  value =
        buildValidationResult
            (value > maxValue)
            (fun _ -> sprintf "Maximum value of %s is %d, but was %d" fieldName  maxValue value)

    let between minValue maxValue fieldName value =
        min minValue fieldName value
        |> Result.bind (fun () -> max maxValue fieldName value)
        |> Result.mapError (fun _ -> sprintf "Value of %s must be between %d and %d, but was %d" fieldName minValue maxValue value)

    let notEqual neqValue fieldName value =
        buildValidationResult (value = neqValue) (fun _ -> sprintf "Value of %s must not be %d" fieldName neqValue)

    let noneOf (blacklistValues:int seq) fieldName value =
        buildValidationResult
            (blacklistValues |> Seq.exists ((=) value))
            (fun _ -> sprintf "Value of %s must not be any of %s"
                            fieldName
                            (blacklistValues
                             |> Seq.map string
                             |> String.concat ", "))

let optional validator fieldName value =
    match value with
    | None ->
        Ok()
    | Some value ->
        validator fieldName value

