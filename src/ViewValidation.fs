[<RequireQualifiedAccess>]
module FSharp.Desktop.UI.ViewValidation

open System.Globalization
open System.Windows.Controls
open System.Windows.Data

type ValidationResult =
    | Invalid of obj
    | Valid


// Encapsulates a validation function in a ValidationRule object.
type ValidatorWrapper(validator : obj -> CultureInfo -> ValidationResult,
                      ?validationStep, ?validatesOnTargetUpdated) =
    inherit ValidationRule(defaultArg validationStep ValidationStep.RawProposedValue,
                           defaultArg validatesOnTargetUpdated false)

    member val Validator = validator

    override o.Validate(value, cultureInfo) =
        match validator value cultureInfo with
        | Valid -> ValidationResult.ValidResult
        | Invalid errorContent -> ValidationResult(false, errorContent)


let bindValidationRules target targetProperty validationRules =
    let b = BindingOperations.GetBinding(target, targetProperty)
    for v in validationRules do b.ValidationRules.Add(v)
