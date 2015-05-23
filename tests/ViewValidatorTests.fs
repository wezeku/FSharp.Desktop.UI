module FSharp.Desktop.UI.ViewValidatorTests

open Xunit
open System
open System.ComponentModel
open System.Globalization
open System.Windows.Controls
open System.Windows.Data

open BindingOptions
open FSharp.Desktop.UI

[<AbstractClass>]
type MyModel() =
    inherit Model()
    abstract Value: string with get, set

[<Fact>]
let ``viewValidationRule from object expression``() = 
    let rule = { new ValidationRule() with member __.Validate(_, _) = ValidationResult.ValidResult }

    let textBox = TextBox()
    let model: MyModel = MyModel.Create()

    textBox.DataContext <- model
    Binding.OfExpression <@ textBox.Text <- model.Value |> ViewValidationRule rule @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding
    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.Same(rule, binding.ValidationRules.[0])

[<Fact>]
let ``viewValidationRule with instantiated ValidationRule descendant``() = 
    // This will cause Coerce(..., ValidationRule) to be part of the quoted expression.
    let rule = ExceptionValidationRule()

    let textBox = TextBox()
    let model: MyModel = MyModel.Create()

    textBox.DataContext <- model
    Binding.OfExpression <@ textBox.Text <- model.Value |> ViewValidationRule rule @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding
    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.Same(rule, binding.ValidationRules.[0])

[<Fact>]
let ``viewValidationRule with inline instantiation``() = 
    let textBox = TextBox()
    let model: MyModel = MyModel.Create()

    textBox.DataContext <- model
    Binding.OfExpression <@ textBox.Text <- model.Value |> ViewValidationRule (ExceptionValidationRule()) @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding
    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.IsType<ExceptionValidationRule>(binding.ValidationRules.[0]) |> ignore

[<Fact>]
let ``viewValidationRule with property initializers``() = 
    let textBox = TextBox()
    let model: MyModel = MyModel.Create()

    textBox.DataContext <- model
    Binding.OfExpression 
        <@ textBox.Text <- model.Value 
                           |> ViewValidationRule (ExceptionValidationRule(ValidationStep = ValidationStep.UpdatedValue,
                                                                          ValidatesOnTargetUpdated = true )) @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding
    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.IsType<ExceptionValidationRule>(binding.ValidationRules.[0]) |> ignore
    Assert.Equal(ValidationStep.UpdatedValue, binding.ValidationRules.[0].ValidationStep)
    Assert.Equal(true, binding.ValidationRules.[0].ValidatesOnTargetUpdated)

[<Fact>]
let viewValidator() = 
    let textBox = TextBox()
    let model: MyModel = MyModel.Create()
    let validator (_ : obj) (_ : CultureInfo) = ViewValidation.Invalid(42)
    textBox.DataContext <- model
    Binding.OfExpression <@ textBox.Text <- model.Value |> ViewValidator validator ValidationStep.UpdatedValue true @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding

    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.IsType<ViewValidation.ValidatorWrapper>(binding.ValidationRules.[0]) |> ignore
    Assert.Equal(ValidationStep.UpdatedValue, binding.ValidationRules.[0].ValidationStep)
    Assert.Equal(true, binding.ValidationRules.[0].ValidatesOnTargetUpdated)
    let validationResult = 
        binding.ValidationRules.[0].Validate(null, CultureInfo.InvariantCulture).ErrorContent |> unbox
    Assert.Equal(42, validationResult)

[<Fact>]
let ``viewValidator with curried arguments``() = 
    let textBox = TextBox()
    let model: MyModel = MyModel.Create()
    let validator (a : int) (b : string) (_ : obj) (_ : CultureInfo) = ViewValidation.Invalid(box (a, b))
    textBox.DataContext <- model
    Binding.OfExpression 
        <@ textBox.Text <- model.Value |> ViewValidator (validator 42 "foo") ValidationStep.UpdatedValue true @>
    let binding = textBox.GetBindingExpression(TextBox.TextProperty).ParentBinding

    Assert.Equal(1, binding.ValidationRules.Count)
    Assert.IsType<ViewValidation.ValidatorWrapper>(binding.ValidationRules.[0]) |> ignore
    Assert.Equal(ValidationStep.UpdatedValue, binding.ValidationRules.[0].ValidationStep)
    Assert.Equal(true, binding.ValidationRules.[0].ValidatesOnTargetUpdated)
    let validationResult = 
        binding.ValidationRules.[0].Validate(null, CultureInfo.InvariantCulture).ErrorContent |> unbox
    Assert.Equal((42, "foo"), validationResult)
