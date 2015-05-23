﻿[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharp.Desktop.UI.Binding

open System
open System.Collections.Generic
open System.Reflection
open System.Diagnostics
open System.Windows
open System.Windows.Data 
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Unchecked

type DerivedPropertyAttribute = ReflectedDefinitionAttribute

let coerce _ = undefined

module BindingOptions =
    open System.Globalization
    open System.Windows.Controls

    let Mode (_ : BindingMode) (_ : 'T) : 'T                 = undefined
    let OneWay (_ : 'T) : 'T                                 = undefined
    let UpdateSource (_ : UpdateSourceTrigger) (_ : 'T) : 'T = undefined
    let UpdateSourceOnChange (_ : 'T) : 'T                   = undefined
    let FallbackValue (_ : obj) (_ : 'T) : 'T                = undefined
    let TargetNullValue (_ : obj) (_ : 'T) : 'T              = undefined
    let ValidatesOnDataErrors (_ : bool) (_ : 'T) : 'T       = undefined
    let ValidatesOnExceptions (_ : bool) (_ : 'T) : 'T       = undefined
    let ViewValidationRule (_ : ValidationRule) (_ : 'T) : 'T  = undefined
    let ViewValidator (_ : (obj -> CultureInfo -> ViewValidation.ValidationResult)) 
                      (_ : ValidationStep) (_ : bool) (_ : 'T) = undefined

module Patterns = 
    open FSharp.Quotations.Evaluator

    type IValueConverter with 
        static member OneWay converter = {
            new IValueConverter with
                member this.Convert(value, _, _, _) = 
                    try converter value 
                    with _ -> DependencyProperty.UnsetValue
                member this.ConvertBack(_, _, _, _) = DependencyProperty.UnsetValue
        }

    type PropertyInfo with
        member this.DependencyProperty: DependencyProperty = 
            this.DeclaringType.GetField(this.Name + "Property").GetValue(null) |> unbox

    let (|Target|_|) expr = 
        let rec loop = function
            | Some( Value(obj, viewType) ) -> obj
            | Some( FieldGet(tail, field) ) ->  field.GetValue(loop tail)
            | Some( PropertyGet(tail, prop, []) ) -> prop.GetValue(loop tail, [||])
            | _ -> null
        match loop expr with
        | :? DependencyObject as target -> Some target
        | _ -> None

    let (|PropertyPath|_|) expr = 
        let rec loop e acc = 
            match e with
            | PropertyGet( Some tail, property, []) -> 
                loop tail (property.Name :: acc)
            | Value _ | Var _ -> Some acc
            | _ -> None

        loop expr [] |> Option.map (String.concat "." )

    let (|StringFormat|_|) = function
        | SpecificCall <@ String.Format : string * obj -> string @> (None, [], [ Value(:? string as format, _); Coerce( propertyPath, _) ]) ->
            Some(format, propertyPath)
        | _ -> None    

    let (|Nullable|_|) = function
        | NewObject( ctorInfo, [ propertyPath ] ) when ctorInfo.DeclaringType.GetGenericTypeDefinition() = typedefof<Nullable<_>> -> 
            Some propertyPath
        | _ -> None    

    let (|Converter|_|) = function
        | Call(instance, method', [ PropertyPath _ as propertyPath ]) -> 
            let instance = match instance with | Some( Value( value, _)) -> value | _ -> null
            Some((fun(value : obj) -> method'.Invoke(instance, [| value |])), propertyPath )
        | _ -> None    
         
    open DerivedProperties

    let (|DerivedProperty|_|) = function
        | PropertyGet
            ( 
                Some( PropertyPath path), 
                (PropertyGetterWithReflectedDefinition (Lambda (model, Lambda(unitVar, propertyBody))) as property), 
                []
            ) when not property.CanWrite ->
            assert(unitVar.Type = typeof<unit>)
            getMultiBindingForDerivedProperty(path, model, propertyBody, property.GetValue) |> Some
        | _ -> None

    let (|ExtensionDerivedProperty|_|) = function
        | Call
            ( 
                None, 
                (MethodWithReflectedDefinition (Lambda (model, Lambda(unitVar, methodBody))) as getMethod), 
                [ PropertyPath path ]
            ) when getMethod.Name.StartsWith(model.Type.Name + ".get_") -> 
            assert(unitVar.Type = typeof<unit>)
            let getter model = getMethod.Invoke(null, [| model |])
            getMultiBindingForDerivedProperty(path, model, methodBody, getter) |> Some
        | _ -> None
    
    let rec (|SourceProperty|_|) = function 
        | PropertyGet( Some _, prop, []) -> Some prop 
        | PropertyGet( Some( PropertyGet( Some _, step1, [])), step2, []) when step2.DeclaredOnNullable || step2.DeclaredOnOption -> Some step1 
        | _ -> None

    let rec extractPropertyGetters propertyBody = 
        seq {
            match propertyBody with 
            | PropertyGet _ as getter -> yield getter
            | ShapeVar _ -> ()
            | ShapeLambda(_, body) -> yield! extractPropertyGetters body   
            | ShapeCombination(_, exprs) -> for subExpr in exprs do yield! extractPropertyGetters subExpr
        }

    let (|SinglePropertyExpression|_|) expr = 
        match expr |> extractPropertyGetters |> Seq.distinct |> Seq.toList with
        | [ SourceProperty prop as getterToReplace ] ->
            let propertyValue = Var("value", typeof<obj>)
            let rec replacePropertyWithParam expr = 
                match expr with 
                | PropertyGet _ as getter when getter = getterToReplace -> 
                    Expr.Coerce( Expr.Var propertyValue, prop.PropertyType)
                | ShapeVar var -> Expr.Var(var)
                | ShapeLambda(var, body) -> Expr.Lambda(var, replacePropertyWithParam body)  
                | ShapeCombination(shape, exprs) -> ExprShape.RebuildShapeCombination(shape, exprs |> List.map(fun e -> replacePropertyWithParam e))

            let converter : obj -> obj = 
                Expr.Lambda( propertyValue, body = Expr.Coerce(replacePropertyWithParam expr, typeof<obj>))
                |> Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation 
                |> unbox

            let binding = Binding(prop.Name, Mode = BindingMode.OneWay)
            binding.ValidatesOnNotifyDataErrors <- false
            binding.Converter <- IValueConverter.OneWay converter
            Some binding
        | _ -> None

    let (|Argument|) = function
        // Try a couple of quick, typical expressions before using
        // the power of QuotationEvaluator (which is slightly slower).
        | Coerce((Value(obj, _)), _)
        | Value(obj, _) -> obj
        | expr -> QuotationEvaluator.EvaluateUntyped expr

    let rec (|Source|) = function
        | DerivedProperty binding 
        | ExtensionDerivedProperty binding -> binding
        | PropertyPath path -> 
            Binding(path) :> BindingBase
        | Coerce( Source binding, _) 
        | SpecificCall <@ coerce @> (None, _, [ Source binding ]) 
        | Nullable( Source binding) -> 
            binding

        // Binding properties modifiers:
        | SpecificCall <@ BindingOptions.Mode @> (None, _, [ Value(mode, _); Source(:? Binding as binding) ]) ->
            binding.Mode <- unbox mode
            upcast binding
        | SpecificCall <@ BindingOptions.OneWay @> (None, _, [ Source(:? Binding as binding) ]) ->
            binding.Mode <- BindingMode.OneWay
            upcast binding
        | SpecificCall <@ BindingOptions.UpdateSource @> (None, _, [ Value(trigger, _); Source(:? Binding as binding) ]) ->
            binding.UpdateSourceTrigger <- unbox trigger
            upcast binding
        | SpecificCall <@ BindingOptions.UpdateSourceOnChange @> (None, _, [ Source(:? Binding as binding) ]) ->
            binding.UpdateSourceTrigger <- UpdateSourceTrigger.PropertyChanged
            upcast binding
        | SpecificCall <@ BindingOptions.FallbackValue @> (None, _, [ Value(value, _); Source(:? Binding as binding) ]) ->
            binding.FallbackValue <- value
            upcast binding
        | SpecificCall <@ BindingOptions.TargetNullValue @> (None, _, [ Value(value, _); Source(:? Binding as binding) ]) ->
            binding.TargetNullValue <- value
            upcast binding
        | SpecificCall <@ BindingOptions.ValidatesOnDataErrors @> (None, _, [ Value(value, _); Source(:? Binding as binding) ]) ->
            binding.ValidatesOnDataErrors <- unbox value
            upcast binding
        | SpecificCall <@ BindingOptions.ValidatesOnExceptions @> (None, _, [ Value(value, _); Source(:? Binding as binding) ]) ->
            binding.ValidatesOnExceptions <- unbox value
            upcast binding
        | SpecificCall <@ BindingOptions.ViewValidationRule @> (None, _, [ Argument(rule); Source(:? Binding as binding) ]) ->
            binding.ValidationRules.Add (unbox rule)
            upcast binding
        | SpecificCall <@ BindingOptions.ViewValidator @> 
                      (None, _, [ Argument(validator); 
                                  Value(validationStep, _); 
                                  Value(validatesOnTargetUpdated, _); 
                                  Source(:? Binding as binding) ]) ->
            binding.ValidationRules.Add (ViewValidation.ValidatorWrapper(unbox validator, 
                                                                         unbox validationStep, 
                                                                         unbox validatesOnTargetUpdated))
            upcast binding
        // End of Binding properties modifiers.

        | StringFormat(format, Source(:? Binding as binding)) -> 
            binding.StringFormat <- format
            binding.ValidatesOnNotifyDataErrors <- false
            upcast binding

        | Converter(convert, Source(:? Binding as binding)) -> 
            binding.Mode <- BindingMode.OneWay
            binding.ValidatesOnNotifyDataErrors <- false
            binding.Converter <- IValueConverter.OneWay convert
            upcast binding
        | SinglePropertyExpression binding -> 
            upcast binding

        | expr -> invalidArg "binding property path quotation" (string expr)

    let inline configureBinding(binding : #BindingBase, mode, updateSourceTrigger, fallbackValue, targetNullValue, validatesOnExceptions, validatesOnDataErrors) = 
        mode |> Option.iter (fun x -> (^a : (member set_Mode : BindingMode -> unit) (binding, x)))
        updateSourceTrigger |> Option.iter (fun x -> (^a : (member set_UpdateSourceTrigger : UpdateSourceTrigger -> unit) (binding, x)))
        fallbackValue |> Option.iter (fun x -> (^a : (member set_FallbackValue : obj -> unit) (binding, x)))
        targetNullValue |> Option.iter (fun x -> (^a : (member set_TargetNullValue : obj -> unit) (binding, x)))
        validatesOnExceptions |> Option.iter (fun x -> (^a : (member set_ValidatesOnExceptions : bool -> unit) (binding, x)))
        validatesOnDataErrors |> Option.iter (fun x -> (^a : (member set_ValidatesOnDataErrors : bool -> unit) (binding, x)))

open Patterns

type Expr with
    static member internal RewritePipeIntoCall (e : Expr) =
        let rec unpipe (vars : Map<string, Expr>) (pipedArgs : Expr list) (expr : Expr) =
            match expr with
            | SpecificCall <@ (|>) @> (None, _, [pipedArg; rightOfPipe]) ->
                unpipe vars (pipedArg::pipedArgs) rightOfPipe
            | Lambda (arg, lambdaExpr) ->
                match pipedArgs with
                | hd::tl ->
                    let vars' = vars.Add(arg.Name, hd)
                    unpipe vars' tl lambdaExpr
                | [] -> failwith "Missing argument."
            | Let (var, letExpr, inBody) ->
                let vars' = vars.Add(var.Name, letExpr)
                unpipe vars' pipedArgs inBody
            | Call(_, fcn, args) ->
                let argValues =
                    [for arg in args ->
                        match arg with
                        | Coerce (Var v, cType) -> Expr.Coerce(vars.[v.Name], cType)
                        | Var v -> vars.[v.Name]
                        | e -> e
                    ] |> List.map Expr.RewritePipeIntoCall
                Expr.Call(fcn, argValues)
            | e -> e  // Leave all other expression types untouched.
        unpipe Map.empty [] e

    member internal this.ToBinding(?mode, ?updateSourceTrigger, ?fallbackValue, ?targetNullValue, ?validatesOnExceptions, ?validatesOnDataErrors) = 
        match this with
        | PropertySet(Target target, targetProperty, [], bindingExpr) ->
            let (Source binding) = Expr.RewritePipeIntoCall bindingExpr

            match binding with
            | :? Binding as single -> 
                configureBinding(single,  mode, updateSourceTrigger, fallbackValue, targetNullValue, validatesOnExceptions, validatesOnDataErrors)
            | :? MultiBinding as multi ->
                configureBinding(multi,  mode, updateSourceTrigger, fallbackValue, targetNullValue, validatesOnExceptions, validatesOnDataErrors)
            | unexpected -> 
                Debug.Fail(sprintf "Unexpected binding type %s" (unexpected.GetType().Name))

            BindingOperations.SetBinding( target, targetProperty.DependencyProperty, binding)
        | _ -> invalidArg "expr" (string this) 

type Binding with
    static member OfExpression(expr, ?mode, ?updateSourceTrigger, ?fallbackValue, ?targetNullValue, ?validatesOnDataErrors, ?validatesOnExceptions) =
        let rec split = function 
            | Sequential(head, tail) -> head :: split tail
            | tail -> [ tail ]

        for e in split expr do
            e.ToBinding(?mode = mode, ?updateSourceTrigger = updateSourceTrigger, ?fallbackValue = fallbackValue, 
                        ?targetNullValue = targetNullValue, ?validatesOnExceptions = validatesOnExceptions, ?validatesOnDataErrors = validatesOnDataErrors)
            |> ignore
    
open System.Windows.Controls.Primitives

type Selector with
    member this.SetBindings(itemsSource : Expr<#seq<'Item>>, ?selectedItem : Expr<'Item>, ?displayMember : Expr<('Item -> _)>) = 

        let e = this.SetBinding(Selector.ItemsSourceProperty, match itemsSource with Source binding -> binding)
        assert not e.HasError

        selectedItem |> Option.iter(fun(Source binding) -> 
            let e = this.SetBinding(Selector.SelectedItemProperty, binding)
            assert not e.HasError
            this.IsSynchronizedWithCurrentItem <- Nullable true
        )

        displayMember |> Option.iter(fun(PropertySelector property) -> 
            this.DisplayMemberPath <- property.Name
        )
        
