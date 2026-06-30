// 
// Copyright (c) 2026 Jun-ichiro Sugisaka
// 
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// 
namespace Aqualis

    module internal TemporaryVariableScope =
        let requireContext() =
            GenerationScope.requireContext()

        let useOne acquire createValue code =
            let name, release = acquire()
            try
                code (createValue name)
            finally
                release()

        let useMany count acquire createValue code =
            if count < 0 then
                invalidArg (nameof count) "The variable count cannot be negative."

            let rec acquireAll remaining acquired =
                if remaining = 0 then
                    List.rev acquired
                else
                    try
                        let name, release = acquire()
                        acquireAll (remaining - 1) ((name, release)::acquired)
                    with _ ->
                        acquired |> List.iter (fun (_, release) -> release())
                        reraise()

            let acquired = acquireAll count []
            try
                acquired
                |> List.map (fst >> createValue)
                |> code
            finally
                acquired
                |> List.rev
                |> List.iter (fun (_, release) -> release())

    type ch =

        static member i code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                context.CurrentProgram.i0.getVar
                (fun name -> num0(Var(It 4, name, NaN), context=context))
                code

        static member d code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                context.CurrentProgram.d0.getVar
                (fun name -> num0(Var(Dt, name, NaN), context=context))
                code

        static member z code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                context.CurrentProgram.z0.getVar
                (fun name -> num0(Var(Zt, name, NaN), context=context))
                code

        static member I name = fun code ->
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                (fun () -> context.CurrentProgram.i0.getVar(name, It 4, A0))
                (fun variableName -> num0(Var(It 4, variableName, NaN), context=context))
                code

        static member D name = fun code ->
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                (fun () -> context.CurrentProgram.d0.getVar(name, Dt, A0))
                (fun variableName -> num0(Var(Dt, variableName, NaN), context=context))
                code

        static member Z name = fun code ->
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useOne
                (fun () -> context.CurrentProgram.z0.getVar(name, Zt, A0))
                (fun variableName -> num0(Var(Zt, variableName, NaN), context=context))
                code

        static member ix (count:int) code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useMany
                count
                context.CurrentProgram.i0.getVar
                (fun name -> num0(Var(It 4, name, NaN), context=context))
                code

        static member dx (count:int) code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useMany
                count
                context.CurrentProgram.d0.getVar
                (fun name -> num0(Var(Dt, name, NaN), context=context))
                code

        static member zx (count:int) code =
            let context = TemporaryVariableScope.requireContext()
            TemporaryVariableScope.useMany
                count
                context.CurrentProgram.z0.getVar
                (fun name -> num0(Var(Zt, name, NaN), context=context))
                code
