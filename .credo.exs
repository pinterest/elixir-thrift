%{configs: [
  %{name: "default",
    files: %{
      included: ["lib/", "test/"],
      excluded: ["_build/", "deps/", "ext/"]
    },
    requires: [],
    check_for_updates: false,

    # You can customize the parameters of any check by adding a second element
    # to the tuple.
    #
    # To disable a check put `false` as second element:
    #
    #     {Credo.Check.Design.DuplicatedCode, false}
    #
    checks: [
      {Credo.Check.Consistency.ExceptionNames},
      {Credo.Check.Consistency.LineEndings},
      {Credo.Check.Consistency.SpaceAroundOperators},
      {Credo.Check.Consistency.SpaceInParentheses},
      {Credo.Check.Consistency.TabsOrSpaces},

      {Credo.Check.Design.AliasUsage, false},
      {Credo.Check.Design.DuplicatedCode, excluded_macros: []},

      {Credo.Check.Readability.FunctionNames},
      {Credo.Check.Readability.LargeNumbers, false},
      {Credo.Check.Readability.MaxLineLength, false},
      {Credo.Check.Readability.ModuleAttributeNames},
      {Credo.Check.Readability.ModuleNames},
      {Credo.Check.Readability.ParenthesesInCondition},
      {Credo.Check.Readability.PredicateFunctionNames},
      {Credo.Check.Readability.Specs, false},
      {Credo.Check.Readability.TrailingBlankLine},
      {Credo.Check.Readability.TrailingWhiteSpace},
      {Credo.Check.Readability.VariableNames},
      {Credo.Check.Readability.RedundantBlankLines},

      {Credo.Check.Refactor.ABCSize, false},
      {Credo.Check.Refactor.CondStatements},
      {Credo.Check.Refactor.MatchInCondition},
      {Credo.Check.Refactor.PipeChainStart, false},
      {Credo.Check.Refactor.CyclomaticComplexity},
      {Credo.Check.Refactor.NegatedConditionsInUnless},
      {Credo.Check.Refactor.NegatedConditionsWithElse},
      {Credo.Check.Refactor.Nesting},
      {Credo.Check.Refactor.UnlessWithElse},

      {Credo.Check.Warning.IExPry},
      {Credo.Check.Warning.IoInspect, false},
      {Credo.Check.Warning.NameRedeclarationByAssignment, false},
      {Credo.Check.Warning.NameRedeclarationByCase, false},
      {Credo.Check.Warning.NameRedeclarationByDef, false},
      {Credo.Check.Warning.NameRedeclarationByFn, false},
      {Credo.Check.Warning.BoolOperationOnSameValues},
      {Credo.Check.Warning.UnusedEnumOperation},
      {Credo.Check.Warning.UnusedKeywordOperation},
      {Credo.Check.Warning.UnusedListOperation},
      {Credo.Check.Warning.UnusedStringOperation},
      {Credo.Check.Warning.UnusedTupleOperation},
      {Credo.Check.Warning.OperationWithConstantResult},
    ]
  }
]}
