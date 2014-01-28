define(['./rnglr'],
/** @param {{Grammar : {fromSerializable : !Function}, Nonterm : !Object, Token : !Object, Rule : !Object}} E */
function(E) {
  const Grammar = E.Grammar;
  const Nonterm = E.Nonterm;
  const Token = E.Token;
  const Rule = E.Rule;

  var g_json = {
    "start": "START",
    "name": "grammar",
    "acceptStates": [
      0,
      1
    ],
    "mode": "RNGLR",
    "derivable": {
      "program": [
        "prelude",
        "prelude_I0_opt",
        "prelude_I1_star",
        "ε",
        "prelude_I0",
        "provide-stmt",
        "prelude_I1",
        "import-stmt",
        "block",
        "stmt",
        "block_I0_star",
        "block_I0",
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "prelude": [
        "prelude_I0_opt",
        "prelude_I1_star",
        "ε",
        "prelude_I0",
        "provide-stmt",
        "prelude_I1",
        "import-stmt"
      ],
      "block": [
        "block_I0_star",
        "ε",
        "block_I0",
        "stmt",
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "end": [],
      "prelude_I0_opt": [
        "ε",
        "prelude_I0",
        "provide-stmt"
      ],
      "prelude_I1_star": [
        "ε",
        "prelude_I1",
        "import-stmt"
      ],
      "prelude_I0": [
        "provide-stmt"
      ],
      "provide-stmt": [],
      "prelude_I1": [
        "import-stmt"
      ],
      "import-stmt": [],
      "import-stmt_I1": [
        "import-name",
        "import-string"
      ],
      "import-name": [],
      "import-string": [],
      "stmt": [
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "block_I0_star": [
        "ε",
        "block_I0",
        "stmt",
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "block_I0": [
        "stmt",
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "let-expr": [],
      "fun-expr": [],
      "data-expr": [],
      "datatype-expr": [],
      "when-expr": [],
      "var-expr": [],
      "assign-expr": [],
      "check-test": [
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "check-expr": [],
      "graph-expr": [],
      "binding": [],
      "binop-expr": [
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "binding_I1_opt": [
        "ε",
        "binding_I1"
      ],
      "binding_I1": [],
      "ann": [
        "name-ann",
        "record-ann",
        "arrow-ann",
        "app-ann",
        "pred-ann",
        "dot-ann"
      ],
      "fun-header": [],
      "doc-string": [
        "doc-string_I0_opt",
        "ε",
        "doc-string_I0"
      ],
      "where-clause": [
        "where-clause_I0_opt",
        "ε",
        "where-clause_I0"
      ],
      "ty-params": [
        "ty-params_I0_opt",
        "ε",
        "ty-params_I0"
      ],
      "args": [],
      "return-ann": [
        "return-ann_I0_opt",
        "ε",
        "return-ann_I0"
      ],
      "ty-params_I0_opt": [
        "ε",
        "ty-params_I0"
      ],
      "ty-params_I0": [],
      "ty-params_I0_I1_star": [
        "ε",
        "ty-params_I0_I1",
        "list-ty-param"
      ],
      "ty-params_I0_I1": [
        "list-ty-param"
      ],
      "list-ty-param": [],
      "args_I0": [],
      "args_I1_opt": [
        "ε",
        "args_I1",
        "binding"
      ],
      "args_I1": [
        "binding"
      ],
      "args_I1_I0_star": [
        "ε",
        "args_I1_I0",
        "list-arg-elt"
      ],
      "args_I1_I0": [
        "list-arg-elt"
      ],
      "list-arg-elt": [],
      "return-ann_I0_opt": [
        "ε",
        "return-ann_I0"
      ],
      "return-ann_I0": [],
      "doc-string_I0_opt": [
        "ε",
        "doc-string_I0"
      ],
      "doc-string_I0": [],
      "where-clause_I0_opt": [
        "ε",
        "where-clause_I0"
      ],
      "where-clause_I0": [],
      "check-op": [],
      "data-mixins": [
        "data-mixins_I0_opt",
        "ε",
        "data-mixins_I0"
      ],
      "data-expr_I5_opt": [
        "ε",
        "data-expr_I5",
        "first-data-variant"
      ],
      "data-expr_I6_star": [
        "ε",
        "data-expr_I6",
        "data-variant"
      ],
      "data-sharing": [
        "data-sharing_I0_opt",
        "ε",
        "data-sharing_I0"
      ],
      "data-expr_I5": [
        "first-data-variant"
      ],
      "first-data-variant": [],
      "data-expr_I6": [
        "data-variant"
      ],
      "data-variant": [],
      "data-mixins_I0_opt": [
        "ε",
        "data-mixins_I0"
      ],
      "data-mixins_I0": [],
      "mixins": [
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "variant-members": [],
      "data-with": [
        "data-with_I0_opt",
        "ε",
        "data-with_I0"
      ],
      "variant-members_I0": [],
      "variant-members_I1_opt": [
        "ε",
        "variant-members_I1",
        "variant-member",
        "binding"
      ],
      "variant-members_I1": [
        "variant-member",
        "binding"
      ],
      "variant-members_I1_I0_star": [
        "ε",
        "variant-members_I1_I0",
        "list-variant-member"
      ],
      "variant-member": [
        "binding"
      ],
      "variant-members_I1_I0": [
        "list-variant-member"
      ],
      "list-variant-member": [],
      "variant-member_I0_opt": [
        "ε",
        "variant-member_I0"
      ],
      "variant-member_I0": [],
      "data-with_I0_opt": [
        "ε",
        "data-with_I0"
      ],
      "data-with_I0": [],
      "fields": [
        "field"
      ],
      "data-sharing_I0_opt": [
        "ε",
        "data-sharing_I0"
      ],
      "data-sharing_I0": [],
      "mixins_I0_star": [
        "ε",
        "mixins_I0",
        "list-mixin"
      ],
      "mixins_I0": [
        "list-mixin"
      ],
      "list-mixin": [],
      "datatype-expr_I4_opt": [
        "ε",
        "datatype-expr_I4",
        "first-datatype-variant"
      ],
      "datatype-expr_I5_star": [
        "ε",
        "datatype-expr_I5",
        "datatype-variant"
      ],
      "datatype-expr_I4": [
        "first-datatype-variant"
      ],
      "first-datatype-variant": [],
      "datatype-expr_I5": [
        "datatype-variant"
      ],
      "datatype-variant": [],
      "constructor-clause": [],
      "constructor-clause_I1": [],
      "graph-expr_I1_star": [
        "ε",
        "graph-expr_I1",
        "let-expr"
      ],
      "graph-expr_I1": [
        "let-expr"
      ],
      "binop-clause": [
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "binop-expr_I1_star": [
        "ε",
        "binop-expr_I1"
      ],
      "binop-expr_I1": [],
      "binop": [],
      "not-expr": [],
      "expr": [
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "paren-expr": [],
      "id-expr": [],
      "prim-expr": [
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "lambda-expr": [],
      "method-expr": [],
      "app-expr": [],
      "left-app-expr": [],
      "obj-expr": [],
      "list-expr": [],
      "dot-expr": [],
      "bracket-expr": [],
      "colon-expr": [],
      "colon-bracket-expr": [],
      "get-bang-expr": [],
      "update-expr": [],
      "extend-expr": [],
      "if-expr": [],
      "cases-expr": [],
      "for-expr": [],
      "try-expr": [],
      "user-block-expr": [],
      "num-expr": [],
      "bool-expr": [],
      "string-expr": [],
      "lambda-expr_I2_opt": [
        "ε",
        "lambda-expr_I2",
        "args"
      ],
      "lambda-expr_I2": [
        "args"
      ],
      "app-args": [],
      "app-args_I1_opt": [
        "ε",
        "app-args_I1",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "app-args_I1": [
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "app-args_I1_I0_star": [
        "ε",
        "app-args_I1_I0",
        "app-arg-elt"
      ],
      "app-args_I1_I0": [
        "app-arg-elt"
      ],
      "app-arg-elt": [],
      "left-app-fun-expr": [
        "id-expr"
      ],
      "obj-fields": [
        "obj-field"
      ],
      "obj-fields_I0_star": [
        "ε",
        "obj-fields_I0",
        "list-obj-field"
      ],
      "obj-field": [],
      "obj-fields_I2_opt": [
        "ε",
        "obj-fields_I2"
      ],
      "obj-fields_I0": [
        "list-obj-field"
      ],
      "list-obj-field": [],
      "obj-fields_I2": [],
      "key": [],
      "obj-field_A1_I2_opt": [
        "ε",
        "obj-field_A1_I2"
      ],
      "obj-field_A1_I2": [],
      "fields_I0_star": [
        "ε",
        "fields_I0",
        "list-field"
      ],
      "field": [],
      "fields_I2_opt": [
        "ε",
        "fields_I2"
      ],
      "fields_I0": [
        "list-field"
      ],
      "list-field": [],
      "fields_I2": [],
      "list-elt": [],
      "list-expr_I1_opt": [
        "ε",
        "list-expr_I1",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "list-expr_I1": [
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ],
      "list-expr_I1_I0_star": [
        "ε",
        "list-expr_I1_I0",
        "list-elt"
      ],
      "list-expr_I1_I0": [
        "list-elt"
      ],
      "if-expr_I4_star": [
        "ε",
        "if-expr_I4",
        "else-if"
      ],
      "if-expr_I5_opt": [
        "ε",
        "if-expr_I5"
      ],
      "if-expr_I4": [
        "else-if"
      ],
      "else-if": [],
      "if-expr_I5": [],
      "cases-expr_I1": [],
      "cases-expr_I6_star": [
        "ε",
        "cases-expr_I6",
        "cases-branch"
      ],
      "cases-expr_I7_opt": [
        "ε",
        "cases-expr_I7"
      ],
      "cases-expr_I6": [
        "cases-branch"
      ],
      "cases-branch": [],
      "cases-expr_I7": [],
      "cases-branch_I2_opt": [
        "ε",
        "cases-branch_I2",
        "args"
      ],
      "cases-branch_I2": [
        "args"
      ],
      "for-bind": [],
      "for-bind-elt": [],
      "for-expr_I3_opt": [
        "ε",
        "for-expr_I3",
        "for-bind"
      ],
      "for-expr_I3": [
        "for-bind"
      ],
      "for-expr_I3_I0_star": [
        "ε",
        "for-expr_I3_I0",
        "for-bind-elt"
      ],
      "for-expr_I3_I0": [
        "for-bind-elt"
      ],
      "try-expr_I3": [],
      "name-ann": [],
      "record-ann": [],
      "arrow-ann": [],
      "app-ann": [],
      "pred-ann": [],
      "dot-ann": [],
      "record-ann_A0_I1_opt": [
        "ε",
        "record-ann_A0_I1",
        "ann-field"
      ],
      "record-ann_A0_I1": [
        "ann-field"
      ],
      "record-ann_A0_I1_I0_star": [
        "ε",
        "record-ann_A0_I1_I0",
        "list-ann-field"
      ],
      "ann-field": [],
      "record-ann_A0_I1_I0": [
        "list-ann-field"
      ],
      "list-ann-field": [],
      "arrow-ann_I0": [],
      "arrow-ann_I1_opt": [
        "ε",
        "arrow-ann_I1",
        "ann",
        "name-ann",
        "record-ann",
        "arrow-ann",
        "app-ann",
        "pred-ann",
        "dot-ann"
      ],
      "arrow-ann_I1": [
        "ann",
        "name-ann",
        "record-ann",
        "arrow-ann",
        "app-ann",
        "pred-ann",
        "dot-ann"
      ],
      "arrow-ann_I1_I0_star": [
        "ε",
        "arrow-ann_I1_I0",
        "arrow-ann-elt"
      ],
      "arrow-ann_I1_I0": [
        "arrow-ann-elt"
      ],
      "arrow-ann-elt": [],
      "app-ann_I0": [
        "name-ann",
        "dot-ann"
      ],
      "app-ann_I2_star": [
        "ε",
        "app-ann_I2",
        "app-ann-elt"
      ],
      "app-ann_I2": [
        "app-ann-elt"
      ],
      "app-ann-elt": [],
      "pred-ann_I1": [],
      "START": [
        "program",
        "ε",
        "prelude",
        "prelude_I0_opt",
        "prelude_I1_star",
        "prelude_I0",
        "provide-stmt",
        "prelude_I1",
        "import-stmt",
        "block",
        "stmt",
        "block_I0_star",
        "block_I0",
        "let-expr",
        "fun-expr",
        "data-expr",
        "datatype-expr",
        "when-expr",
        "var-expr",
        "assign-expr",
        "check-test",
        "check-expr",
        "graph-expr",
        "binop-expr",
        "binop-clause",
        "not-expr",
        "expr",
        "paren-expr",
        "id-expr",
        "prim-expr",
        "lambda-expr",
        "method-expr",
        "app-expr",
        "left-app-expr",
        "obj-expr",
        "list-expr",
        "dot-expr",
        "bracket-expr",
        "colon-expr",
        "colon-bracket-expr",
        "get-bang-expr",
        "update-expr",
        "extend-expr",
        "if-expr",
        "cases-expr",
        "for-expr",
        "try-expr",
        "user-block-expr",
        "num-expr",
        "bool-expr",
        "string-expr"
      ]
    },
    "rulesByOldId": {
      "0": {
        "name": "program",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "1": {
        "name": "end",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "END"
          }
        ]
      },
      "2": {
        "name": "end",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "SEMI"
          }
        ]
      },
      "3": {
        "name": "prelude",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I0_opt"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "4": {
        "name": "prelude_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "5": {
        "name": "prelude_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I0"
          }
        ]
      },
      "6": {
        "name": "prelude_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "provide-stmt"
          }
        ]
      },
      "7": {
        "name": "prelude_I1_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "8": {
        "name": "prelude_I1_star",
        "action": "Rule.ListCons(\"prelude_I1\", \"prelude_I1_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I1"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "9": {
        "name": "prelude_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-stmt"
          }
        ]
      },
      "10": {
        "name": "import-stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "IMPORT"
          },
          {
            "type": "Nonterm",
            "name": "import-stmt_I1"
          },
          {
            "type": "Token",
            "name": "AS"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "11": {
        "name": "import-stmt_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-name"
          }
        ]
      },
      "12": {
        "name": "import-stmt_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-string"
          }
        ]
      },
      "13": {
        "name": "import-name",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "14": {
        "name": "import-string",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "15": {
        "name": "provide-stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PROVIDE"
          },
          {
            "type": "Nonterm",
            "name": "stmt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "16": {
        "name": "provide-stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PROVIDE"
          },
          {
            "type": "Token",
            "name": "STAR"
          }
        ]
      },
      "17": {
        "name": "block",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "block_I0_star"
          }
        ]
      },
      "18": {
        "name": "block_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "19": {
        "name": "block_I0_star",
        "action": "Rule.ListCons(\"block_I0\", \"block_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "block_I0"
          },
          {
            "type": "Nonterm",
            "name": "block_I0_star"
          }
        ]
      },
      "20": {
        "name": "block_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "stmt"
          }
        ]
      },
      "21": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "let-expr"
          }
        ]
      },
      "22": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fun-expr"
          }
        ]
      },
      "23": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr"
          }
        ]
      },
      "24": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr"
          }
        ]
      },
      "25": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "when-expr"
          }
        ]
      },
      "26": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "var-expr"
          }
        ]
      },
      "27": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "assign-expr"
          }
        ]
      },
      "28": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "check-test"
          }
        ]
      },
      "29": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "check-expr"
          }
        ]
      },
      "30": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "graph-expr"
          }
        ]
      },
      "31": {
        "name": "let-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "EQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "32": {
        "name": "binding",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "binding_I1_opt"
          }
        ]
      },
      "33": {
        "name": "binding_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "34": {
        "name": "binding_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding_I1"
          }
        ]
      },
      "35": {
        "name": "binding_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "36": {
        "name": "fun-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "FUN"
          },
          {
            "type": "Nonterm",
            "name": "fun-header"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "37": {
        "name": "fun-header",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          }
        ]
      },
      "38": {
        "name": "ty-params",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0_opt"
          }
        ]
      },
      "39": {
        "name": "ty-params_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "40": {
        "name": "ty-params_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0"
          }
        ]
      },
      "41": {
        "name": "ty-params_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LT"
          },
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1_star"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "42": {
        "name": "ty-params_I0_I1_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "43": {
        "name": "ty-params_I0_I1_star",
        "action": "Rule.ListCons(\"ty-params_I0_I1\", \"ty-params_I0_I1_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1"
          },
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1_star"
          }
        ]
      },
      "44": {
        "name": "ty-params_I0_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-ty-param"
          }
        ]
      },
      "45": {
        "name": "list-ty-param",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "46": {
        "name": "args",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I0"
          },
          {
            "type": "Nonterm",
            "name": "args_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "47": {
        "name": "args_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "48": {
        "name": "args_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "49": {
        "name": "args_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "50": {
        "name": "args_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1"
          }
        ]
      },
      "51": {
        "name": "args_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          }
        ]
      },
      "52": {
        "name": "args_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "53": {
        "name": "args_I1_I0_star",
        "action": "Rule.ListCons(\"args_I1_I0\", \"args_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "args_I1_I0_star"
          }
        ]
      },
      "54": {
        "name": "args_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-arg-elt"
          }
        ]
      },
      "55": {
        "name": "list-arg-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "56": {
        "name": "return-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "return-ann_I0_opt"
          }
        ]
      },
      "57": {
        "name": "return-ann_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "58": {
        "name": "return-ann_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "return-ann_I0"
          }
        ]
      },
      "59": {
        "name": "return-ann_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "THINARROW"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "60": {
        "name": "doc-string",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "doc-string_I0_opt"
          }
        ]
      },
      "61": {
        "name": "doc-string_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "62": {
        "name": "doc-string_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "doc-string_I0"
          }
        ]
      },
      "63": {
        "name": "doc-string_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DOC"
          },
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "64": {
        "name": "where-clause",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "where-clause_I0_opt"
          }
        ]
      },
      "65": {
        "name": "where-clause_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "66": {
        "name": "where-clause_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "where-clause_I0"
          }
        ]
      },
      "67": {
        "name": "where-clause_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "WHERE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "68": {
        "name": "check-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "CHECK"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "69": {
        "name": "check-test",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Nonterm",
            "name": "check-op"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "70": {
        "name": "check-test",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "71": {
        "name": "data-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DATA"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Nonterm",
            "name": "data-mixins"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I5_opt"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I6_star"
          },
          {
            "type": "Nonterm",
            "name": "data-sharing"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "72": {
        "name": "data-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "73": {
        "name": "data-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr_I5"
          }
        ]
      },
      "74": {
        "name": "data-expr_I5",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "first-data-variant"
          }
        ]
      },
      "75": {
        "name": "data-expr_I6_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "76": {
        "name": "data-expr_I6_star",
        "action": "Rule.ListCons(\"data-expr_I6\", \"data-expr_I6_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I6_star"
          }
        ]
      },
      "77": {
        "name": "data-expr_I6",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-variant"
          }
        ]
      },
      "78": {
        "name": "data-mixins",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-mixins_I0_opt"
          }
        ]
      },
      "79": {
        "name": "data-mixins_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "80": {
        "name": "data-mixins_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-mixins_I0"
          }
        ]
      },
      "81": {
        "name": "data-mixins_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DERIVING"
          },
          {
            "type": "Nonterm",
            "name": "mixins"
          }
        ]
      },
      "82": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "83": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "84": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "85": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "86": {
        "name": "variant-members",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I0"
          },
          {
            "type": "Nonterm",
            "name": "variant-members_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "87": {
        "name": "variant-members_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "88": {
        "name": "variant-members_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "89": {
        "name": "variant-members_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "90": {
        "name": "variant-members_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1"
          }
        ]
      },
      "91": {
        "name": "variant-members_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "variant-member"
          }
        ]
      },
      "92": {
        "name": "variant-members_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "93": {
        "name": "variant-members_I1_I0_star",
        "action": "Rule.ListCons(\"variant-members_I1_I0\", \"variant-members_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0_star"
          }
        ]
      },
      "94": {
        "name": "variant-members_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-variant-member"
          }
        ]
      },
      "95": {
        "name": "list-variant-member",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "96": {
        "name": "variant-member",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member_I0_opt"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          }
        ]
      },
      "97": {
        "name": "variant-member_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "98": {
        "name": "variant-member_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member_I0"
          }
        ]
      },
      "99": {
        "name": "variant-member_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "MUTABLE"
          }
        ]
      },
      "100": {
        "name": "variant-member_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "CYCLIC"
          }
        ]
      },
      "101": {
        "name": "data-with",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-with_I0_opt"
          }
        ]
      },
      "102": {
        "name": "data-with_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "103": {
        "name": "data-with_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-with_I0"
          }
        ]
      },
      "104": {
        "name": "data-with_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "WITH"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          }
        ]
      },
      "105": {
        "name": "data-sharing",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-sharing_I0_opt"
          }
        ]
      },
      "106": {
        "name": "data-sharing_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "107": {
        "name": "data-sharing_I0_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-sharing_I0"
          }
        ]
      },
      "108": {
        "name": "data-sharing_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "SHARING"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          }
        ]
      },
      "109": {
        "name": "mixins",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "mixins_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "110": {
        "name": "mixins_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "111": {
        "name": "mixins_I0_star",
        "action": "Rule.ListCons(\"mixins_I0\", \"mixins_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "mixins_I0"
          },
          {
            "type": "Nonterm",
            "name": "mixins_I0_star"
          }
        ]
      },
      "112": {
        "name": "mixins_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-mixin"
          }
        ]
      },
      "113": {
        "name": "list-mixin",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "114": {
        "name": "datatype-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DATATYPE"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I4_opt"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5_star"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "115": {
        "name": "datatype-expr_I4_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "116": {
        "name": "datatype-expr_I4_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr_I4"
          }
        ]
      },
      "117": {
        "name": "datatype-expr_I4",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "first-datatype-variant"
          }
        ]
      },
      "118": {
        "name": "datatype-expr_I5_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "119": {
        "name": "datatype-expr_I5_star",
        "action": "Rule.ListCons(\"datatype-expr_I5\", \"datatype-expr_I5_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5_star"
          }
        ]
      },
      "120": {
        "name": "datatype-expr_I5",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-variant"
          }
        ]
      },
      "121": {
        "name": "first-datatype-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "122": {
        "name": "first-datatype-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "123": {
        "name": "datatype-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "124": {
        "name": "datatype-variant",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "125": {
        "name": "constructor-clause",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "WITHCONSTRUCTOR"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause_I1"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "126": {
        "name": "constructor-clause_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "127": {
        "name": "constructor-clause_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "128": {
        "name": "var-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "VAR"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "EQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "129": {
        "name": "assign-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLONEQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "130": {
        "name": "graph-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "GRAPH"
          },
          {
            "type": "Nonterm",
            "name": "graph-expr_I1_star"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "131": {
        "name": "graph-expr_I1_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "132": {
        "name": "graph-expr_I1_star",
        "action": "Rule.ListCons(\"graph-expr_I1\", \"graph-expr_I1_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "graph-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "graph-expr_I1_star"
          }
        ]
      },
      "133": {
        "name": "graph-expr_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "let-expr"
          }
        ]
      },
      "134": {
        "name": "when-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "WHEN"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "135": {
        "name": "binop-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-clause"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "136": {
        "name": "binop-expr_I1_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "137": {
        "name": "binop-expr_I1_star",
        "action": "Rule.ListCons(\"binop-expr_I1\", \"binop-expr_I1_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "138": {
        "name": "binop-expr_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop"
          },
          {
            "type": "Nonterm",
            "name": "binop-clause"
          }
        ]
      },
      "139": {
        "name": "binop-clause",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "not-expr"
          }
        ]
      },
      "140": {
        "name": "binop-clause",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          }
        ]
      },
      "141": {
        "name": "not-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NOT"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          }
        ]
      },
      "142": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PLUS"
          }
        ]
      },
      "143": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DASH"
          }
        ]
      },
      "144": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "STAR"
          }
        ]
      },
      "145": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "SLASH"
          }
        ]
      },
      "146": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LEQ"
          }
        ]
      },
      "147": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "GEQ"
          }
        ]
      },
      "148": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "EQUALEQUAL"
          }
        ]
      },
      "149": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NEQ"
          }
        ]
      },
      "150": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LT"
          }
        ]
      },
      "151": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "152": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "AND"
          }
        ]
      },
      "153": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "OR"
          }
        ]
      },
      "154": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "IS"
          }
        ]
      },
      "155": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "RAISES"
          }
        ]
      },
      "156": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "SATISFIES"
          }
        ]
      },
      "157": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "paren-expr"
          }
        ]
      },
      "158": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          }
        ]
      },
      "159": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prim-expr"
          }
        ]
      },
      "160": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "lambda-expr"
          }
        ]
      },
      "161": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "method-expr"
          }
        ]
      },
      "162": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-expr"
          }
        ]
      },
      "163": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "left-app-expr"
          }
        ]
      },
      "164": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-expr"
          }
        ]
      },
      "165": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr"
          }
        ]
      },
      "166": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-expr"
          }
        ]
      },
      "167": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "bracket-expr"
          }
        ]
      },
      "168": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "colon-expr"
          }
        ]
      },
      "169": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "colon-bracket-expr"
          }
        ]
      },
      "170": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "get-bang-expr"
          }
        ]
      },
      "171": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "update-expr"
          }
        ]
      },
      "172": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "extend-expr"
          }
        ]
      },
      "173": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr"
          }
        ]
      },
      "174": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr"
          }
        ]
      },
      "175": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr"
          }
        ]
      },
      "176": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "try-expr"
          }
        ]
      },
      "177": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "user-block-expr"
          }
        ]
      },
      "178": {
        "name": "paren-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "179": {
        "name": "id-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "180": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "num-expr"
          }
        ]
      },
      "181": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "bool-expr"
          }
        ]
      },
      "182": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "string-expr"
          }
        ]
      },
      "183": {
        "name": "num-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NUMBER"
          }
        ]
      },
      "184": {
        "name": "num-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "DASH"
          },
          {
            "type": "Token",
            "name": "NUMBER"
          }
        ]
      },
      "185": {
        "name": "bool-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "TRUE"
          }
        ]
      },
      "186": {
        "name": "bool-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "FALSE"
          }
        ]
      },
      "187": {
        "name": "string-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "188": {
        "name": "lambda-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "FUN"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Nonterm",
            "name": "lambda-expr_I2_opt"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "189": {
        "name": "lambda-expr_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "190": {
        "name": "lambda-expr_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "lambda-expr_I2"
          }
        ]
      },
      "191": {
        "name": "lambda-expr_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args"
          }
        ]
      },
      "192": {
        "name": "method-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "METHOD"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "193": {
        "name": "app-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Nonterm",
            "name": "app-args"
          }
        ]
      },
      "194": {
        "name": "app-args",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          },
          {
            "type": "Nonterm",
            "name": "app-args_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "195": {
        "name": "app-args_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "196": {
        "name": "app-args_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1"
          }
        ]
      },
      "197": {
        "name": "app-args_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "198": {
        "name": "app-args_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "199": {
        "name": "app-args_I1_I0_star",
        "action": "Rule.ListCons(\"app-args_I1_I0\", \"app-args_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0_star"
          }
        ]
      },
      "200": {
        "name": "app-args_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-arg-elt"
          }
        ]
      },
      "201": {
        "name": "app-arg-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "202": {
        "name": "left-app-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "CARET"
          },
          {
            "type": "Nonterm",
            "name": "left-app-fun-expr"
          },
          {
            "type": "Nonterm",
            "name": "app-args"
          }
        ]
      },
      "203": {
        "name": "left-app-fun-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          }
        ]
      },
      "204": {
        "name": "left-app-fun-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "205": {
        "name": "obj-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "206": {
        "name": "obj-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "207": {
        "name": "obj-fields",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "obj-field"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I2_opt"
          }
        ]
      },
      "208": {
        "name": "obj-fields_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "209": {
        "name": "obj-fields_I0_star",
        "action": "Rule.ListCons(\"obj-fields_I0\", \"obj-fields_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          }
        ]
      },
      "210": {
        "name": "obj-fields_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-obj-field"
          }
        ]
      },
      "211": {
        "name": "obj-fields_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "212": {
        "name": "obj-fields_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I2"
          }
        ]
      },
      "213": {
        "name": "obj-fields_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "214": {
        "name": "list-obj-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "215": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "216": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "MUTABLE"
          },
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "obj-field_A1_I2_opt"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "217": {
        "name": "obj-field_A1_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "218": {
        "name": "obj-field_A1_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-field_A1_I2"
          }
        ]
      },
      "219": {
        "name": "obj-field_A1_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "220": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "221": {
        "name": "fields",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "field"
          },
          {
            "type": "Nonterm",
            "name": "fields_I2_opt"
          }
        ]
      },
      "222": {
        "name": "fields_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "223": {
        "name": "fields_I0_star",
        "action": "Rule.ListCons(\"fields_I0\", \"fields_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          }
        ]
      },
      "224": {
        "name": "fields_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-field"
          }
        ]
      },
      "225": {
        "name": "fields_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "226": {
        "name": "fields_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I2"
          }
        ]
      },
      "227": {
        "name": "fields_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "228": {
        "name": "list-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "229": {
        "name": "field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "230": {
        "name": "field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "231": {
        "name": "key",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "232": {
        "name": "key",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "233": {
        "name": "list-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "234": {
        "name": "list-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "list-expr_I1_opt"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "235": {
        "name": "list-expr_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "236": {
        "name": "list-expr_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1"
          }
        ]
      },
      "237": {
        "name": "list-expr_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "238": {
        "name": "list-expr_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "239": {
        "name": "list-expr_I1_I0_star",
        "action": "Rule.ListCons(\"list-expr_I1_I0\", \"list-expr_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0_star"
          }
        ]
      },
      "240": {
        "name": "list-expr_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-elt"
          }
        ]
      },
      "241": {
        "name": "dot-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "242": {
        "name": "bracket-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "243": {
        "name": "get-bang-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "BANG"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "244": {
        "name": "colon-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "245": {
        "name": "colon-bracket-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "246": {
        "name": "extend-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "247": {
        "name": "update-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "BANG"
          },
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "248": {
        "name": "if-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "IF"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I4_star"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I5_opt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "249": {
        "name": "if-expr_I4_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "250": {
        "name": "if-expr_I4_star",
        "action": "Rule.ListCons(\"if-expr_I4\", \"if-expr_I4_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr_I4"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I4_star"
          }
        ]
      },
      "251": {
        "name": "if-expr_I4",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "else-if"
          }
        ]
      },
      "252": {
        "name": "if-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "253": {
        "name": "if-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr_I5"
          }
        ]
      },
      "254": {
        "name": "if-expr_I5",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "255": {
        "name": "else-if",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSEIF"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "256": {
        "name": "cases-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "CASES"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I6_star"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I7_opt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "257": {
        "name": "cases-expr_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "258": {
        "name": "cases-expr_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "259": {
        "name": "cases-expr_I6_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "260": {
        "name": "cases-expr_I6_star",
        "action": "Rule.ListCons(\"cases-expr_I6\", \"cases-expr_I6_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I6_star"
          }
        ]
      },
      "261": {
        "name": "cases-expr_I6",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-branch"
          }
        ]
      },
      "262": {
        "name": "cases-expr_I7_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "263": {
        "name": "cases-expr_I7_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr_I7"
          }
        ]
      },
      "264": {
        "name": "cases-expr_I7",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "265": {
        "name": "cases-branch",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "cases-branch_I2_opt"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "266": {
        "name": "cases-branch_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "267": {
        "name": "cases-branch_I2_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-branch_I2"
          }
        ]
      },
      "268": {
        "name": "cases-branch_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args"
          }
        ]
      },
      "269": {
        "name": "for-bind",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "FROM"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "270": {
        "name": "for-bind-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-bind"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "271": {
        "name": "for-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "FOR"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          },
          {
            "type": "Nonterm",
            "name": "for-expr_I3_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "272": {
        "name": "for-expr_I3_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "273": {
        "name": "for-expr_I3_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3"
          }
        ]
      },
      "274": {
        "name": "for-expr_I3",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "for-bind"
          }
        ]
      },
      "275": {
        "name": "for-expr_I3_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "276": {
        "name": "for-expr_I3_I0_star",
        "action": "Rule.ListCons(\"for-expr_I3_I0\", \"for-expr_I3_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0"
          },
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0_star"
          }
        ]
      },
      "277": {
        "name": "for-expr_I3_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-bind-elt"
          }
        ]
      },
      "278": {
        "name": "try-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "TRY"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Token",
            "name": "EXCEPT"
          },
          {
            "type": "Nonterm",
            "name": "try-expr_I3"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "279": {
        "name": "try-expr_I3",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "280": {
        "name": "try-expr_I3",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "281": {
        "name": "user-block-expr",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "BLOCK"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "282": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "name-ann"
          }
        ]
      },
      "283": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann"
          }
        ]
      },
      "284": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann"
          }
        ]
      },
      "285": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann"
          }
        ]
      },
      "286": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "pred-ann"
          }
        ]
      },
      "287": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-ann"
          }
        ]
      },
      "288": {
        "name": "name-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "289": {
        "name": "record-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_opt"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "290": {
        "name": "record-ann_A0_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "291": {
        "name": "record-ann_A0_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1"
          }
        ]
      },
      "292": {
        "name": "record-ann_A0_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "ann-field"
          }
        ]
      },
      "293": {
        "name": "record-ann_A0_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "294": {
        "name": "record-ann_A0_I1_I0_star",
        "action": "Rule.ListCons(\"record-ann_A0_I1_I0\", \"record-ann_A0_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0_star"
          }
        ]
      },
      "295": {
        "name": "record-ann_A0_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-ann-field"
          }
        ]
      },
      "296": {
        "name": "record-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "297": {
        "name": "list-ann-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann-field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "298": {
        "name": "ann-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "299": {
        "name": "ann-field",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "300": {
        "name": "arrow-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I0"
          },
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_opt"
          },
          {
            "type": "Token",
            "name": "THINARROW"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "301": {
        "name": "arrow-ann_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "302": {
        "name": "arrow-ann_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "303": {
        "name": "arrow-ann_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "304": {
        "name": "arrow-ann_I1_opt",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1"
          }
        ]
      },
      "305": {
        "name": "arrow-ann_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "306": {
        "name": "arrow-ann_I1_I0_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "307": {
        "name": "arrow-ann_I1_I0_star",
        "action": "Rule.ListCons(\"arrow-ann_I1_I0\", \"arrow-ann_I1_I0_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0_star"
          }
        ]
      },
      "308": {
        "name": "arrow-ann_I1_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann-elt"
          }
        ]
      },
      "309": {
        "name": "arrow-ann-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "310": {
        "name": "app-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann_I0"
          },
          {
            "type": "Token",
            "name": "LT"
          },
          {
            "type": "Nonterm",
            "name": "app-ann_I2_star"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "311": {
        "name": "app-ann_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "name-ann"
          }
        ]
      },
      "312": {
        "name": "app-ann_I0",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-ann"
          }
        ]
      },
      "313": {
        "name": "app-ann_I2_star",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": []
      },
      "314": {
        "name": "app-ann_I2_star",
        "action": "Rule.ListCons(\"app-ann_I2\", \"app-ann_I2_star\", true)",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann_I2"
          },
          {
            "type": "Nonterm",
            "name": "app-ann_I2_star"
          }
        ]
      },
      "315": {
        "name": "app-ann_I2",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann-elt"
          }
        ]
      },
      "316": {
        "name": "app-ann-elt",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "317": {
        "name": "pred-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Nonterm",
            "name": "pred-ann_I1"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "318": {
        "name": "pred-ann_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "319": {
        "name": "pred-ann_I1",
        "action": "Rule.Inline",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "320": {
        "name": "dot-ann",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "321": {
        "name": "START",
        "action": "Rule.defaultAction",
        "position": 0,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "program"
          }
        ]
      },
      "324": {
        "name": "program",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "325": {
        "name": "prelude",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I0_opt"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "326": {
        "name": "prelude_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I0"
          }
        ]
      },
      "327": {
        "name": "prelude_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "provide-stmt"
          }
        ]
      },
      "328": {
        "name": "prelude_I1_star",
        "action": "Rule.ListCons(\"prelude_I1\", \"prelude_I1_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I1"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "329": {
        "name": "prelude_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-stmt"
          }
        ]
      },
      "332": {
        "name": "binding",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "binding_I1_opt"
          }
        ]
      },
      "333": {
        "name": "id-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "334": {
        "name": "string-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "337": {
        "name": "block_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "stmt"
          }
        ]
      },
      "338": {
        "name": "block",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "block_I0_star"
          }
        ]
      },
      "339": {
        "name": "block_I0_star",
        "action": "Rule.ListCons(\"block_I0\", \"block_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "block_I0"
          },
          {
            "type": "Nonterm",
            "name": "block_I0_star"
          }
        ]
      },
      "340": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "let-expr"
          }
        ]
      },
      "341": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fun-expr"
          }
        ]
      },
      "342": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr"
          }
        ]
      },
      "343": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr"
          }
        ]
      },
      "344": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "when-expr"
          }
        ]
      },
      "345": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "var-expr"
          }
        ]
      },
      "346": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "assign-expr"
          }
        ]
      },
      "347": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "check-test"
          }
        ]
      },
      "348": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "check-expr"
          }
        ]
      },
      "349": {
        "name": "stmt",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "graph-expr"
          }
        ]
      },
      "352": {
        "name": "check-test",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "362": {
        "name": "binop-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-clause"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "363": {
        "name": "binop-clause",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "not-expr"
          }
        ]
      },
      "364": {
        "name": "binop-clause",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          }
        ]
      },
      "376": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "paren-expr"
          }
        ]
      },
      "377": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          }
        ]
      },
      "378": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prim-expr"
          }
        ]
      },
      "379": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "lambda-expr"
          }
        ]
      },
      "380": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "method-expr"
          }
        ]
      },
      "381": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-expr"
          }
        ]
      },
      "382": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "left-app-expr"
          }
        ]
      },
      "383": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-expr"
          }
        ]
      },
      "384": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr"
          }
        ]
      },
      "385": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-expr"
          }
        ]
      },
      "386": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "bracket-expr"
          }
        ]
      },
      "387": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "colon-expr"
          }
        ]
      },
      "388": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "colon-bracket-expr"
          }
        ]
      },
      "389": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "get-bang-expr"
          }
        ]
      },
      "390": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "update-expr"
          }
        ]
      },
      "391": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "extend-expr"
          }
        ]
      },
      "392": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr"
          }
        ]
      },
      "393": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr"
          }
        ]
      },
      "394": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr"
          }
        ]
      },
      "395": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "try-expr"
          }
        ]
      },
      "396": {
        "name": "expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "user-block-expr"
          }
        ]
      },
      "397": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "num-expr"
          }
        ]
      },
      "398": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "bool-expr"
          }
        ]
      },
      "399": {
        "name": "prim-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "string-expr"
          }
        ]
      },
      "400": {
        "name": "num-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NUMBER"
          }
        ]
      },
      "401": {
        "name": "bool-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "TRUE"
          }
        ]
      },
      "402": {
        "name": "bool-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "FALSE"
          }
        ]
      },
      "412": {
        "name": "program",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "413": {
        "name": "prelude",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I0_opt"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "414": {
        "name": "prelude_I1_star",
        "action": "Rule.ListCons(\"prelude_I1\", \"prelude_I1_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "prelude_I1"
          },
          {
            "type": "Nonterm",
            "name": "prelude_I1_star"
          }
        ]
      },
      "416": {
        "name": "import-name",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "417": {
        "name": "import-stmt_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-name"
          }
        ]
      },
      "418": {
        "name": "import-stmt_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "import-string"
          }
        ]
      },
      "419": {
        "name": "import-string",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "420": {
        "name": "binding",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "binding_I1_opt"
          }
        ]
      },
      "421": {
        "name": "binding_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding_I1"
          }
        ]
      },
      "425": {
        "name": "provide-stmt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "PROVIDE"
          },
          {
            "type": "Token",
            "name": "STAR"
          }
        ]
      },
      "426": {
        "name": "block_I0_star",
        "action": "Rule.ListCons(\"block_I0\", \"block_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "block_I0"
          },
          {
            "type": "Nonterm",
            "name": "block_I0_star"
          }
        ]
      },
      "429": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "IS"
          }
        ]
      },
      "430": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "RAISES"
          }
        ]
      },
      "431": {
        "name": "check-op",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "SATISFIES"
          }
        ]
      },
      "435": {
        "name": "ty-params",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0_opt"
          }
        ]
      },
      "436": {
        "name": "ty-params_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0"
          }
        ]
      },
      "443": {
        "name": "graph-expr_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "let-expr"
          }
        ]
      },
      "445": {
        "name": "graph-expr_I1_star",
        "action": "Rule.ListCons(\"graph-expr_I1\", \"graph-expr_I1_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "graph-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "graph-expr_I1_star"
          }
        ]
      },
      "447": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "STAR"
          }
        ]
      },
      "448": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "LT"
          }
        ]
      },
      "449": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "450": {
        "name": "binop-expr",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-clause"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "451": {
        "name": "binop-expr_I1_star",
        "action": "Rule.ListCons(\"binop-expr_I1\", \"binop-expr_I1_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "453": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PLUS"
          }
        ]
      },
      "454": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "DASH"
          }
        ]
      },
      "455": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "SLASH"
          }
        ]
      },
      "456": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "LEQ"
          }
        ]
      },
      "457": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "GEQ"
          }
        ]
      },
      "458": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "EQUALEQUAL"
          }
        ]
      },
      "459": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NEQ"
          }
        ]
      },
      "460": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "AND"
          }
        ]
      },
      "461": {
        "name": "binop",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "OR"
          }
        ]
      },
      "465": {
        "name": "app-expr",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Nonterm",
            "name": "app-args"
          }
        ]
      },
      "472": {
        "name": "not-expr",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NOT"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          }
        ]
      },
      "473": {
        "name": "num-expr",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "DASH"
          },
          {
            "type": "Token",
            "name": "NUMBER"
          }
        ]
      },
      "476": {
        "name": "args_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "477": {
        "name": "args_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "478": {
        "name": "key",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "481": {
        "name": "obj-expr",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "484": {
        "name": "obj-fields_I0_star",
        "action": "Rule.ListCons(\"obj-fields_I0\", \"obj-fields_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          }
        ]
      },
      "485": {
        "name": "obj-fields_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-obj-field"
          }
        ]
      },
      "490": {
        "name": "list-expr_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-elt"
          }
        ]
      },
      "492": {
        "name": "list-expr_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1"
          }
        ]
      },
      "494": {
        "name": "list-expr_I1_I0_star",
        "action": "Rule.ListCons(\"list-expr_I1_I0\", \"list-expr_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0_star"
          }
        ]
      },
      "496": {
        "name": "cases-expr_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "497": {
        "name": "cases-expr_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "503": {
        "name": "name-ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "505": {
        "name": "binding_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "507": {
        "name": "arrow-ann_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "508": {
        "name": "arrow-ann_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "511": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "name-ann"
          }
        ]
      },
      "512": {
        "name": "app-ann_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "name-ann"
          }
        ]
      },
      "513": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann"
          }
        ]
      },
      "514": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann"
          }
        ]
      },
      "515": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann"
          }
        ]
      },
      "516": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "pred-ann"
          }
        ]
      },
      "517": {
        "name": "ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-ann"
          }
        ]
      },
      "518": {
        "name": "app-ann_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "dot-ann"
          }
        ]
      },
      "521": {
        "name": "assign-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLONEQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "522": {
        "name": "provide-stmt",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "PROVIDE"
          },
          {
            "type": "Nonterm",
            "name": "stmt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "523": {
        "name": "end",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "END"
          }
        ]
      },
      "524": {
        "name": "end",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "SEMI"
          }
        ]
      },
      "525": {
        "name": "let-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "EQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "526": {
        "name": "check-test",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Nonterm",
            "name": "check-op"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "529": {
        "name": "lambda-expr_I2",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args"
          }
        ]
      },
      "531": {
        "name": "lambda-expr_I2_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "lambda-expr_I2"
          }
        ]
      },
      "534": {
        "name": "ty-params_I0_I1_star",
        "action": "Rule.ListCons(\"ty-params_I0_I1\", \"ty-params_I0_I1_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1"
          },
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1_star"
          }
        ]
      },
      "535": {
        "name": "ty-params_I0_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-ty-param"
          }
        ]
      },
      "536": {
        "name": "paren-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "537": {
        "name": "check-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "CHECK"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "541": {
        "name": "graph-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "GRAPH"
          },
          {
            "type": "Nonterm",
            "name": "graph-expr_I1_star"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "542": {
        "name": "graph-expr_I1_star",
        "action": "Rule.ListCons(\"graph-expr_I1\", \"graph-expr_I1_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "graph-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "graph-expr_I1_star"
          }
        ]
      },
      "544": {
        "name": "binop-expr_I1_star",
        "action": "Rule.ListCons(\"binop-expr_I1\", \"binop-expr_I1_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr_I1_star"
          }
        ]
      },
      "545": {
        "name": "binop-expr_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop"
          },
          {
            "type": "Nonterm",
            "name": "binop-clause"
          }
        ]
      },
      "546": {
        "name": "colon-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "550": {
        "name": "app-args_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1"
          }
        ]
      },
      "552": {
        "name": "app-args_I1_I0_star",
        "action": "Rule.ListCons(\"app-args_I1_I0\", \"app-args_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0_star"
          }
        ]
      },
      "553": {
        "name": "app-args_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-arg-elt"
          }
        ]
      },
      "554": {
        "name": "left-app-fun-expr",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          }
        ]
      },
      "557": {
        "name": "dot-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "560": {
        "name": "get-bang-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "BANG"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "563": {
        "name": "return-ann",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "return-ann_I0_opt"
          }
        ]
      },
      "564": {
        "name": "return-ann_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "return-ann_I0"
          }
        ]
      },
      "568": {
        "name": "args_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1"
          }
        ]
      },
      "570": {
        "name": "args_I1_I0_star",
        "action": "Rule.ListCons(\"args_I1_I0\", \"args_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "args_I1_I0_star"
          }
        ]
      },
      "571": {
        "name": "args_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-arg-elt"
          }
        ]
      },
      "573": {
        "name": "obj-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "574": {
        "name": "obj-fields",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "obj-field"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I2_opt"
          }
        ]
      },
      "575": {
        "name": "list-obj-field",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "576": {
        "name": "obj-fields_I0_star",
        "action": "Rule.ListCons(\"obj-fields_I0\", \"obj-fields_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          }
        ]
      },
      "580": {
        "name": "list-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "581": {
        "name": "list-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "list-expr_I1_opt"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "582": {
        "name": "list-expr_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "583": {
        "name": "list-expr_I1_I0_star",
        "action": "Rule.ListCons(\"list-expr_I1_I0\", \"list-expr_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "list-expr_I1_I0_star"
          }
        ]
      },
      "588": {
        "name": "user-block-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "BLOCK"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "589": {
        "name": "import-stmt",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "IMPORT"
          },
          {
            "type": "Nonterm",
            "name": "import-stmt_I1"
          },
          {
            "type": "Token",
            "name": "AS"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "591": {
        "name": "pred-ann_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "592": {
        "name": "pred-ann_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "596": {
        "name": "record-ann",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "598": {
        "name": "record-ann_A0_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1"
          }
        ]
      },
      "601": {
        "name": "record-ann_A0_I1_I0_star",
        "action": "Rule.ListCons(\"record-ann_A0_I1_I0\", \"record-ann_A0_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0_star"
          }
        ]
      },
      "602": {
        "name": "record-ann_A0_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-ann-field"
          }
        ]
      },
      "605": {
        "name": "arrow-ann_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1"
          }
        ]
      },
      "607": {
        "name": "arrow-ann_I1_I0_star",
        "action": "Rule.ListCons(\"arrow-ann_I1_I0\", \"arrow-ann_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0_star"
          }
        ]
      },
      "608": {
        "name": "arrow-ann_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann-elt"
          }
        ]
      },
      "611": {
        "name": "doc-string",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "doc-string_I0_opt"
          }
        ]
      },
      "612": {
        "name": "doc-string_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "doc-string_I0"
          }
        ]
      },
      "614": {
        "name": "fun-header",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          }
        ]
      },
      "616": {
        "name": "list-ty-param",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "618": {
        "name": "ty-params_I0_I1_star",
        "action": "Rule.ListCons(\"ty-params_I0_I1\", \"ty-params_I0_I1_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1"
          },
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1_star"
          }
        ]
      },
      "620": {
        "name": "data-mixins",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-mixins_I0_opt"
          }
        ]
      },
      "621": {
        "name": "data-mixins_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-mixins_I0"
          }
        ]
      },
      "624": {
        "name": "var-expr",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "VAR"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "EQUALS"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "627": {
        "name": "app-arg-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "628": {
        "name": "app-args",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          },
          {
            "type": "Nonterm",
            "name": "app-args_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "629": {
        "name": "app-args_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "630": {
        "name": "app-args_I1_I0_star",
        "action": "Rule.ListCons(\"app-args_I1_I0\", \"app-args_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "app-args_I1_I0_star"
          }
        ]
      },
      "632": {
        "name": "left-app-expr",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "CARET"
          },
          {
            "type": "Nonterm",
            "name": "left-app-fun-expr"
          },
          {
            "type": "Nonterm",
            "name": "app-args"
          }
        ]
      },
      "638": {
        "name": "fields_I0_star",
        "action": "Rule.ListCons(\"fields_I0\", \"fields_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          }
        ]
      },
      "639": {
        "name": "fields_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-field"
          }
        ]
      },
      "643": {
        "name": "return-ann_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "THINARROW"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "644": {
        "name": "list-arg-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "645": {
        "name": "args",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I0"
          },
          {
            "type": "Nonterm",
            "name": "args_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "646": {
        "name": "args_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          }
        ]
      },
      "647": {
        "name": "args_I1_I0_star",
        "action": "Rule.ListCons(\"args_I1_I0\", \"args_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "args_I1_I0_star"
          }
        ]
      },
      "650": {
        "name": "obj-field_A1_I2_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-field_A1_I2"
          }
        ]
      },
      "651": {
        "name": "obj-fields_I2",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "652": {
        "name": "obj-fields",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "obj-field"
          },
          {
            "type": "Nonterm",
            "name": "obj-fields_I2_opt"
          }
        ]
      },
      "653": {
        "name": "obj-fields_I2_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "obj-fields_I2"
          }
        ]
      },
      "654": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "656": {
        "name": "key",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "661": {
        "name": "for-expr_I3_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-bind-elt"
          }
        ]
      },
      "663": {
        "name": "for-expr_I3_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3"
          }
        ]
      },
      "665": {
        "name": "for-expr_I3_I0_star",
        "action": "Rule.ListCons(\"for-expr_I3_I0\", \"for-expr_I3_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0"
          },
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0_star"
          }
        ]
      },
      "666": {
        "name": "try-expr_I3",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "667": {
        "name": "try-expr_I3",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "669": {
        "name": "dot-ann",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "673": {
        "name": "record-ann",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_opt"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "674": {
        "name": "record-ann_A0_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "ann-field"
          }
        ]
      },
      "675": {
        "name": "list-ann-field",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann-field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "676": {
        "name": "record-ann_A0_I1_I0_star",
        "action": "Rule.ListCons(\"record-ann_A0_I1_I0\", \"record-ann_A0_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "record-ann_A0_I1_I0_star"
          }
        ]
      },
      "677": {
        "name": "arrow-ann-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "679": {
        "name": "arrow-ann_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "680": {
        "name": "arrow-ann_I1_I0_star",
        "action": "Rule.ListCons(\"arrow-ann_I1_I0\", \"arrow-ann_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_I0_star"
          }
        ]
      },
      "683": {
        "name": "app-ann_I2_star",
        "action": "Rule.ListCons(\"app-ann_I2\", \"app-ann_I2_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann_I2"
          },
          {
            "type": "Nonterm",
            "name": "app-ann_I2_star"
          }
        ]
      },
      "684": {
        "name": "app-ann_I2",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann-elt"
          }
        ]
      },
      "686": {
        "name": "doc-string_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "DOC"
          },
          {
            "type": "Token",
            "name": "STRING"
          }
        ]
      },
      "687": {
        "name": "fun-header",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          }
        ]
      },
      "689": {
        "name": "ty-params_I0",
        "action": "Rule.Inline",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "LT"
          },
          {
            "type": "Nonterm",
            "name": "ty-params_I0_I1_star"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "692": {
        "name": "data-mixins_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "DERIVING"
          },
          {
            "type": "Nonterm",
            "name": "mixins"
          }
        ]
      },
      "694": {
        "name": "mixins_I0_star",
        "action": "Rule.ListCons(\"mixins_I0\", \"mixins_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "mixins_I0"
          },
          {
            "type": "Nonterm",
            "name": "mixins_I0_star"
          }
        ]
      },
      "695": {
        "name": "mixins_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-mixin"
          }
        ]
      },
      "699": {
        "name": "datatype-expr_I4_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr_I4"
          }
        ]
      },
      "700": {
        "name": "datatype-expr_I4",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "first-datatype-variant"
          }
        ]
      },
      "701": {
        "name": "when-expr",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Token",
            "name": "WHEN"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "702": {
        "name": "colon-bracket-expr",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "703": {
        "name": "left-app-fun-expr",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "id-expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "NAME"
          }
        ]
      },
      "704": {
        "name": "extend-expr",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "707": {
        "name": "fields",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "field"
          },
          {
            "type": "Nonterm",
            "name": "fields_I2_opt"
          }
        ]
      },
      "708": {
        "name": "list-field",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "field"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "709": {
        "name": "fields_I0_star",
        "action": "Rule.ListCons(\"fields_I0\", \"fields_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0"
          },
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          }
        ]
      },
      "710": {
        "name": "bracket-expr",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "DOT"
          },
          {
            "type": "Token",
            "name": "LBRACK"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RBRACK"
          }
        ]
      },
      "711": {
        "name": "update-expr",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "BANG"
          },
          {
            "type": "Token",
            "name": "LBRACE"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          },
          {
            "type": "Token",
            "name": "RBRACE"
          }
        ]
      },
      "713": {
        "name": "obj-field_A1_I2",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "717": {
        "name": "if-expr_I4_star",
        "action": "Rule.ListCons(\"if-expr_I4\", \"if-expr_I4_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr_I4"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I4_star"
          }
        ]
      },
      "718": {
        "name": "if-expr_I4",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "else-if"
          }
        ]
      },
      "722": {
        "name": "for-bind-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-bind"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "724": {
        "name": "for-expr_I3",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "for-bind"
          }
        ]
      },
      "725": {
        "name": "for-expr_I3_I0_star",
        "action": "Rule.ListCons(\"for-expr_I3_I0\", \"for-expr_I3_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0"
          },
          {
            "type": "Nonterm",
            "name": "for-expr_I3_I0_star"
          }
        ]
      },
      "727": {
        "name": "pred-ann",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Nonterm",
            "name": "pred-ann_I1"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "728": {
        "name": "ann-field",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLONCOLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "729": {
        "name": "ann-field",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          }
        ]
      },
      "731": {
        "name": "app-ann-elt",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "733": {
        "name": "app-ann_I2_star",
        "action": "Rule.ListCons(\"app-ann_I2\", \"app-ann_I2_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann_I2"
          },
          {
            "type": "Nonterm",
            "name": "app-ann_I2_star"
          }
        ]
      },
      "735": {
        "name": "where-clause",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "where-clause_I0_opt"
          }
        ]
      },
      "736": {
        "name": "where-clause_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "where-clause_I0"
          }
        ]
      },
      "737": {
        "name": "where-clause_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "WHERE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "740": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "742": {
        "name": "data-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr_I5"
          }
        ]
      },
      "743": {
        "name": "data-expr_I5",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "first-data-variant"
          }
        ]
      },
      "744": {
        "name": "list-mixin",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "745": {
        "name": "mixins",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "mixins_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "746": {
        "name": "mixins_I0_star",
        "action": "Rule.ListCons(\"mixins_I0\", \"mixins_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "mixins_I0"
          },
          {
            "type": "Nonterm",
            "name": "mixins_I0_star"
          }
        ]
      },
      "747": {
        "name": "variant-members_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "748": {
        "name": "variant-members_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "751": {
        "name": "first-datatype-variant",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "756": {
        "name": "datatype-expr_I5_star",
        "action": "Rule.ListCons(\"datatype-expr_I5\", \"datatype-expr_I5_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5_star"
          }
        ]
      },
      "757": {
        "name": "datatype-expr_I5",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-variant"
          }
        ]
      },
      "758": {
        "name": "field",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "760": {
        "name": "fields_I2",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "761": {
        "name": "fields",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "field"
          },
          {
            "type": "Nonterm",
            "name": "fields_I2_opt"
          }
        ]
      },
      "762": {
        "name": "fields_I2_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "fields_I2"
          }
        ]
      },
      "764": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Token",
            "name": "MUTABLE"
          },
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "obj-field_A1_I2_opt"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "767": {
        "name": "if-expr_I5_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr_I5"
          }
        ]
      },
      "768": {
        "name": "if-expr_I5",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "769": {
        "name": "if-expr_I4_star",
        "action": "Rule.ListCons(\"if-expr_I4\", \"if-expr_I4_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "if-expr_I4"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I4_star"
          }
        ]
      },
      "772": {
        "name": "for-bind",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "FROM"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          }
        ]
      },
      "775": {
        "name": "arrow-ann",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "arrow-ann_I0"
          },
          {
            "type": "Nonterm",
            "name": "arrow-ann_I1_opt"
          },
          {
            "type": "Token",
            "name": "THINARROW"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "776": {
        "name": "app-ann",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "app-ann_I0"
          },
          {
            "type": "Token",
            "name": "LT"
          },
          {
            "type": "Nonterm",
            "name": "app-ann_I2_star"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "GT"
          }
        ]
      },
      "777": {
        "name": "fun-expr",
        "action": "Rule.defaultAction",
        "position": 7,
        "symbols": [
          {
            "type": "Token",
            "name": "FUN"
          },
          {
            "type": "Nonterm",
            "name": "fun-header"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "778": {
        "name": "where-clause_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "WHERE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "780": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "781": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "782": {
        "name": "data-with",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-with_I0_opt"
          }
        ]
      },
      "783": {
        "name": "data-with_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-with_I0"
          }
        ]
      },
      "786": {
        "name": "data-expr_I6_star",
        "action": "Rule.ListCons(\"data-expr_I6\", \"data-expr_I6_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I6_star"
          }
        ]
      },
      "787": {
        "name": "data-expr_I6",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-variant"
          }
        ]
      },
      "790": {
        "name": "first-datatype-variant",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "792": {
        "name": "variant-members_I1_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1"
          }
        ]
      },
      "795": {
        "name": "variant-members_I1_I0_star",
        "action": "Rule.ListCons(\"variant-members_I1_I0\", \"variant-members_I1_I0_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0_star"
          }
        ]
      },
      "796": {
        "name": "variant-members_I1_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "list-variant-member"
          }
        ]
      },
      "798": {
        "name": "variant-member_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member_I0"
          }
        ]
      },
      "799": {
        "name": "variant-member_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "MUTABLE"
          }
        ]
      },
      "800": {
        "name": "variant-member_I0",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "CYCLIC"
          }
        ]
      },
      "801": {
        "name": "constructor-clause_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENSPACE"
          }
        ]
      },
      "802": {
        "name": "constructor-clause_I1",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          }
        ]
      },
      "807": {
        "name": "datatype-expr_I5_star",
        "action": "Rule.ListCons(\"datatype-expr_I5\", \"datatype-expr_I5_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5_star"
          }
        ]
      },
      "811": {
        "name": "if-expr",
        "action": "Rule.defaultAction",
        "position": 7,
        "symbols": [
          {
            "type": "Token",
            "name": "IF"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I4_star"
          },
          {
            "type": "Nonterm",
            "name": "if-expr_I5_opt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "812": {
        "name": "if-expr_I5",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "813": {
        "name": "else-if",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSEIF"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "816": {
        "name": "cases-expr_I6_star",
        "action": "Rule.ListCons(\"cases-expr_I6\", \"cases-expr_I6_star\", true)",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I6_star"
          }
        ]
      },
      "817": {
        "name": "cases-expr_I6",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-branch"
          }
        ]
      },
      "821": {
        "name": "first-data-variant",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "822": {
        "name": "data-with_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "WITH"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          }
        ]
      },
      "824": {
        "name": "data-sharing",
        "action": "Rule.defaultAction",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-sharing_I0_opt"
          }
        ]
      },
      "825": {
        "name": "data-sharing_I0_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-sharing_I0"
          }
        ]
      },
      "827": {
        "name": "data-expr_I6_star",
        "action": "Rule.ListCons(\"data-expr_I6\", \"data-expr_I6_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "data-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I6_star"
          }
        ]
      },
      "829": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "830": {
        "name": "variant-members",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I0"
          },
          {
            "type": "Nonterm",
            "name": "variant-members_I1_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          }
        ]
      },
      "831": {
        "name": "variant-members_I1",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0_star"
          },
          {
            "type": "Nonterm",
            "name": "variant-member"
          }
        ]
      },
      "832": {
        "name": "list-variant-member",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member"
          },
          {
            "type": "Token",
            "name": "COMMA"
          }
        ]
      },
      "833": {
        "name": "variant-members_I1_I0_star",
        "action": "Rule.ListCons(\"variant-members_I1_I0\", \"variant-members_I1_I0_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0"
          },
          {
            "type": "Nonterm",
            "name": "variant-members_I1_I0_star"
          }
        ]
      },
      "834": {
        "name": "variant-member",
        "action": "Rule.defaultAction",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "variant-member_I0_opt"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          }
        ]
      },
      "837": {
        "name": "datatype-variant",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "838": {
        "name": "datatype-expr",
        "action": "Rule.defaultAction",
        "position": 8,
        "symbols": [
          {
            "type": "Token",
            "name": "DATATYPE"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I4_opt"
          },
          {
            "type": "Nonterm",
            "name": "datatype-expr_I5_star"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "840": {
        "name": "method-expr",
        "action": "Rule.defaultAction",
        "position": 8,
        "symbols": [
          {
            "type": "Token",
            "name": "METHOD"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "842": {
        "name": "else-if",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "ELSEIF"
          },
          {
            "type": "Nonterm",
            "name": "binop-expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "846": {
        "name": "cases-expr_I7_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr_I7"
          }
        ]
      },
      "847": {
        "name": "cases-expr_I6_star",
        "action": "Rule.ListCons(\"cases-expr_I6\", \"cases-expr_I6_star\", true)",
        "position": 2,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-expr_I6"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I6_star"
          }
        ]
      },
      "850": {
        "name": "lambda-expr",
        "action": "Rule.defaultAction",
        "position": 9,
        "symbols": [
          {
            "type": "Token",
            "name": "FUN"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Nonterm",
            "name": "lambda-expr_I2_opt"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "852": {
        "name": "data-sharing_I0",
        "action": "Rule.Inline",
        "position": 2,
        "symbols": [
          {
            "type": "Token",
            "name": "SHARING"
          },
          {
            "type": "Nonterm",
            "name": "fields"
          }
        ]
      },
      "853": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "854": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "856": {
        "name": "datatype-variant",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause"
          }
        ]
      },
      "858": {
        "name": "obj-field",
        "action": "Rule.defaultAction",
        "position": 8,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "859": {
        "name": "cases-branch_I2",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "args"
          }
        ]
      },
      "861": {
        "name": "cases-branch_I2_opt",
        "action": "Rule.Inline",
        "position": 1,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "cases-branch_I2"
          }
        ]
      },
      "863": {
        "name": "cases-expr",
        "action": "Rule.defaultAction",
        "position": 9,
        "symbols": [
          {
            "type": "Token",
            "name": "CASES"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I1"
          },
          {
            "type": "Nonterm",
            "name": "ann"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I6_star"
          },
          {
            "type": "Nonterm",
            "name": "cases-expr_I7_opt"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "864": {
        "name": "for-expr",
        "action": "Rule.defaultAction",
        "position": 9,
        "symbols": [
          {
            "type": "Token",
            "name": "FOR"
          },
          {
            "type": "Nonterm",
            "name": "expr"
          },
          {
            "type": "Token",
            "name": "PARENNOSPACE"
          },
          {
            "type": "Nonterm",
            "name": "for-expr_I3_opt"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "865": {
        "name": "try-expr",
        "action": "Rule.defaultAction",
        "position": 9,
        "symbols": [
          {
            "type": "Token",
            "name": "TRY"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Token",
            "name": "EXCEPT"
          },
          {
            "type": "Nonterm",
            "name": "try-expr_I3"
          },
          {
            "type": "Nonterm",
            "name": "binding"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "866": {
        "name": "data-expr",
        "action": "Rule.defaultAction",
        "position": 10,
        "symbols": [
          {
            "type": "Token",
            "name": "DATA"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "ty-params"
          },
          {
            "type": "Nonterm",
            "name": "data-mixins"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I5_opt"
          },
          {
            "type": "Nonterm",
            "name": "data-expr_I6_star"
          },
          {
            "type": "Nonterm",
            "name": "data-sharing"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "867": {
        "name": "data-variant",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "variant-members"
          },
          {
            "type": "Nonterm",
            "name": "data-with"
          }
        ]
      },
      "870": {
        "name": "cases-branch",
        "action": "Rule.defaultAction",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "cases-branch_I2_opt"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "871": {
        "name": "cases-expr_I7",
        "action": "Rule.Inline",
        "position": 3,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "873": {
        "name": "field",
        "action": "Rule.defaultAction",
        "position": 8,
        "symbols": [
          {
            "type": "Nonterm",
            "name": "key"
          },
          {
            "type": "Nonterm",
            "name": "args"
          },
          {
            "type": "Nonterm",
            "name": "return-ann"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "doc-string"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "where-clause"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "874": {
        "name": "cases-branch",
        "action": "Rule.defaultAction",
        "position": 5,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Nonterm",
            "name": "cases-branch_I2_opt"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "875": {
        "name": "cases-expr_I7",
        "action": "Rule.Inline",
        "position": 4,
        "symbols": [
          {
            "type": "Token",
            "name": "BAR"
          },
          {
            "type": "Token",
            "name": "ELSE"
          },
          {
            "type": "Token",
            "name": "THICKARROW"
          },
          {
            "type": "Nonterm",
            "name": "block"
          }
        ]
      },
      "876": {
        "name": "constructor-clause",
        "action": "Rule.defaultAction",
        "position": 7,
        "symbols": [
          {
            "type": "Token",
            "name": "WITHCONSTRUCTOR"
          },
          {
            "type": "Nonterm",
            "name": "constructor-clause_I1"
          },
          {
            "type": "Token",
            "name": "NAME"
          },
          {
            "type": "Token",
            "name": "RPAREN"
          },
          {
            "type": "Token",
            "name": "COLON"
          },
          {
            "type": "Nonterm",
            "name": "block"
          },
          {
            "type": "Nonterm",
            "name": "end"
          }
        ]
      },
      "879": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 3
      },
      "880": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 3
      },
      "881": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 3
      },
      "882": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 3
      },
      "883": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 3
      },
      "884": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 3
      },
      "885": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 3
      },
      "886": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 3
      },
      "887": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 3
      },
      "888": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 3
      },
      "889": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 3
      },
      "890": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 3
      },
      "891": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 3
      },
      "892": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 3
      },
      "893": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 3
      },
      "894": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 3
      },
      "895": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 3
      },
      "896": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 3
      },
      "897": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 3
      },
      "898": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 3
      },
      "899": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 3
      },
      "900": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 3
      },
      "901": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 3
      },
      "903": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 4
      },
      "904": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 4
      },
      "907": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 4
      },
      "909": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 4
      },
      "911": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 4
      },
      "913": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 4
      },
      "915": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 4
      },
      "917": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 4
      },
      "919": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 4
      },
      "921": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 4
      },
      "923": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 4
      },
      "925": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 4
      },
      "927": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 4
      },
      "929": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 4
      },
      "931": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 4
      },
      "933": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 4
      },
      "935": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 4
      },
      "937": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 4
      },
      "939": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 4
      },
      "941": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 4
      },
      "943": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 4
      },
      "945": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 4
      },
      "947": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 4
      },
      "949": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 4
      },
      "4080": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 38
      },
      "4081": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 39
      },
      "4085": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 38
      },
      "4086": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 38
      },
      "4087": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 38
      },
      "4088": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 38
      },
      "4089": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 39
      },
      "4091": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 39
      },
      "4093": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 39
      },
      "4095": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 39
      },
      "4157": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 17
      },
      "4158": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 17
      },
      "4159": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 18
      },
      "4161": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 18
      },
      "4169": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 131
      },
      "4170": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 131
      },
      "4423": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 208
      },
      "4424": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 208
      },
      "4425": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 208
      },
      "4446": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 235
      },
      "4449": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 238
      },
      "4450": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 238
      },
      "4451": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 238
      },
      "4452": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 238
      },
      "4453": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 238
      },
      "4454": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 238
      },
      "4455": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 238
      },
      "4456": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 238
      },
      "4457": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 238
      },
      "4458": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 238
      },
      "4459": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 238
      },
      "4460": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 238
      },
      "4461": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 238
      },
      "4462": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 238
      },
      "4463": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 238
      },
      "4464": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 238
      },
      "4465": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 238
      },
      "4583": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 17
      },
      "4584": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 18
      },
      "4727": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 189
      },
      "4728": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 189
      },
      "4734": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 42
      },
      "4741": {
        "lookahead": {
          "type": "Token",
          "name": "DERIVING"
        },
        "like": 38
      },
      "4742": {
        "lookahead": {
          "type": "Token",
          "name": "DERIVING"
        },
        "like": 39
      },
      "4758": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 195
      },
      "4761": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 198
      },
      "4762": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 198
      },
      "4763": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 198
      },
      "4764": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 198
      },
      "4765": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 198
      },
      "4766": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 198
      },
      "4767": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 198
      },
      "4768": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 198
      },
      "4769": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 198
      },
      "4770": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 198
      },
      "4771": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 198
      },
      "4772": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 198
      },
      "4773": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 198
      },
      "4774": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 198
      },
      "4775": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 198
      },
      "4776": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 198
      },
      "4777": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 198
      },
      "4839": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 56
      },
      "4840": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 57
      },
      "4844": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 49
      },
      "4847": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 52
      },
      "4989": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 290
      },
      "4992": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 293
      },
      "5000": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 303
      },
      "5003": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 306
      },
      "5004": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 306
      },
      "5005": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 306
      },
      "5006": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 306
      },
      "5034": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 60
      },
      "5035": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 60
      },
      "5036": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 60
      },
      "5037": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 60
      },
      "5038": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 60
      },
      "5039": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 60
      },
      "5040": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 60
      },
      "5041": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 60
      },
      "5042": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 60
      },
      "5043": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 60
      },
      "5044": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 60
      },
      "5045": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 60
      },
      "5046": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 60
      },
      "5047": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 60
      },
      "5048": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 60
      },
      "5049": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 60
      },
      "5050": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 60
      },
      "5051": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 60
      },
      "5052": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 60
      },
      "5053": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 60
      },
      "5054": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 60
      },
      "5055": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 60
      },
      "5056": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 60
      },
      "5057": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 60
      },
      "5058": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 60
      },
      "5059": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 60
      },
      "5060": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 61
      },
      "5062": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 61
      },
      "5064": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 61
      },
      "5066": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 61
      },
      "5068": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 61
      },
      "5070": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 61
      },
      "5072": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 61
      },
      "5074": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 61
      },
      "5076": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 61
      },
      "5078": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 61
      },
      "5080": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 61
      },
      "5082": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 61
      },
      "5084": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 61
      },
      "5086": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 61
      },
      "5088": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 61
      },
      "5090": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 61
      },
      "5092": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 61
      },
      "5094": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 61
      },
      "5096": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 61
      },
      "5098": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 61
      },
      "5100": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 61
      },
      "5102": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 61
      },
      "5104": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 61
      },
      "5106": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 61
      },
      "5108": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 61
      },
      "5110": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 61
      },
      "5149": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 78
      },
      "5150": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 79
      },
      "5170": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 222
      },
      "5171": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 222
      },
      "5194": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 217
      },
      "5205": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 17
      },
      "5206": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 17
      },
      "5207": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 18
      },
      "5209": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 18
      },
      "5365": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 272
      },
      "5368": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 275
      },
      "5400": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 313
      },
      "5401": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 313
      },
      "5402": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 313
      },
      "5403": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 313
      },
      "5417": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 17
      },
      "5418": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 18
      },
      "5507": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 110
      },
      "5508": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 110
      },
      "5509": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 110
      },
      "5510": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 110
      },
      "5511": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 110
      },
      "5512": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 110
      },
      "5513": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 110
      },
      "5514": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 110
      },
      "5515": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 110
      },
      "5516": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 110
      },
      "5517": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 110
      },
      "5518": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 110
      },
      "5519": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 110
      },
      "5520": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 110
      },
      "5521": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 110
      },
      "5522": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 110
      },
      "5523": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 110
      },
      "5576": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 115
      },
      "5577": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 115
      },
      "5578": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 115
      },
      "5579": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 115
      },
      "5619": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 249
      },
      "5620": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 249
      },
      "5621": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 249
      },
      "5672": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 64
      },
      "5673": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 64
      },
      "5674": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 65
      },
      "5676": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 65
      },
      "5682": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 72
      },
      "5683": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 72
      },
      "5684": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 72
      },
      "5685": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 72
      },
      "5686": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 72
      },
      "5727": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 118
      },
      "5728": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 118
      },
      "5729": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 118
      },
      "5756": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 252
      },
      "5757": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 252
      },
      "5788": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 75
      },
      "5789": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 75
      },
      "5790": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 75
      },
      "5791": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 75
      },
      "5813": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 89
      },
      "5816": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 92
      },
      "5817": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 92
      },
      "5818": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 92
      },
      "5829": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 97
      },
      "5852": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 259
      },
      "5853": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 259
      },
      "5854": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 259
      },
      "5871": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 105
      },
      "5872": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 105
      },
      "5873": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 105
      },
      "5874": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 106
      },
      "5876": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 106
      },
      "5878": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 106
      },
      "5911": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 262
      },
      "5912": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 262
      },
      "5934": {
        "lookahead": {
          "type": "Token",
          "name": "THICKARROW"
        },
        "like": 266
      },
      "5954": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 324
      },
      "5955": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 325
      },
      "5956": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 325
      },
      "5957": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 325
      },
      "5958": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 325
      },
      "5959": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 325
      },
      "5960": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 325
      },
      "5961": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 325
      },
      "5962": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 325
      },
      "5963": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 325
      },
      "5964": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 325
      },
      "5965": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 325
      },
      "5966": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 325
      },
      "5967": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 325
      },
      "5968": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 325
      },
      "5969": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 325
      },
      "5970": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 325
      },
      "5971": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 325
      },
      "5972": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 325
      },
      "5973": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 325
      },
      "5974": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 325
      },
      "5975": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 325
      },
      "5976": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 325
      },
      "5977": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 325
      },
      "5978": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 325
      },
      "5979": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 326
      },
      "5980": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 326
      },
      "5981": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 326
      },
      "5982": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 326
      },
      "5983": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 326
      },
      "5984": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 326
      },
      "5985": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 326
      },
      "5986": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 326
      },
      "5987": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 326
      },
      "5988": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 326
      },
      "5989": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 326
      },
      "5990": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 326
      },
      "5991": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 326
      },
      "5992": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 326
      },
      "5993": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 326
      },
      "5994": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 326
      },
      "5995": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 326
      },
      "5996": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 326
      },
      "5997": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 326
      },
      "5998": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 326
      },
      "5999": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 326
      },
      "6000": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 326
      },
      "6001": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 326
      },
      "6002": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 326
      },
      "6003": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 326
      },
      "6004": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 327
      },
      "6005": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 327
      },
      "6006": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 327
      },
      "6007": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 327
      },
      "6008": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 327
      },
      "6009": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 327
      },
      "6010": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 327
      },
      "6011": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 327
      },
      "6012": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 327
      },
      "6013": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 327
      },
      "6014": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 327
      },
      "6015": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 327
      },
      "6016": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 327
      },
      "6017": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 327
      },
      "6018": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 327
      },
      "6019": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 327
      },
      "6020": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 327
      },
      "6021": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 327
      },
      "6022": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 327
      },
      "6023": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 327
      },
      "6024": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 327
      },
      "6025": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 327
      },
      "6026": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 327
      },
      "6027": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 327
      },
      "6028": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 327
      },
      "6029": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 328
      },
      "6030": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 328
      },
      "6031": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 328
      },
      "6032": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 328
      },
      "6033": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 328
      },
      "6034": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 328
      },
      "6035": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 328
      },
      "6036": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 328
      },
      "6037": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 328
      },
      "6038": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 328
      },
      "6039": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 328
      },
      "6040": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 328
      },
      "6041": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 328
      },
      "6042": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 328
      },
      "6043": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 328
      },
      "6044": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 328
      },
      "6045": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 328
      },
      "6046": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 328
      },
      "6047": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 328
      },
      "6048": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 328
      },
      "6049": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 328
      },
      "6050": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 328
      },
      "6051": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 328
      },
      "6052": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 328
      },
      "6053": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 329
      },
      "6054": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 329
      },
      "6055": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 329
      },
      "6056": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 329
      },
      "6057": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 329
      },
      "6058": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 329
      },
      "6059": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 329
      },
      "6060": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 329
      },
      "6061": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 329
      },
      "6062": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 329
      },
      "6063": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 329
      },
      "6064": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 329
      },
      "6065": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 329
      },
      "6066": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 329
      },
      "6067": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 329
      },
      "6068": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 329
      },
      "6069": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 329
      },
      "6070": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 329
      },
      "6071": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 329
      },
      "6072": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 329
      },
      "6073": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 329
      },
      "6074": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 329
      },
      "6075": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 329
      },
      "6076": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 329
      },
      "6077": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 329
      },
      "6134": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 332
      },
      "6135": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 333
      },
      "6136": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 333
      },
      "6137": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 333
      },
      "6138": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 333
      },
      "6139": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 333
      },
      "6140": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 333
      },
      "6141": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 333
      },
      "6142": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 333
      },
      "6143": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 333
      },
      "6144": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 333
      },
      "6145": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 333
      },
      "6146": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 333
      },
      "6147": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 333
      },
      "6148": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 333
      },
      "6149": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 333
      },
      "6150": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 333
      },
      "6151": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 333
      },
      "6152": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 333
      },
      "6153": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 333
      },
      "6154": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 333
      },
      "6155": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 333
      },
      "6156": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 333
      },
      "6157": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 333
      },
      "6158": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 333
      },
      "6159": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 333
      },
      "6160": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 333
      },
      "6161": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 333
      },
      "6162": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 333
      },
      "6163": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 333
      },
      "6164": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 333
      },
      "6165": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 333
      },
      "6166": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 333
      },
      "6167": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 333
      },
      "6168": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 333
      },
      "6169": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 333
      },
      "6170": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 333
      },
      "6171": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 333
      },
      "6172": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 333
      },
      "6173": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 333
      },
      "6174": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 333
      },
      "6175": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 333
      },
      "6176": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 333
      },
      "6177": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 333
      },
      "6178": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 333
      },
      "6179": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 333
      },
      "6180": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 333
      },
      "6181": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 333
      },
      "6182": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 333
      },
      "6183": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 333
      },
      "6184": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 333
      },
      "6185": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 334
      },
      "6186": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 334
      },
      "6187": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 334
      },
      "6188": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 334
      },
      "6189": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 334
      },
      "6190": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 334
      },
      "6191": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 334
      },
      "6192": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 334
      },
      "6193": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 334
      },
      "6194": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 334
      },
      "6195": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 334
      },
      "6196": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 334
      },
      "6197": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 334
      },
      "6198": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 334
      },
      "6199": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 334
      },
      "6200": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 334
      },
      "6201": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 334
      },
      "6202": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 334
      },
      "6203": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 334
      },
      "6204": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 334
      },
      "6205": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 334
      },
      "6206": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 334
      },
      "6207": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 334
      },
      "6208": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 334
      },
      "6209": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 334
      },
      "6210": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 334
      },
      "6211": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 334
      },
      "6212": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 334
      },
      "6213": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 334
      },
      "6214": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 334
      },
      "6215": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 334
      },
      "6216": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 334
      },
      "6217": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 334
      },
      "6218": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 334
      },
      "6219": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 334
      },
      "6220": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 334
      },
      "6221": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 334
      },
      "6222": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 334
      },
      "6223": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 334
      },
      "6224": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 334
      },
      "6225": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 334
      },
      "6226": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 334
      },
      "6227": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 334
      },
      "6228": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 334
      },
      "6229": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 334
      },
      "6230": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 334
      },
      "6231": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 334
      },
      "6232": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 334
      },
      "6233": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 334
      },
      "6234": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 334
      },
      "6235": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 334
      },
      "6236": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 334
      },
      "6237": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 334
      },
      "6238": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 334
      },
      "6239": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 334
      },
      "6290": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 337
      },
      "6291": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 337
      },
      "6292": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 337
      },
      "6293": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 337
      },
      "6294": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 337
      },
      "6295": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 337
      },
      "6296": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 337
      },
      "6297": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 337
      },
      "6298": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 337
      },
      "6299": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 337
      },
      "6300": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 337
      },
      "6301": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 337
      },
      "6302": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 337
      },
      "6303": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 337
      },
      "6304": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 337
      },
      "6305": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 337
      },
      "6306": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 337
      },
      "6307": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 337
      },
      "6308": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 337
      },
      "6309": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 337
      },
      "6310": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 337
      },
      "6311": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 337
      },
      "6312": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 337
      },
      "6313": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 337
      },
      "6314": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 337
      },
      "6315": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 337
      },
      "6316": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 337
      },
      "6317": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 337
      },
      "6318": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 337
      },
      "6319": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 337
      },
      "6320": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 337
      },
      "6321": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 338
      },
      "6322": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 338
      },
      "6323": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 338
      },
      "6324": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 338
      },
      "6325": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 338
      },
      "6326": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 338
      },
      "6327": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 338
      },
      "6328": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 338
      },
      "6329": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 339
      },
      "6330": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 339
      },
      "6331": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 339
      },
      "6332": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 339
      },
      "6333": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 339
      },
      "6334": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 339
      },
      "6335": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 339
      },
      "6336": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 339
      },
      "6337": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 340
      },
      "6338": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 340
      },
      "6339": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 340
      },
      "6340": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 340
      },
      "6341": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 340
      },
      "6342": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 340
      },
      "6343": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 340
      },
      "6344": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 340
      },
      "6345": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 340
      },
      "6346": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 340
      },
      "6347": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 340
      },
      "6348": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 340
      },
      "6349": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 340
      },
      "6350": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 340
      },
      "6351": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 340
      },
      "6352": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 340
      },
      "6353": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 340
      },
      "6354": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 340
      },
      "6355": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 340
      },
      "6356": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 340
      },
      "6357": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 340
      },
      "6358": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 340
      },
      "6359": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 340
      },
      "6360": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 340
      },
      "6361": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 340
      },
      "6362": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 340
      },
      "6363": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 340
      },
      "6364": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 340
      },
      "6365": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 340
      },
      "6366": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 340
      },
      "6367": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 340
      },
      "6368": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 341
      },
      "6369": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 341
      },
      "6370": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 341
      },
      "6371": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 341
      },
      "6372": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 341
      },
      "6373": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 341
      },
      "6374": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 341
      },
      "6375": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 341
      },
      "6376": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 341
      },
      "6377": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 341
      },
      "6378": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 341
      },
      "6379": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 341
      },
      "6380": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 341
      },
      "6381": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 341
      },
      "6382": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 341
      },
      "6383": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 341
      },
      "6384": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 341
      },
      "6385": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 341
      },
      "6386": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 341
      },
      "6387": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 341
      },
      "6388": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 341
      },
      "6389": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 341
      },
      "6390": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 341
      },
      "6391": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 341
      },
      "6392": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 341
      },
      "6393": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 341
      },
      "6394": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 341
      },
      "6395": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 341
      },
      "6396": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 341
      },
      "6397": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 341
      },
      "6398": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 341
      },
      "6399": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 342
      },
      "6400": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 342
      },
      "6401": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 342
      },
      "6402": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 342
      },
      "6403": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 342
      },
      "6404": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 342
      },
      "6405": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 342
      },
      "6406": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 342
      },
      "6407": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 342
      },
      "6408": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 342
      },
      "6409": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 342
      },
      "6410": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 342
      },
      "6411": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 342
      },
      "6412": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 342
      },
      "6413": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 342
      },
      "6414": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 342
      },
      "6415": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 342
      },
      "6416": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 342
      },
      "6417": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 342
      },
      "6418": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 342
      },
      "6419": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 342
      },
      "6420": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 342
      },
      "6421": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 342
      },
      "6422": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 342
      },
      "6423": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 342
      },
      "6424": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 342
      },
      "6425": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 342
      },
      "6426": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 342
      },
      "6427": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 342
      },
      "6428": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 342
      },
      "6429": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 342
      },
      "6430": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 343
      },
      "6431": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 343
      },
      "6432": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 343
      },
      "6433": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 343
      },
      "6434": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 343
      },
      "6435": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 343
      },
      "6436": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 343
      },
      "6437": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 343
      },
      "6438": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 343
      },
      "6439": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 343
      },
      "6440": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 343
      },
      "6441": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 343
      },
      "6442": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 343
      },
      "6443": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 343
      },
      "6444": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 343
      },
      "6445": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 343
      },
      "6446": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 343
      },
      "6447": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 343
      },
      "6448": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 343
      },
      "6449": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 343
      },
      "6450": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 343
      },
      "6451": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 343
      },
      "6452": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 343
      },
      "6453": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 343
      },
      "6454": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 343
      },
      "6455": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 343
      },
      "6456": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 343
      },
      "6457": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 343
      },
      "6458": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 343
      },
      "6459": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 343
      },
      "6460": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 343
      },
      "6461": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 344
      },
      "6462": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 344
      },
      "6463": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 344
      },
      "6464": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 344
      },
      "6465": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 344
      },
      "6466": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 344
      },
      "6467": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 344
      },
      "6468": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 344
      },
      "6469": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 344
      },
      "6470": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 344
      },
      "6471": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 344
      },
      "6472": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 344
      },
      "6473": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 344
      },
      "6474": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 344
      },
      "6475": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 344
      },
      "6476": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 344
      },
      "6477": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 344
      },
      "6478": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 344
      },
      "6479": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 344
      },
      "6480": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 344
      },
      "6481": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 344
      },
      "6482": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 344
      },
      "6483": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 344
      },
      "6484": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 344
      },
      "6485": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 344
      },
      "6486": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 344
      },
      "6487": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 344
      },
      "6488": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 344
      },
      "6489": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 344
      },
      "6490": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 344
      },
      "6491": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 344
      },
      "6492": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 345
      },
      "6493": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 345
      },
      "6494": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 345
      },
      "6495": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 345
      },
      "6496": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 345
      },
      "6497": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 345
      },
      "6498": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 345
      },
      "6499": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 345
      },
      "6500": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 345
      },
      "6501": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 345
      },
      "6502": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 345
      },
      "6503": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 345
      },
      "6504": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 345
      },
      "6505": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 345
      },
      "6506": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 345
      },
      "6507": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 345
      },
      "6508": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 345
      },
      "6509": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 345
      },
      "6510": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 345
      },
      "6511": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 345
      },
      "6512": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 345
      },
      "6513": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 345
      },
      "6514": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 345
      },
      "6515": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 345
      },
      "6516": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 345
      },
      "6517": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 345
      },
      "6518": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 345
      },
      "6519": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 345
      },
      "6520": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 345
      },
      "6521": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 345
      },
      "6522": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 345
      },
      "6523": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 346
      },
      "6524": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 346
      },
      "6525": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 346
      },
      "6526": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 346
      },
      "6527": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 346
      },
      "6528": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 346
      },
      "6529": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 346
      },
      "6530": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 346
      },
      "6531": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 346
      },
      "6532": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 346
      },
      "6533": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 346
      },
      "6534": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 346
      },
      "6535": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 346
      },
      "6536": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 346
      },
      "6537": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 346
      },
      "6538": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 346
      },
      "6539": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 346
      },
      "6540": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 346
      },
      "6541": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 346
      },
      "6542": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 346
      },
      "6543": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 346
      },
      "6544": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 346
      },
      "6545": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 346
      },
      "6546": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 346
      },
      "6547": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 346
      },
      "6548": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 346
      },
      "6549": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 346
      },
      "6550": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 346
      },
      "6551": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 346
      },
      "6552": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 346
      },
      "6553": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 346
      },
      "6554": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 347
      },
      "6555": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 347
      },
      "6556": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 347
      },
      "6557": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 347
      },
      "6558": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 347
      },
      "6559": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 347
      },
      "6560": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 347
      },
      "6561": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 347
      },
      "6562": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 347
      },
      "6563": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 347
      },
      "6564": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 347
      },
      "6565": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 347
      },
      "6566": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 347
      },
      "6567": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 347
      },
      "6568": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 347
      },
      "6569": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 347
      },
      "6570": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 347
      },
      "6571": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 347
      },
      "6572": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 347
      },
      "6573": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 347
      },
      "6574": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 347
      },
      "6575": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 347
      },
      "6576": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 347
      },
      "6577": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 347
      },
      "6578": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 347
      },
      "6579": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 347
      },
      "6580": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 347
      },
      "6581": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 347
      },
      "6582": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 347
      },
      "6583": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 347
      },
      "6584": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 347
      },
      "6585": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 348
      },
      "6586": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 348
      },
      "6587": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 348
      },
      "6588": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 348
      },
      "6589": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 348
      },
      "6590": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 348
      },
      "6591": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 348
      },
      "6592": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 348
      },
      "6593": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 348
      },
      "6594": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 348
      },
      "6595": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 348
      },
      "6596": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 348
      },
      "6597": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 348
      },
      "6598": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 348
      },
      "6599": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 348
      },
      "6600": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 348
      },
      "6601": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 348
      },
      "6602": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 348
      },
      "6603": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 348
      },
      "6604": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 348
      },
      "6605": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 348
      },
      "6606": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 348
      },
      "6607": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 348
      },
      "6608": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 348
      },
      "6609": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 348
      },
      "6610": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 348
      },
      "6611": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 348
      },
      "6612": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 348
      },
      "6613": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 348
      },
      "6614": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 348
      },
      "6615": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 348
      },
      "6616": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 349
      },
      "6617": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 349
      },
      "6618": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 349
      },
      "6619": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 349
      },
      "6620": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 349
      },
      "6621": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 349
      },
      "6622": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 349
      },
      "6623": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 349
      },
      "6624": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 349
      },
      "6625": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 349
      },
      "6626": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 349
      },
      "6627": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 349
      },
      "6628": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 349
      },
      "6629": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 349
      },
      "6630": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 349
      },
      "6631": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 349
      },
      "6632": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 349
      },
      "6633": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 349
      },
      "6634": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 349
      },
      "6635": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 349
      },
      "6636": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 349
      },
      "6637": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 349
      },
      "6638": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 349
      },
      "6639": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 349
      },
      "6640": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 349
      },
      "6641": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 349
      },
      "6642": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 349
      },
      "6643": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 349
      },
      "6644": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 349
      },
      "6645": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 349
      },
      "6646": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 349
      },
      "6709": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 352
      },
      "6710": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 352
      },
      "6711": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 352
      },
      "6712": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 352
      },
      "6713": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 352
      },
      "6714": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 352
      },
      "6715": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 352
      },
      "6716": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 352
      },
      "6717": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 352
      },
      "6718": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 352
      },
      "6719": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 352
      },
      "6720": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 352
      },
      "6721": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 352
      },
      "6722": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 352
      },
      "6723": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 352
      },
      "6724": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 352
      },
      "6725": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 352
      },
      "6726": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 352
      },
      "6727": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 352
      },
      "6728": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 352
      },
      "6729": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 352
      },
      "6730": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 352
      },
      "6731": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 352
      },
      "6732": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 352
      },
      "6733": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 352
      },
      "6734": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 352
      },
      "6735": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 352
      },
      "6736": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 352
      },
      "6737": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 352
      },
      "6738": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 352
      },
      "6739": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 352
      },
      "7062": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 362
      },
      "7063": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 362
      },
      "7064": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 362
      },
      "7065": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 362
      },
      "7066": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 362
      },
      "7067": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 362
      },
      "7068": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 362
      },
      "7069": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 362
      },
      "7070": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 362
      },
      "7071": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 362
      },
      "7072": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 362
      },
      "7073": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 362
      },
      "7074": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 362
      },
      "7075": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 362
      },
      "7076": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 362
      },
      "7077": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 362
      },
      "7078": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 362
      },
      "7079": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 362
      },
      "7080": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 362
      },
      "7081": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 362
      },
      "7082": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 362
      },
      "7083": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 362
      },
      "7084": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 362
      },
      "7085": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 362
      },
      "7086": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 362
      },
      "7087": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 362
      },
      "7088": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 362
      },
      "7089": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 362
      },
      "7090": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 362
      },
      "7091": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 362
      },
      "7092": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 362
      },
      "7093": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 362
      },
      "7094": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 362
      },
      "7095": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 362
      },
      "7096": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 362
      },
      "7097": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 362
      },
      "7098": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 362
      },
      "7099": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 362
      },
      "7100": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 362
      },
      "7101": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 362
      },
      "7102": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 363
      },
      "7103": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 363
      },
      "7104": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 363
      },
      "7105": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 363
      },
      "7106": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 363
      },
      "7107": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 363
      },
      "7108": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 363
      },
      "7109": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 363
      },
      "7110": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 363
      },
      "7111": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 363
      },
      "7112": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 363
      },
      "7113": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 363
      },
      "7114": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 363
      },
      "7115": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 363
      },
      "7116": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 363
      },
      "7117": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 363
      },
      "7118": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 363
      },
      "7119": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 363
      },
      "7120": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 363
      },
      "7121": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 363
      },
      "7122": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 363
      },
      "7123": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 363
      },
      "7124": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 363
      },
      "7125": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 363
      },
      "7126": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 363
      },
      "7127": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 363
      },
      "7128": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 363
      },
      "7129": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 363
      },
      "7130": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 363
      },
      "7131": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 363
      },
      "7132": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 363
      },
      "7133": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 363
      },
      "7134": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 363
      },
      "7135": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 363
      },
      "7136": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 363
      },
      "7137": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 363
      },
      "7138": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 363
      },
      "7139": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 363
      },
      "7140": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 363
      },
      "7141": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 363
      },
      "7142": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 363
      },
      "7143": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 363
      },
      "7144": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 363
      },
      "7145": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 363
      },
      "7146": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 363
      },
      "7147": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 363
      },
      "7148": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 363
      },
      "7149": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 363
      },
      "7150": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 363
      },
      "7151": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 363
      },
      "7152": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 363
      },
      "7153": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 364
      },
      "7154": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 364
      },
      "7155": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 364
      },
      "7156": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 364
      },
      "7157": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 364
      },
      "7158": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 364
      },
      "7159": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 364
      },
      "7160": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 364
      },
      "7161": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 364
      },
      "7162": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 364
      },
      "7163": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 364
      },
      "7164": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 364
      },
      "7165": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 364
      },
      "7166": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 364
      },
      "7167": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 364
      },
      "7168": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 364
      },
      "7169": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 364
      },
      "7170": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 364
      },
      "7171": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 364
      },
      "7172": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 364
      },
      "7173": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 364
      },
      "7174": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 364
      },
      "7175": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 364
      },
      "7176": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 364
      },
      "7177": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 364
      },
      "7178": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 364
      },
      "7179": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 364
      },
      "7180": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 364
      },
      "7181": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 364
      },
      "7182": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 364
      },
      "7183": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 364
      },
      "7184": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 364
      },
      "7185": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 364
      },
      "7186": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 364
      },
      "7187": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 364
      },
      "7188": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 364
      },
      "7189": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 364
      },
      "7190": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 364
      },
      "7191": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 364
      },
      "7192": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 364
      },
      "7193": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 364
      },
      "7194": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 364
      },
      "7195": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 364
      },
      "7196": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 364
      },
      "7197": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 364
      },
      "7198": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 364
      },
      "7199": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 364
      },
      "7200": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 364
      },
      "7201": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 364
      },
      "7202": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 364
      },
      "7203": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 364
      },
      "7805": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 376
      },
      "7806": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 376
      },
      "7807": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 376
      },
      "7808": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 376
      },
      "7809": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 376
      },
      "7810": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 376
      },
      "7811": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 376
      },
      "7812": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 376
      },
      "7813": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 376
      },
      "7814": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 376
      },
      "7815": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 376
      },
      "7816": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 376
      },
      "7817": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 376
      },
      "7818": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 376
      },
      "7819": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 376
      },
      "7820": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 376
      },
      "7821": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 376
      },
      "7822": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 376
      },
      "7823": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 376
      },
      "7824": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 376
      },
      "7825": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 376
      },
      "7826": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 376
      },
      "7827": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 376
      },
      "7828": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 376
      },
      "7829": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 376
      },
      "7830": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 376
      },
      "7831": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 376
      },
      "7832": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 376
      },
      "7833": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 376
      },
      "7834": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 376
      },
      "7835": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 376
      },
      "7836": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 376
      },
      "7837": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 376
      },
      "7838": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 376
      },
      "7839": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 376
      },
      "7840": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 376
      },
      "7841": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 376
      },
      "7842": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 376
      },
      "7843": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 376
      },
      "7844": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 376
      },
      "7845": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 376
      },
      "7846": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 376
      },
      "7847": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 376
      },
      "7848": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 376
      },
      "7849": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 376
      },
      "7850": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 376
      },
      "7851": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 376
      },
      "7852": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 376
      },
      "7853": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 376
      },
      "7854": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 376
      },
      "7855": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 376
      },
      "7856": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 376
      },
      "7857": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 376
      },
      "7858": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 376
      },
      "7859": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 376
      },
      "7860": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 377
      },
      "7861": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 377
      },
      "7862": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 377
      },
      "7863": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 377
      },
      "7864": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 377
      },
      "7865": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 377
      },
      "7866": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 377
      },
      "7867": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 377
      },
      "7868": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 377
      },
      "7869": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 377
      },
      "7870": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 377
      },
      "7871": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 377
      },
      "7872": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 377
      },
      "7873": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 377
      },
      "7874": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 377
      },
      "7875": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 377
      },
      "7876": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 377
      },
      "7877": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 377
      },
      "7878": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 377
      },
      "7879": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 377
      },
      "7880": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 377
      },
      "7881": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 377
      },
      "7882": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 377
      },
      "7883": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 377
      },
      "7884": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 377
      },
      "7885": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 377
      },
      "7886": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 377
      },
      "7887": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 377
      },
      "7888": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 377
      },
      "7889": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 377
      },
      "7890": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 377
      },
      "7891": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 377
      },
      "7892": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 377
      },
      "7893": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 377
      },
      "7894": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 377
      },
      "7895": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 377
      },
      "7896": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 377
      },
      "7897": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 377
      },
      "7898": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 377
      },
      "7899": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 377
      },
      "7900": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 377
      },
      "7901": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 377
      },
      "7902": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 377
      },
      "7903": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 377
      },
      "7904": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 377
      },
      "7905": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 377
      },
      "7906": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 377
      },
      "7907": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 377
      },
      "7908": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 377
      },
      "7909": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 377
      },
      "7910": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 377
      },
      "7911": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 377
      },
      "7912": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 377
      },
      "7913": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 377
      },
      "7914": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 377
      },
      "7915": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 378
      },
      "7916": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 378
      },
      "7917": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 378
      },
      "7918": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 378
      },
      "7919": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 378
      },
      "7920": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 378
      },
      "7921": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 378
      },
      "7922": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 378
      },
      "7923": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 378
      },
      "7924": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 378
      },
      "7925": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 378
      },
      "7926": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 378
      },
      "7927": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 378
      },
      "7928": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 378
      },
      "7929": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 378
      },
      "7930": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 378
      },
      "7931": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 378
      },
      "7932": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 378
      },
      "7933": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 378
      },
      "7934": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 378
      },
      "7935": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 378
      },
      "7936": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 378
      },
      "7937": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 378
      },
      "7938": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 378
      },
      "7939": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 378
      },
      "7940": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 378
      },
      "7941": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 378
      },
      "7942": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 378
      },
      "7943": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 378
      },
      "7944": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 378
      },
      "7945": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 378
      },
      "7946": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 378
      },
      "7947": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 378
      },
      "7948": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 378
      },
      "7949": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 378
      },
      "7950": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 378
      },
      "7951": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 378
      },
      "7952": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 378
      },
      "7953": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 378
      },
      "7954": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 378
      },
      "7955": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 378
      },
      "7956": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 378
      },
      "7957": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 378
      },
      "7958": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 378
      },
      "7959": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 378
      },
      "7960": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 378
      },
      "7961": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 378
      },
      "7962": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 378
      },
      "7963": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 378
      },
      "7964": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 378
      },
      "7965": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 378
      },
      "7966": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 378
      },
      "7967": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 378
      },
      "7968": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 378
      },
      "7969": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 378
      },
      "7970": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 379
      },
      "7971": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 379
      },
      "7972": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 379
      },
      "7973": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 379
      },
      "7974": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 379
      },
      "7975": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 379
      },
      "7976": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 379
      },
      "7977": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 379
      },
      "7978": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 379
      },
      "7979": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 379
      },
      "7980": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 379
      },
      "7981": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 379
      },
      "7982": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 379
      },
      "7983": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 379
      },
      "7984": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 379
      },
      "7985": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 379
      },
      "7986": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 379
      },
      "7987": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 379
      },
      "7988": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 379
      },
      "7989": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 379
      },
      "7990": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 379
      },
      "7991": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 379
      },
      "7992": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 379
      },
      "7993": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 379
      },
      "7994": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 379
      },
      "7995": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 379
      },
      "7996": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 379
      },
      "7997": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 379
      },
      "7998": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 379
      },
      "7999": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 379
      },
      "8000": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 379
      },
      "8001": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 379
      },
      "8002": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 379
      },
      "8003": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 379
      },
      "8004": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 379
      },
      "8005": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 379
      },
      "8006": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 379
      },
      "8007": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 379
      },
      "8008": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 379
      },
      "8009": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 379
      },
      "8010": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 379
      },
      "8011": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 379
      },
      "8012": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 379
      },
      "8013": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 379
      },
      "8014": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 379
      },
      "8015": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 379
      },
      "8016": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 379
      },
      "8017": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 379
      },
      "8018": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 379
      },
      "8019": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 379
      },
      "8020": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 379
      },
      "8021": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 379
      },
      "8022": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 379
      },
      "8023": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 379
      },
      "8024": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 379
      },
      "8025": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 380
      },
      "8026": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 380
      },
      "8027": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 380
      },
      "8028": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 380
      },
      "8029": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 380
      },
      "8030": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 380
      },
      "8031": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 380
      },
      "8032": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 380
      },
      "8033": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 380
      },
      "8034": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 380
      },
      "8035": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 380
      },
      "8036": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 380
      },
      "8037": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 380
      },
      "8038": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 380
      },
      "8039": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 380
      },
      "8040": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 380
      },
      "8041": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 380
      },
      "8042": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 380
      },
      "8043": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 380
      },
      "8044": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 380
      },
      "8045": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 380
      },
      "8046": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 380
      },
      "8047": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 380
      },
      "8048": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 380
      },
      "8049": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 380
      },
      "8050": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 380
      },
      "8051": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 380
      },
      "8052": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 380
      },
      "8053": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 380
      },
      "8054": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 380
      },
      "8055": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 380
      },
      "8056": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 380
      },
      "8057": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 380
      },
      "8058": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 380
      },
      "8059": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 380
      },
      "8060": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 380
      },
      "8061": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 380
      },
      "8062": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 380
      },
      "8063": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 380
      },
      "8064": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 380
      },
      "8065": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 380
      },
      "8066": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 380
      },
      "8067": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 380
      },
      "8068": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 380
      },
      "8069": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 380
      },
      "8070": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 380
      },
      "8071": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 380
      },
      "8072": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 380
      },
      "8073": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 380
      },
      "8074": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 380
      },
      "8075": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 380
      },
      "8076": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 380
      },
      "8077": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 380
      },
      "8078": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 380
      },
      "8079": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 380
      },
      "8080": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 381
      },
      "8081": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 381
      },
      "8082": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 381
      },
      "8083": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 381
      },
      "8084": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 381
      },
      "8085": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 381
      },
      "8086": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 381
      },
      "8087": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 381
      },
      "8088": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 381
      },
      "8089": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 381
      },
      "8090": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 381
      },
      "8091": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 381
      },
      "8092": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 381
      },
      "8093": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 381
      },
      "8094": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 381
      },
      "8095": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 381
      },
      "8096": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 381
      },
      "8097": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 381
      },
      "8098": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 381
      },
      "8099": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 381
      },
      "8100": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 381
      },
      "8101": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 381
      },
      "8102": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 381
      },
      "8103": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 381
      },
      "8104": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 381
      },
      "8105": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 381
      },
      "8106": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 381
      },
      "8107": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 381
      },
      "8108": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 381
      },
      "8109": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 381
      },
      "8110": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 381
      },
      "8111": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 381
      },
      "8112": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 381
      },
      "8113": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 381
      },
      "8114": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 381
      },
      "8115": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 381
      },
      "8116": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 381
      },
      "8117": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 381
      },
      "8118": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 381
      },
      "8119": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 381
      },
      "8120": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 381
      },
      "8121": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 381
      },
      "8122": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 381
      },
      "8123": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 381
      },
      "8124": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 381
      },
      "8125": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 381
      },
      "8126": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 381
      },
      "8127": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 381
      },
      "8128": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 381
      },
      "8129": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 381
      },
      "8130": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 381
      },
      "8131": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 381
      },
      "8132": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 381
      },
      "8133": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 381
      },
      "8134": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 381
      },
      "8135": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 382
      },
      "8136": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 382
      },
      "8137": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 382
      },
      "8138": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 382
      },
      "8139": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 382
      },
      "8140": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 382
      },
      "8141": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 382
      },
      "8142": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 382
      },
      "8143": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 382
      },
      "8144": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 382
      },
      "8145": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 382
      },
      "8146": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 382
      },
      "8147": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 382
      },
      "8148": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 382
      },
      "8149": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 382
      },
      "8150": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 382
      },
      "8151": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 382
      },
      "8152": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 382
      },
      "8153": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 382
      },
      "8154": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 382
      },
      "8155": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 382
      },
      "8156": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 382
      },
      "8157": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 382
      },
      "8158": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 382
      },
      "8159": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 382
      },
      "8160": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 382
      },
      "8161": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 382
      },
      "8162": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 382
      },
      "8163": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 382
      },
      "8164": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 382
      },
      "8165": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 382
      },
      "8166": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 382
      },
      "8167": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 382
      },
      "8168": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 382
      },
      "8169": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 382
      },
      "8170": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 382
      },
      "8171": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 382
      },
      "8172": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 382
      },
      "8173": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 382
      },
      "8174": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 382
      },
      "8175": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 382
      },
      "8176": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 382
      },
      "8177": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 382
      },
      "8178": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 382
      },
      "8179": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 382
      },
      "8180": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 382
      },
      "8181": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 382
      },
      "8182": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 382
      },
      "8183": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 382
      },
      "8184": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 382
      },
      "8185": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 382
      },
      "8186": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 382
      },
      "8187": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 382
      },
      "8188": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 382
      },
      "8189": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 382
      },
      "8190": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 383
      },
      "8191": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 383
      },
      "8192": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 383
      },
      "8193": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 383
      },
      "8194": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 383
      },
      "8195": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 383
      },
      "8196": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 383
      },
      "8197": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 383
      },
      "8198": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 383
      },
      "8199": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 383
      },
      "8200": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 383
      },
      "8201": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 383
      },
      "8202": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 383
      },
      "8203": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 383
      },
      "8204": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 383
      },
      "8205": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 383
      },
      "8206": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 383
      },
      "8207": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 383
      },
      "8208": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 383
      },
      "8209": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 383
      },
      "8210": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 383
      },
      "8211": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 383
      },
      "8212": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 383
      },
      "8213": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 383
      },
      "8214": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 383
      },
      "8215": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 383
      },
      "8216": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 383
      },
      "8217": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 383
      },
      "8218": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 383
      },
      "8219": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 383
      },
      "8220": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 383
      },
      "8221": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 383
      },
      "8222": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 383
      },
      "8223": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 383
      },
      "8224": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 383
      },
      "8225": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 383
      },
      "8226": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 383
      },
      "8227": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 383
      },
      "8228": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 383
      },
      "8229": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 383
      },
      "8230": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 383
      },
      "8231": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 383
      },
      "8232": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 383
      },
      "8233": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 383
      },
      "8234": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 383
      },
      "8235": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 383
      },
      "8236": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 383
      },
      "8237": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 383
      },
      "8238": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 383
      },
      "8239": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 383
      },
      "8240": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 383
      },
      "8241": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 383
      },
      "8242": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 383
      },
      "8243": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 383
      },
      "8244": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 383
      },
      "8245": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 384
      },
      "8246": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 384
      },
      "8247": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 384
      },
      "8248": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 384
      },
      "8249": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 384
      },
      "8250": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 384
      },
      "8251": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 384
      },
      "8252": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 384
      },
      "8253": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 384
      },
      "8254": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 384
      },
      "8255": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 384
      },
      "8256": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 384
      },
      "8257": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 384
      },
      "8258": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 384
      },
      "8259": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 384
      },
      "8260": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 384
      },
      "8261": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 384
      },
      "8262": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 384
      },
      "8263": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 384
      },
      "8264": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 384
      },
      "8265": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 384
      },
      "8266": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 384
      },
      "8267": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 384
      },
      "8268": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 384
      },
      "8269": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 384
      },
      "8270": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 384
      },
      "8271": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 384
      },
      "8272": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 384
      },
      "8273": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 384
      },
      "8274": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 384
      },
      "8275": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 384
      },
      "8276": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 384
      },
      "8277": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 384
      },
      "8278": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 384
      },
      "8279": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 384
      },
      "8280": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 384
      },
      "8281": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 384
      },
      "8282": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 384
      },
      "8283": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 384
      },
      "8284": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 384
      },
      "8285": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 384
      },
      "8286": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 384
      },
      "8287": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 384
      },
      "8288": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 384
      },
      "8289": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 384
      },
      "8290": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 384
      },
      "8291": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 384
      },
      "8292": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 384
      },
      "8293": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 384
      },
      "8294": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 384
      },
      "8295": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 384
      },
      "8296": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 384
      },
      "8297": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 384
      },
      "8298": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 384
      },
      "8299": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 384
      },
      "8300": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 385
      },
      "8301": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 385
      },
      "8302": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 385
      },
      "8303": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 385
      },
      "8304": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 385
      },
      "8305": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 385
      },
      "8306": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 385
      },
      "8307": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 385
      },
      "8308": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 385
      },
      "8309": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 385
      },
      "8310": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 385
      },
      "8311": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 385
      },
      "8312": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 385
      },
      "8313": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 385
      },
      "8314": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 385
      },
      "8315": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 385
      },
      "8316": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 385
      },
      "8317": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 385
      },
      "8318": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 385
      },
      "8319": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 385
      },
      "8320": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 385
      },
      "8321": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 385
      },
      "8322": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 385
      },
      "8323": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 385
      },
      "8324": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 385
      },
      "8325": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 385
      },
      "8326": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 385
      },
      "8327": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 385
      },
      "8328": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 385
      },
      "8329": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 385
      },
      "8330": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 385
      },
      "8331": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 385
      },
      "8332": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 385
      },
      "8333": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 385
      },
      "8334": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 385
      },
      "8335": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 385
      },
      "8336": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 385
      },
      "8337": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 385
      },
      "8338": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 385
      },
      "8339": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 385
      },
      "8340": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 385
      },
      "8341": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 385
      },
      "8342": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 385
      },
      "8343": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 385
      },
      "8344": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 385
      },
      "8345": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 385
      },
      "8346": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 385
      },
      "8347": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 385
      },
      "8348": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 385
      },
      "8349": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 385
      },
      "8350": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 385
      },
      "8351": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 385
      },
      "8352": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 385
      },
      "8353": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 385
      },
      "8354": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 385
      },
      "8355": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 386
      },
      "8356": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 386
      },
      "8357": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 386
      },
      "8358": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 386
      },
      "8359": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 386
      },
      "8360": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 386
      },
      "8361": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 386
      },
      "8362": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 386
      },
      "8363": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 386
      },
      "8364": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 386
      },
      "8365": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 386
      },
      "8366": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 386
      },
      "8367": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 386
      },
      "8368": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 386
      },
      "8369": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 386
      },
      "8370": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 386
      },
      "8371": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 386
      },
      "8372": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 386
      },
      "8373": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 386
      },
      "8374": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 386
      },
      "8375": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 386
      },
      "8376": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 386
      },
      "8377": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 386
      },
      "8378": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 386
      },
      "8379": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 386
      },
      "8380": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 386
      },
      "8381": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 386
      },
      "8382": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 386
      },
      "8383": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 386
      },
      "8384": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 386
      },
      "8385": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 386
      },
      "8386": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 386
      },
      "8387": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 386
      },
      "8388": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 386
      },
      "8389": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 386
      },
      "8390": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 386
      },
      "8391": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 386
      },
      "8392": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 386
      },
      "8393": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 386
      },
      "8394": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 386
      },
      "8395": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 386
      },
      "8396": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 386
      },
      "8397": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 386
      },
      "8398": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 386
      },
      "8399": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 386
      },
      "8400": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 386
      },
      "8401": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 386
      },
      "8402": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 386
      },
      "8403": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 386
      },
      "8404": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 386
      },
      "8405": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 386
      },
      "8406": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 386
      },
      "8407": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 386
      },
      "8408": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 386
      },
      "8409": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 386
      },
      "8410": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 387
      },
      "8411": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 387
      },
      "8412": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 387
      },
      "8413": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 387
      },
      "8414": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 387
      },
      "8415": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 387
      },
      "8416": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 387
      },
      "8417": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 387
      },
      "8418": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 387
      },
      "8419": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 387
      },
      "8420": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 387
      },
      "8421": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 387
      },
      "8422": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 387
      },
      "8423": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 387
      },
      "8424": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 387
      },
      "8425": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 387
      },
      "8426": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 387
      },
      "8427": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 387
      },
      "8428": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 387
      },
      "8429": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 387
      },
      "8430": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 387
      },
      "8431": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 387
      },
      "8432": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 387
      },
      "8433": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 387
      },
      "8434": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 387
      },
      "8435": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 387
      },
      "8436": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 387
      },
      "8437": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 387
      },
      "8438": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 387
      },
      "8439": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 387
      },
      "8440": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 387
      },
      "8441": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 387
      },
      "8442": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 387
      },
      "8443": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 387
      },
      "8444": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 387
      },
      "8445": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 387
      },
      "8446": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 387
      },
      "8447": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 387
      },
      "8448": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 387
      },
      "8449": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 387
      },
      "8450": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 387
      },
      "8451": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 387
      },
      "8452": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 387
      },
      "8453": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 387
      },
      "8454": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 387
      },
      "8455": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 387
      },
      "8456": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 387
      },
      "8457": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 387
      },
      "8458": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 387
      },
      "8459": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 387
      },
      "8460": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 387
      },
      "8461": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 387
      },
      "8462": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 387
      },
      "8463": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 387
      },
      "8464": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 387
      },
      "8465": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 388
      },
      "8466": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 388
      },
      "8467": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 388
      },
      "8468": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 388
      },
      "8469": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 388
      },
      "8470": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 388
      },
      "8471": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 388
      },
      "8472": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 388
      },
      "8473": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 388
      },
      "8474": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 388
      },
      "8475": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 388
      },
      "8476": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 388
      },
      "8477": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 388
      },
      "8478": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 388
      },
      "8479": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 388
      },
      "8480": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 388
      },
      "8481": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 388
      },
      "8482": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 388
      },
      "8483": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 388
      },
      "8484": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 388
      },
      "8485": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 388
      },
      "8486": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 388
      },
      "8487": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 388
      },
      "8488": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 388
      },
      "8489": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 388
      },
      "8490": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 388
      },
      "8491": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 388
      },
      "8492": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 388
      },
      "8493": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 388
      },
      "8494": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 388
      },
      "8495": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 388
      },
      "8496": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 388
      },
      "8497": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 388
      },
      "8498": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 388
      },
      "8499": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 388
      },
      "8500": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 388
      },
      "8501": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 388
      },
      "8502": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 388
      },
      "8503": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 388
      },
      "8504": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 388
      },
      "8505": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 388
      },
      "8506": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 388
      },
      "8507": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 388
      },
      "8508": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 388
      },
      "8509": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 388
      },
      "8510": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 388
      },
      "8511": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 388
      },
      "8512": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 388
      },
      "8513": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 388
      },
      "8514": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 388
      },
      "8515": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 388
      },
      "8516": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 388
      },
      "8517": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 388
      },
      "8518": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 388
      },
      "8519": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 388
      },
      "8520": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 389
      },
      "8521": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 389
      },
      "8522": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 389
      },
      "8523": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 389
      },
      "8524": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 389
      },
      "8525": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 389
      },
      "8526": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 389
      },
      "8527": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 389
      },
      "8528": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 389
      },
      "8529": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 389
      },
      "8530": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 389
      },
      "8531": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 389
      },
      "8532": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 389
      },
      "8533": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 389
      },
      "8534": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 389
      },
      "8535": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 389
      },
      "8536": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 389
      },
      "8537": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 389
      },
      "8538": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 389
      },
      "8539": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 389
      },
      "8540": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 389
      },
      "8541": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 389
      },
      "8542": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 389
      },
      "8543": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 389
      },
      "8544": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 389
      },
      "8545": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 389
      },
      "8546": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 389
      },
      "8547": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 389
      },
      "8548": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 389
      },
      "8549": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 389
      },
      "8550": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 389
      },
      "8551": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 389
      },
      "8552": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 389
      },
      "8553": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 389
      },
      "8554": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 389
      },
      "8555": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 389
      },
      "8556": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 389
      },
      "8557": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 389
      },
      "8558": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 389
      },
      "8559": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 389
      },
      "8560": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 389
      },
      "8561": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 389
      },
      "8562": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 389
      },
      "8563": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 389
      },
      "8564": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 389
      },
      "8565": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 389
      },
      "8566": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 389
      },
      "8567": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 389
      },
      "8568": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 389
      },
      "8569": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 389
      },
      "8570": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 389
      },
      "8571": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 389
      },
      "8572": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 389
      },
      "8573": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 389
      },
      "8574": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 389
      },
      "8575": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 390
      },
      "8576": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 390
      },
      "8577": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 390
      },
      "8578": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 390
      },
      "8579": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 390
      },
      "8580": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 390
      },
      "8581": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 390
      },
      "8582": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 390
      },
      "8583": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 390
      },
      "8584": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 390
      },
      "8585": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 390
      },
      "8586": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 390
      },
      "8587": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 390
      },
      "8588": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 390
      },
      "8589": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 390
      },
      "8590": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 390
      },
      "8591": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 390
      },
      "8592": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 390
      },
      "8593": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 390
      },
      "8594": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 390
      },
      "8595": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 390
      },
      "8596": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 390
      },
      "8597": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 390
      },
      "8598": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 390
      },
      "8599": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 390
      },
      "8600": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 390
      },
      "8601": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 390
      },
      "8602": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 390
      },
      "8603": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 390
      },
      "8604": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 390
      },
      "8605": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 390
      },
      "8606": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 390
      },
      "8607": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 390
      },
      "8608": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 390
      },
      "8609": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 390
      },
      "8610": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 390
      },
      "8611": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 390
      },
      "8612": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 390
      },
      "8613": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 390
      },
      "8614": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 390
      },
      "8615": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 390
      },
      "8616": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 390
      },
      "8617": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 390
      },
      "8618": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 390
      },
      "8619": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 390
      },
      "8620": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 390
      },
      "8621": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 390
      },
      "8622": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 390
      },
      "8623": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 390
      },
      "8624": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 390
      },
      "8625": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 390
      },
      "8626": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 390
      },
      "8627": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 390
      },
      "8628": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 390
      },
      "8629": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 390
      },
      "8630": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 391
      },
      "8631": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 391
      },
      "8632": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 391
      },
      "8633": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 391
      },
      "8634": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 391
      },
      "8635": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 391
      },
      "8636": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 391
      },
      "8637": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 391
      },
      "8638": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 391
      },
      "8639": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 391
      },
      "8640": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 391
      },
      "8641": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 391
      },
      "8642": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 391
      },
      "8643": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 391
      },
      "8644": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 391
      },
      "8645": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 391
      },
      "8646": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 391
      },
      "8647": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 391
      },
      "8648": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 391
      },
      "8649": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 391
      },
      "8650": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 391
      },
      "8651": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 391
      },
      "8652": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 391
      },
      "8653": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 391
      },
      "8654": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 391
      },
      "8655": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 391
      },
      "8656": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 391
      },
      "8657": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 391
      },
      "8658": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 391
      },
      "8659": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 391
      },
      "8660": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 391
      },
      "8661": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 391
      },
      "8662": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 391
      },
      "8663": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 391
      },
      "8664": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 391
      },
      "8665": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 391
      },
      "8666": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 391
      },
      "8667": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 391
      },
      "8668": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 391
      },
      "8669": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 391
      },
      "8670": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 391
      },
      "8671": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 391
      },
      "8672": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 391
      },
      "8673": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 391
      },
      "8674": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 391
      },
      "8675": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 391
      },
      "8676": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 391
      },
      "8677": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 391
      },
      "8678": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 391
      },
      "8679": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 391
      },
      "8680": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 391
      },
      "8681": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 391
      },
      "8682": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 391
      },
      "8683": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 391
      },
      "8684": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 391
      },
      "8685": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 392
      },
      "8686": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 392
      },
      "8687": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 392
      },
      "8688": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 392
      },
      "8689": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 392
      },
      "8690": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 392
      },
      "8691": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 392
      },
      "8692": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 392
      },
      "8693": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 392
      },
      "8694": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 392
      },
      "8695": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 392
      },
      "8696": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 392
      },
      "8697": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 392
      },
      "8698": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 392
      },
      "8699": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 392
      },
      "8700": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 392
      },
      "8701": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 392
      },
      "8702": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 392
      },
      "8703": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 392
      },
      "8704": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 392
      },
      "8705": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 392
      },
      "8706": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 392
      },
      "8707": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 392
      },
      "8708": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 392
      },
      "8709": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 392
      },
      "8710": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 392
      },
      "8711": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 392
      },
      "8712": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 392
      },
      "8713": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 392
      },
      "8714": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 392
      },
      "8715": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 392
      },
      "8716": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 392
      },
      "8717": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 392
      },
      "8718": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 392
      },
      "8719": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 392
      },
      "8720": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 392
      },
      "8721": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 392
      },
      "8722": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 392
      },
      "8723": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 392
      },
      "8724": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 392
      },
      "8725": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 392
      },
      "8726": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 392
      },
      "8727": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 392
      },
      "8728": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 392
      },
      "8729": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 392
      },
      "8730": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 392
      },
      "8731": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 392
      },
      "8732": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 392
      },
      "8733": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 392
      },
      "8734": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 392
      },
      "8735": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 392
      },
      "8736": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 392
      },
      "8737": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 392
      },
      "8738": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 392
      },
      "8739": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 392
      },
      "8740": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 393
      },
      "8741": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 393
      },
      "8742": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 393
      },
      "8743": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 393
      },
      "8744": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 393
      },
      "8745": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 393
      },
      "8746": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 393
      },
      "8747": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 393
      },
      "8748": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 393
      },
      "8749": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 393
      },
      "8750": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 393
      },
      "8751": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 393
      },
      "8752": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 393
      },
      "8753": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 393
      },
      "8754": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 393
      },
      "8755": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 393
      },
      "8756": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 393
      },
      "8757": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 393
      },
      "8758": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 393
      },
      "8759": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 393
      },
      "8760": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 393
      },
      "8761": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 393
      },
      "8762": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 393
      },
      "8763": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 393
      },
      "8764": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 393
      },
      "8765": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 393
      },
      "8766": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 393
      },
      "8767": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 393
      },
      "8768": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 393
      },
      "8769": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 393
      },
      "8770": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 393
      },
      "8771": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 393
      },
      "8772": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 393
      },
      "8773": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 393
      },
      "8774": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 393
      },
      "8775": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 393
      },
      "8776": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 393
      },
      "8777": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 393
      },
      "8778": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 393
      },
      "8779": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 393
      },
      "8780": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 393
      },
      "8781": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 393
      },
      "8782": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 393
      },
      "8783": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 393
      },
      "8784": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 393
      },
      "8785": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 393
      },
      "8786": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 393
      },
      "8787": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 393
      },
      "8788": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 393
      },
      "8789": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 393
      },
      "8790": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 393
      },
      "8791": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 393
      },
      "8792": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 393
      },
      "8793": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 393
      },
      "8794": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 393
      },
      "8795": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 394
      },
      "8796": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 394
      },
      "8797": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 394
      },
      "8798": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 394
      },
      "8799": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 394
      },
      "8800": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 394
      },
      "8801": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 394
      },
      "8802": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 394
      },
      "8803": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 394
      },
      "8804": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 394
      },
      "8805": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 394
      },
      "8806": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 394
      },
      "8807": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 394
      },
      "8808": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 394
      },
      "8809": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 394
      },
      "8810": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 394
      },
      "8811": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 394
      },
      "8812": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 394
      },
      "8813": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 394
      },
      "8814": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 394
      },
      "8815": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 394
      },
      "8816": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 394
      },
      "8817": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 394
      },
      "8818": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 394
      },
      "8819": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 394
      },
      "8820": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 394
      },
      "8821": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 394
      },
      "8822": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 394
      },
      "8823": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 394
      },
      "8824": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 394
      },
      "8825": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 394
      },
      "8826": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 394
      },
      "8827": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 394
      },
      "8828": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 394
      },
      "8829": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 394
      },
      "8830": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 394
      },
      "8831": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 394
      },
      "8832": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 394
      },
      "8833": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 394
      },
      "8834": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 394
      },
      "8835": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 394
      },
      "8836": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 394
      },
      "8837": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 394
      },
      "8838": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 394
      },
      "8839": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 394
      },
      "8840": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 394
      },
      "8841": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 394
      },
      "8842": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 394
      },
      "8843": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 394
      },
      "8844": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 394
      },
      "8845": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 394
      },
      "8846": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 394
      },
      "8847": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 394
      },
      "8848": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 394
      },
      "8849": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 394
      },
      "8850": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 395
      },
      "8851": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 395
      },
      "8852": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 395
      },
      "8853": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 395
      },
      "8854": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 395
      },
      "8855": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 395
      },
      "8856": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 395
      },
      "8857": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 395
      },
      "8858": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 395
      },
      "8859": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 395
      },
      "8860": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 395
      },
      "8861": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 395
      },
      "8862": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 395
      },
      "8863": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 395
      },
      "8864": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 395
      },
      "8865": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 395
      },
      "8866": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 395
      },
      "8867": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 395
      },
      "8868": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 395
      },
      "8869": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 395
      },
      "8870": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 395
      },
      "8871": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 395
      },
      "8872": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 395
      },
      "8873": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 395
      },
      "8874": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 395
      },
      "8875": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 395
      },
      "8876": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 395
      },
      "8877": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 395
      },
      "8878": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 395
      },
      "8879": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 395
      },
      "8880": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 395
      },
      "8881": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 395
      },
      "8882": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 395
      },
      "8883": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 395
      },
      "8884": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 395
      },
      "8885": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 395
      },
      "8886": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 395
      },
      "8887": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 395
      },
      "8888": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 395
      },
      "8889": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 395
      },
      "8890": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 395
      },
      "8891": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 395
      },
      "8892": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 395
      },
      "8893": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 395
      },
      "8894": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 395
      },
      "8895": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 395
      },
      "8896": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 395
      },
      "8897": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 395
      },
      "8898": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 395
      },
      "8899": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 395
      },
      "8900": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 395
      },
      "8901": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 395
      },
      "8902": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 395
      },
      "8903": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 395
      },
      "8904": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 395
      },
      "8905": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 396
      },
      "8906": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 396
      },
      "8907": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 396
      },
      "8908": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 396
      },
      "8909": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 396
      },
      "8910": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 396
      },
      "8911": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 396
      },
      "8912": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 396
      },
      "8913": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 396
      },
      "8914": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 396
      },
      "8915": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 396
      },
      "8916": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 396
      },
      "8917": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 396
      },
      "8918": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 396
      },
      "8919": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 396
      },
      "8920": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 396
      },
      "8921": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 396
      },
      "8922": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 396
      },
      "8923": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 396
      },
      "8924": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 396
      },
      "8925": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 396
      },
      "8926": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 396
      },
      "8927": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 396
      },
      "8928": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 396
      },
      "8929": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 396
      },
      "8930": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 396
      },
      "8931": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 396
      },
      "8932": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 396
      },
      "8933": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 396
      },
      "8934": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 396
      },
      "8935": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 396
      },
      "8936": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 396
      },
      "8937": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 396
      },
      "8938": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 396
      },
      "8939": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 396
      },
      "8940": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 396
      },
      "8941": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 396
      },
      "8942": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 396
      },
      "8943": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 396
      },
      "8944": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 396
      },
      "8945": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 396
      },
      "8946": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 396
      },
      "8947": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 396
      },
      "8948": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 396
      },
      "8949": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 396
      },
      "8950": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 396
      },
      "8951": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 396
      },
      "8952": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 396
      },
      "8953": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 396
      },
      "8954": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 396
      },
      "8955": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 396
      },
      "8956": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 396
      },
      "8957": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 396
      },
      "8958": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 396
      },
      "8959": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 396
      },
      "8960": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 397
      },
      "8961": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 397
      },
      "8962": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 397
      },
      "8963": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 397
      },
      "8964": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 397
      },
      "8965": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 397
      },
      "8966": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 397
      },
      "8967": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 397
      },
      "8968": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 397
      },
      "8969": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 397
      },
      "8970": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 397
      },
      "8971": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 397
      },
      "8972": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 397
      },
      "8973": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 397
      },
      "8974": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 397
      },
      "8975": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 397
      },
      "8976": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 397
      },
      "8977": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 397
      },
      "8978": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 397
      },
      "8979": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 397
      },
      "8980": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 397
      },
      "8981": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 397
      },
      "8982": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 397
      },
      "8983": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 397
      },
      "8984": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 397
      },
      "8985": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 397
      },
      "8986": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 397
      },
      "8987": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 397
      },
      "8988": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 397
      },
      "8989": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 397
      },
      "8990": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 397
      },
      "8991": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 397
      },
      "8992": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 397
      },
      "8993": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 397
      },
      "8994": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 397
      },
      "8995": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 397
      },
      "8996": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 397
      },
      "8997": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 397
      },
      "8998": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 397
      },
      "8999": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 397
      },
      "9000": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 397
      },
      "9001": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 397
      },
      "9002": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 397
      },
      "9003": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 397
      },
      "9004": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 397
      },
      "9005": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 397
      },
      "9006": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 397
      },
      "9007": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 397
      },
      "9008": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 397
      },
      "9009": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 397
      },
      "9010": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 397
      },
      "9011": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 397
      },
      "9012": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 397
      },
      "9013": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 397
      },
      "9014": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 397
      },
      "9015": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 398
      },
      "9016": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 398
      },
      "9017": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 398
      },
      "9018": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 398
      },
      "9019": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 398
      },
      "9020": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 398
      },
      "9021": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 398
      },
      "9022": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 398
      },
      "9023": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 398
      },
      "9024": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 398
      },
      "9025": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 398
      },
      "9026": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 398
      },
      "9027": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 398
      },
      "9028": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 398
      },
      "9029": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 398
      },
      "9030": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 398
      },
      "9031": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 398
      },
      "9032": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 398
      },
      "9033": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 398
      },
      "9034": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 398
      },
      "9035": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 398
      },
      "9036": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 398
      },
      "9037": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 398
      },
      "9038": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 398
      },
      "9039": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 398
      },
      "9040": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 398
      },
      "9041": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 398
      },
      "9042": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 398
      },
      "9043": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 398
      },
      "9044": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 398
      },
      "9045": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 398
      },
      "9046": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 398
      },
      "9047": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 398
      },
      "9048": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 398
      },
      "9049": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 398
      },
      "9050": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 398
      },
      "9051": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 398
      },
      "9052": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 398
      },
      "9053": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 398
      },
      "9054": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 398
      },
      "9055": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 398
      },
      "9056": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 398
      },
      "9057": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 398
      },
      "9058": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 398
      },
      "9059": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 398
      },
      "9060": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 398
      },
      "9061": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 398
      },
      "9062": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 398
      },
      "9063": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 398
      },
      "9064": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 398
      },
      "9065": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 398
      },
      "9066": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 398
      },
      "9067": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 398
      },
      "9068": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 398
      },
      "9069": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 398
      },
      "9070": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 399
      },
      "9071": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 399
      },
      "9072": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 399
      },
      "9073": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 399
      },
      "9074": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 399
      },
      "9075": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 399
      },
      "9076": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 399
      },
      "9077": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 399
      },
      "9078": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 399
      },
      "9079": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 399
      },
      "9080": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 399
      },
      "9081": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 399
      },
      "9082": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 399
      },
      "9083": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 399
      },
      "9084": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 399
      },
      "9085": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 399
      },
      "9086": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 399
      },
      "9087": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 399
      },
      "9088": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 399
      },
      "9089": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 399
      },
      "9090": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 399
      },
      "9091": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 399
      },
      "9092": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 399
      },
      "9093": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 399
      },
      "9094": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 399
      },
      "9095": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 399
      },
      "9096": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 399
      },
      "9097": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 399
      },
      "9098": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 399
      },
      "9099": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 399
      },
      "9100": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 399
      },
      "9101": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 399
      },
      "9102": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 399
      },
      "9103": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 399
      },
      "9104": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 399
      },
      "9105": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 399
      },
      "9106": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 399
      },
      "9107": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 399
      },
      "9108": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 399
      },
      "9109": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 399
      },
      "9110": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 399
      },
      "9111": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 399
      },
      "9112": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 399
      },
      "9113": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 399
      },
      "9114": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 399
      },
      "9115": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 399
      },
      "9116": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 399
      },
      "9117": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 399
      },
      "9118": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 399
      },
      "9119": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 399
      },
      "9120": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 399
      },
      "9121": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 399
      },
      "9122": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 399
      },
      "9123": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 399
      },
      "9124": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 399
      },
      "9125": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 400
      },
      "9126": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 400
      },
      "9127": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 400
      },
      "9128": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 400
      },
      "9129": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 400
      },
      "9130": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 400
      },
      "9131": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 400
      },
      "9132": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 400
      },
      "9133": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 400
      },
      "9134": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 400
      },
      "9135": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 400
      },
      "9136": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 400
      },
      "9137": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 400
      },
      "9138": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 400
      },
      "9139": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 400
      },
      "9140": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 400
      },
      "9141": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 400
      },
      "9142": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 400
      },
      "9143": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 400
      },
      "9144": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 400
      },
      "9145": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 400
      },
      "9146": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 400
      },
      "9147": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 400
      },
      "9148": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 400
      },
      "9149": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 400
      },
      "9150": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 400
      },
      "9151": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 400
      },
      "9152": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 400
      },
      "9153": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 400
      },
      "9154": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 400
      },
      "9155": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 400
      },
      "9156": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 400
      },
      "9157": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 400
      },
      "9158": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 400
      },
      "9159": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 400
      },
      "9160": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 400
      },
      "9161": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 400
      },
      "9162": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 400
      },
      "9163": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 400
      },
      "9164": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 400
      },
      "9165": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 400
      },
      "9166": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 400
      },
      "9167": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 400
      },
      "9168": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 400
      },
      "9169": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 400
      },
      "9170": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 400
      },
      "9171": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 400
      },
      "9172": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 400
      },
      "9173": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 400
      },
      "9174": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 400
      },
      "9175": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 400
      },
      "9176": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 400
      },
      "9177": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 400
      },
      "9178": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 400
      },
      "9179": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 400
      },
      "9180": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 401
      },
      "9181": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 401
      },
      "9182": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 401
      },
      "9183": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 401
      },
      "9184": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 401
      },
      "9185": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 401
      },
      "9186": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 401
      },
      "9187": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 401
      },
      "9188": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 401
      },
      "9189": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 401
      },
      "9190": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 401
      },
      "9191": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 401
      },
      "9192": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 401
      },
      "9193": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 401
      },
      "9194": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 401
      },
      "9195": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 401
      },
      "9196": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 401
      },
      "9197": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 401
      },
      "9198": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 401
      },
      "9199": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 401
      },
      "9200": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 401
      },
      "9201": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 401
      },
      "9202": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 401
      },
      "9203": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 401
      },
      "9204": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 401
      },
      "9205": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 401
      },
      "9206": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 401
      },
      "9207": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 401
      },
      "9208": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 401
      },
      "9209": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 401
      },
      "9210": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 401
      },
      "9211": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 401
      },
      "9212": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 401
      },
      "9213": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 401
      },
      "9214": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 401
      },
      "9215": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 401
      },
      "9216": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 401
      },
      "9217": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 401
      },
      "9218": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 401
      },
      "9219": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 401
      },
      "9220": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 401
      },
      "9221": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 401
      },
      "9222": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 401
      },
      "9223": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 401
      },
      "9224": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 401
      },
      "9225": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 401
      },
      "9226": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 401
      },
      "9227": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 401
      },
      "9228": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 401
      },
      "9229": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 401
      },
      "9230": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 401
      },
      "9231": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 401
      },
      "9232": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 401
      },
      "9233": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 401
      },
      "9234": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 401
      },
      "9235": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 402
      },
      "9236": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 402
      },
      "9237": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 402
      },
      "9238": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 402
      },
      "9239": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 402
      },
      "9240": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 402
      },
      "9241": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 402
      },
      "9242": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 402
      },
      "9243": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 402
      },
      "9244": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 402
      },
      "9245": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 402
      },
      "9246": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 402
      },
      "9247": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 402
      },
      "9248": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 402
      },
      "9249": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 402
      },
      "9250": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 402
      },
      "9251": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 402
      },
      "9252": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 402
      },
      "9253": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 402
      },
      "9254": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 402
      },
      "9255": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 402
      },
      "9256": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 402
      },
      "9257": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 402
      },
      "9258": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 402
      },
      "9259": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 402
      },
      "9260": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 402
      },
      "9261": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 402
      },
      "9262": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 402
      },
      "9263": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 402
      },
      "9264": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 402
      },
      "9265": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 402
      },
      "9266": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 402
      },
      "9267": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 402
      },
      "9268": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 402
      },
      "9269": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 402
      },
      "9270": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 402
      },
      "9271": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 402
      },
      "9272": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 402
      },
      "9273": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 402
      },
      "9274": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 402
      },
      "9275": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 402
      },
      "9276": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 402
      },
      "9277": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 402
      },
      "9278": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 402
      },
      "9279": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 402
      },
      "9280": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 402
      },
      "9281": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 402
      },
      "9282": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 402
      },
      "9283": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 402
      },
      "9284": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 402
      },
      "9285": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 402
      },
      "9286": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 402
      },
      "9287": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 402
      },
      "9288": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 402
      },
      "9289": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 402
      },
      "9785": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 412
      },
      "9786": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 413
      },
      "9787": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 413
      },
      "9788": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 413
      },
      "9789": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 413
      },
      "9790": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 413
      },
      "9791": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 413
      },
      "9792": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 413
      },
      "9793": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 413
      },
      "9794": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 413
      },
      "9795": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 413
      },
      "9796": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 413
      },
      "9797": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 413
      },
      "9798": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 413
      },
      "9799": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 413
      },
      "9800": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 413
      },
      "9801": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 413
      },
      "9802": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 413
      },
      "9803": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 413
      },
      "9804": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 413
      },
      "9805": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 413
      },
      "9806": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 413
      },
      "9807": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 413
      },
      "9808": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 413
      },
      "9809": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 413
      },
      "9810": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 414
      },
      "9811": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 414
      },
      "9812": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 414
      },
      "9813": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 414
      },
      "9814": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 414
      },
      "9815": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 414
      },
      "9816": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 414
      },
      "9817": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 414
      },
      "9818": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 414
      },
      "9819": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 414
      },
      "9820": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 414
      },
      "9821": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 414
      },
      "9822": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 414
      },
      "9823": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 414
      },
      "9824": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 414
      },
      "9825": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 414
      },
      "9826": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 414
      },
      "9827": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 414
      },
      "9828": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 414
      },
      "9829": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 414
      },
      "9830": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 414
      },
      "9831": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 414
      },
      "9832": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 414
      },
      "9833": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 414
      },
      "9859": {
        "lookahead": {
          "type": "Token",
          "name": "AS"
        },
        "like": 416
      },
      "9860": {
        "lookahead": {
          "type": "Token",
          "name": "AS"
        },
        "like": 417
      },
      "9861": {
        "lookahead": {
          "type": "Token",
          "name": "AS"
        },
        "like": 418
      },
      "9862": {
        "lookahead": {
          "type": "Token",
          "name": "AS"
        },
        "like": 419
      },
      "9863": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 420
      },
      "9864": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 420
      },
      "9865": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 420
      },
      "9866": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 420
      },
      "9867": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 421
      },
      "9868": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 421
      },
      "9869": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 421
      },
      "9870": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 421
      },
      "9931": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 425
      },
      "9932": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 425
      },
      "9933": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 425
      },
      "9934": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 425
      },
      "9935": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 425
      },
      "9936": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 425
      },
      "9937": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 425
      },
      "9938": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 425
      },
      "9939": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 425
      },
      "9940": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 425
      },
      "9941": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 425
      },
      "9942": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 425
      },
      "9943": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 425
      },
      "9944": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 425
      },
      "9945": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 425
      },
      "9946": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 425
      },
      "9947": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 425
      },
      "9948": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 425
      },
      "9949": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 425
      },
      "9950": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 425
      },
      "9951": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 425
      },
      "9952": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 425
      },
      "9953": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 425
      },
      "9954": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 425
      },
      "9955": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 425
      },
      "9956": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 426
      },
      "9957": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 426
      },
      "9958": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 426
      },
      "9959": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 426
      },
      "9960": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 426
      },
      "9961": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 426
      },
      "9962": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 426
      },
      "9963": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 426
      },
      "10026": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 429
      },
      "10027": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 429
      },
      "10028": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 429
      },
      "10029": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 429
      },
      "10030": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 429
      },
      "10031": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 429
      },
      "10032": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 429
      },
      "10033": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 429
      },
      "10034": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 429
      },
      "10035": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 429
      },
      "10036": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 429
      },
      "10037": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 429
      },
      "10038": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 429
      },
      "10039": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 429
      },
      "10040": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 429
      },
      "10041": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 429
      },
      "10042": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 429
      },
      "10043": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 430
      },
      "10044": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 430
      },
      "10045": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 430
      },
      "10046": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 430
      },
      "10047": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 430
      },
      "10048": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 430
      },
      "10049": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 430
      },
      "10050": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 430
      },
      "10051": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 430
      },
      "10052": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 430
      },
      "10053": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 430
      },
      "10054": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 430
      },
      "10055": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 430
      },
      "10056": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 430
      },
      "10057": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 430
      },
      "10058": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 430
      },
      "10059": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 430
      },
      "10060": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 431
      },
      "10061": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 431
      },
      "10062": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 431
      },
      "10063": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 431
      },
      "10064": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 431
      },
      "10065": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 431
      },
      "10066": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 431
      },
      "10067": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 431
      },
      "10068": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 431
      },
      "10069": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 431
      },
      "10070": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 431
      },
      "10071": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 431
      },
      "10072": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 431
      },
      "10073": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 431
      },
      "10074": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 431
      },
      "10075": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 431
      },
      "10076": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 431
      },
      "10159": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 435
      },
      "10160": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 435
      },
      "10161": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 435
      },
      "10162": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 435
      },
      "10163": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 435
      },
      "10164": {
        "lookahead": {
          "type": "Token",
          "name": "DERIVING"
        },
        "like": 435
      },
      "10165": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 436
      },
      "10166": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 436
      },
      "10167": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 436
      },
      "10168": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 436
      },
      "10169": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 436
      },
      "10170": {
        "lookahead": {
          "type": "Token",
          "name": "DERIVING"
        },
        "like": 436
      },
      "10177": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 333
      },
      "10178": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 333
      },
      "10179": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 333
      },
      "10180": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 333
      },
      "10181": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 333
      },
      "10335": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 332
      },
      "10336": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 332
      },
      "10337": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 332
      },
      "10369": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 443
      },
      "10370": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 443
      },
      "10371": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 443
      },
      "10403": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 445
      },
      "10404": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 445
      },
      "10436": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 447
      },
      "10437": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 447
      },
      "10438": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 447
      },
      "10439": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 447
      },
      "10440": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 447
      },
      "10441": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 447
      },
      "10442": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 447
      },
      "10443": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 447
      },
      "10444": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 447
      },
      "10445": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 447
      },
      "10446": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 447
      },
      "10447": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 447
      },
      "10448": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 447
      },
      "10449": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 447
      },
      "10450": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 447
      },
      "10451": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 447
      },
      "10452": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 447
      },
      "10453": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 448
      },
      "10454": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 448
      },
      "10455": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 448
      },
      "10456": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 448
      },
      "10457": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 448
      },
      "10458": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 448
      },
      "10459": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 448
      },
      "10460": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 448
      },
      "10461": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 448
      },
      "10462": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 448
      },
      "10463": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 448
      },
      "10464": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 448
      },
      "10465": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 448
      },
      "10466": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 448
      },
      "10467": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 448
      },
      "10468": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 448
      },
      "10469": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 448
      },
      "10470": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 449
      },
      "10471": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 449
      },
      "10472": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 449
      },
      "10473": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 449
      },
      "10474": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 449
      },
      "10475": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 449
      },
      "10476": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 449
      },
      "10477": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 449
      },
      "10478": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 449
      },
      "10479": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 449
      },
      "10480": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 449
      },
      "10481": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 449
      },
      "10482": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 449
      },
      "10483": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 449
      },
      "10484": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 449
      },
      "10485": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 449
      },
      "10486": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 449
      },
      "10487": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 450
      },
      "10488": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 450
      },
      "10489": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 450
      },
      "10490": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 450
      },
      "10491": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 450
      },
      "10492": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 450
      },
      "10493": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 450
      },
      "10494": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 450
      },
      "10495": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 450
      },
      "10496": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 450
      },
      "10497": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 450
      },
      "10498": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 450
      },
      "10499": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 450
      },
      "10500": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 450
      },
      "10501": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 450
      },
      "10502": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 450
      },
      "10503": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 450
      },
      "10504": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 450
      },
      "10505": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 450
      },
      "10506": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 450
      },
      "10507": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 450
      },
      "10508": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 450
      },
      "10509": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 450
      },
      "10510": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 450
      },
      "10511": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 450
      },
      "10512": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 450
      },
      "10513": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 450
      },
      "10514": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 450
      },
      "10515": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 450
      },
      "10516": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 450
      },
      "10517": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 450
      },
      "10518": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 450
      },
      "10519": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 450
      },
      "10520": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 450
      },
      "10521": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 450
      },
      "10522": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 450
      },
      "10523": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 450
      },
      "10524": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 450
      },
      "10525": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 450
      },
      "10526": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 450
      },
      "10527": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 451
      },
      "10528": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 451
      },
      "10529": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 451
      },
      "10530": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 451
      },
      "10531": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 451
      },
      "10532": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 451
      },
      "10533": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 451
      },
      "10534": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 451
      },
      "10535": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 451
      },
      "10536": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 451
      },
      "10537": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 451
      },
      "10538": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 451
      },
      "10539": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 451
      },
      "10540": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 451
      },
      "10541": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 451
      },
      "10542": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 451
      },
      "10543": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 451
      },
      "10544": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 451
      },
      "10545": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 451
      },
      "10546": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 451
      },
      "10547": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 451
      },
      "10548": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 451
      },
      "10549": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 451
      },
      "10550": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 451
      },
      "10551": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 451
      },
      "10552": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 451
      },
      "10553": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 451
      },
      "10554": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 451
      },
      "10555": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 451
      },
      "10556": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 451
      },
      "10557": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 451
      },
      "10558": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 451
      },
      "10559": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 451
      },
      "10560": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 451
      },
      "10561": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 451
      },
      "10562": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 451
      },
      "10563": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 451
      },
      "10564": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 451
      },
      "10565": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 451
      },
      "10566": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 451
      },
      "10618": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 453
      },
      "10619": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 453
      },
      "10620": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 453
      },
      "10621": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 453
      },
      "10622": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 453
      },
      "10623": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 453
      },
      "10624": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 453
      },
      "10625": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 453
      },
      "10626": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 453
      },
      "10627": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 453
      },
      "10628": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 453
      },
      "10629": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 453
      },
      "10630": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 453
      },
      "10631": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 453
      },
      "10632": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 453
      },
      "10633": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 453
      },
      "10634": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 453
      },
      "10635": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 454
      },
      "10636": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 454
      },
      "10637": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 454
      },
      "10638": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 454
      },
      "10639": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 454
      },
      "10640": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 454
      },
      "10641": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 454
      },
      "10642": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 454
      },
      "10643": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 454
      },
      "10644": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 454
      },
      "10645": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 454
      },
      "10646": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 454
      },
      "10647": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 454
      },
      "10648": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 454
      },
      "10649": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 454
      },
      "10650": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 454
      },
      "10651": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 454
      },
      "10652": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 455
      },
      "10653": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 455
      },
      "10654": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 455
      },
      "10655": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 455
      },
      "10656": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 455
      },
      "10657": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 455
      },
      "10658": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 455
      },
      "10659": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 455
      },
      "10660": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 455
      },
      "10661": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 455
      },
      "10662": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 455
      },
      "10663": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 455
      },
      "10664": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 455
      },
      "10665": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 455
      },
      "10666": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 455
      },
      "10667": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 455
      },
      "10668": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 455
      },
      "10669": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 456
      },
      "10670": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 456
      },
      "10671": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 456
      },
      "10672": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 456
      },
      "10673": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 456
      },
      "10674": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 456
      },
      "10675": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 456
      },
      "10676": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 456
      },
      "10677": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 456
      },
      "10678": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 456
      },
      "10679": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 456
      },
      "10680": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 456
      },
      "10681": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 456
      },
      "10682": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 456
      },
      "10683": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 456
      },
      "10684": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 456
      },
      "10685": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 456
      },
      "10686": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 457
      },
      "10687": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 457
      },
      "10688": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 457
      },
      "10689": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 457
      },
      "10690": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 457
      },
      "10691": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 457
      },
      "10692": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 457
      },
      "10693": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 457
      },
      "10694": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 457
      },
      "10695": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 457
      },
      "10696": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 457
      },
      "10697": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 457
      },
      "10698": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 457
      },
      "10699": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 457
      },
      "10700": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 457
      },
      "10701": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 457
      },
      "10702": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 457
      },
      "10703": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 458
      },
      "10704": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 458
      },
      "10705": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 458
      },
      "10706": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 458
      },
      "10707": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 458
      },
      "10708": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 458
      },
      "10709": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 458
      },
      "10710": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 458
      },
      "10711": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 458
      },
      "10712": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 458
      },
      "10713": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 458
      },
      "10714": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 458
      },
      "10715": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 458
      },
      "10716": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 458
      },
      "10717": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 458
      },
      "10718": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 458
      },
      "10719": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 458
      },
      "10720": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 459
      },
      "10721": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 459
      },
      "10722": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 459
      },
      "10723": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 459
      },
      "10724": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 459
      },
      "10725": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 459
      },
      "10726": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 459
      },
      "10727": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 459
      },
      "10728": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 459
      },
      "10729": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 459
      },
      "10730": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 459
      },
      "10731": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 459
      },
      "10732": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 459
      },
      "10733": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 459
      },
      "10734": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 459
      },
      "10735": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 459
      },
      "10736": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 459
      },
      "10737": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 460
      },
      "10738": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 460
      },
      "10739": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 460
      },
      "10740": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 460
      },
      "10741": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 460
      },
      "10742": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 460
      },
      "10743": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 460
      },
      "10744": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 460
      },
      "10745": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 460
      },
      "10746": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 460
      },
      "10747": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 460
      },
      "10748": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 460
      },
      "10749": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 460
      },
      "10750": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 460
      },
      "10751": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 460
      },
      "10752": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 460
      },
      "10753": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 460
      },
      "10754": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 461
      },
      "10755": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 461
      },
      "10756": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 461
      },
      "10757": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 461
      },
      "10758": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 461
      },
      "10759": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 461
      },
      "10760": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 461
      },
      "10761": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 461
      },
      "10762": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 461
      },
      "10763": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 461
      },
      "10764": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 461
      },
      "10765": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 461
      },
      "10766": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 461
      },
      "10767": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 461
      },
      "10768": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 461
      },
      "10769": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 461
      },
      "10770": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 461
      },
      "10936": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 465
      },
      "10937": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 465
      },
      "10938": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 465
      },
      "10939": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 465
      },
      "10940": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 465
      },
      "10941": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 465
      },
      "10942": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 465
      },
      "10943": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 465
      },
      "10944": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 465
      },
      "10945": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 465
      },
      "10946": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 465
      },
      "10947": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 465
      },
      "10948": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 465
      },
      "10949": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 465
      },
      "10950": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 465
      },
      "10951": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 465
      },
      "10952": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 465
      },
      "10953": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 465
      },
      "10954": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 465
      },
      "10955": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 465
      },
      "10956": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 465
      },
      "10957": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 465
      },
      "10958": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 465
      },
      "10959": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 465
      },
      "10960": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 465
      },
      "10961": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 465
      },
      "10962": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 465
      },
      "10963": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 465
      },
      "10964": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 465
      },
      "10965": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 465
      },
      "10966": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 465
      },
      "10967": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 465
      },
      "10968": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 465
      },
      "10969": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 465
      },
      "10970": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 465
      },
      "10971": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 465
      },
      "10972": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 465
      },
      "10973": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 465
      },
      "10974": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 465
      },
      "10975": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 465
      },
      "10976": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 465
      },
      "10977": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 465
      },
      "10978": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 465
      },
      "10979": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 465
      },
      "10980": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 465
      },
      "10981": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 465
      },
      "10982": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 465
      },
      "10983": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 465
      },
      "10984": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 465
      },
      "10985": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 465
      },
      "10986": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 465
      },
      "10987": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 465
      },
      "10988": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 465
      },
      "10989": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 465
      },
      "10990": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 465
      },
      "11321": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 472
      },
      "11322": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 472
      },
      "11323": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 472
      },
      "11324": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 472
      },
      "11325": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 472
      },
      "11326": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 472
      },
      "11327": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 472
      },
      "11328": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 472
      },
      "11329": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 472
      },
      "11330": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 472
      },
      "11331": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 472
      },
      "11332": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 472
      },
      "11333": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 472
      },
      "11334": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 472
      },
      "11335": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 472
      },
      "11336": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 472
      },
      "11337": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 472
      },
      "11338": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 472
      },
      "11339": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 472
      },
      "11340": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 472
      },
      "11341": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 472
      },
      "11342": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 472
      },
      "11343": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 472
      },
      "11344": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 472
      },
      "11345": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 472
      },
      "11346": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 472
      },
      "11347": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 472
      },
      "11348": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 472
      },
      "11349": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 472
      },
      "11350": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 472
      },
      "11351": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 472
      },
      "11352": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 472
      },
      "11353": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 472
      },
      "11354": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 472
      },
      "11355": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 472
      },
      "11356": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 472
      },
      "11357": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 472
      },
      "11358": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 472
      },
      "11359": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 472
      },
      "11360": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 472
      },
      "11361": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 472
      },
      "11362": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 472
      },
      "11363": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 472
      },
      "11364": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 472
      },
      "11365": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 472
      },
      "11366": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 472
      },
      "11367": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 472
      },
      "11368": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 472
      },
      "11369": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 472
      },
      "11370": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 472
      },
      "11371": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 472
      },
      "11372": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 473
      },
      "11373": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 473
      },
      "11374": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 473
      },
      "11375": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 473
      },
      "11376": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 473
      },
      "11377": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 473
      },
      "11378": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 473
      },
      "11379": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 473
      },
      "11380": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 473
      },
      "11381": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 473
      },
      "11382": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 473
      },
      "11383": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 473
      },
      "11384": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 473
      },
      "11385": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 473
      },
      "11386": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 473
      },
      "11387": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 473
      },
      "11388": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 473
      },
      "11389": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 473
      },
      "11390": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 473
      },
      "11391": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 473
      },
      "11392": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 473
      },
      "11393": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 473
      },
      "11394": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 473
      },
      "11395": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 473
      },
      "11396": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 473
      },
      "11397": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 473
      },
      "11398": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 473
      },
      "11399": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 473
      },
      "11400": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 473
      },
      "11401": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 473
      },
      "11402": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 473
      },
      "11403": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 473
      },
      "11404": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 473
      },
      "11405": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 473
      },
      "11406": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 473
      },
      "11407": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 473
      },
      "11408": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 473
      },
      "11409": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 473
      },
      "11410": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 473
      },
      "11411": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 473
      },
      "11412": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 473
      },
      "11413": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 473
      },
      "11414": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 473
      },
      "11415": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 473
      },
      "11416": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 473
      },
      "11417": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 473
      },
      "11418": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 473
      },
      "11419": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 473
      },
      "11420": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 473
      },
      "11421": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 473
      },
      "11422": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 473
      },
      "11423": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 473
      },
      "11424": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 473
      },
      "11425": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 473
      },
      "11426": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 473
      },
      "11485": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 476
      },
      "11486": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 476
      },
      "11487": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 477
      },
      "11488": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 477
      },
      "11489": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 478
      },
      "11490": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 478
      },
      "11491": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 478
      },
      "11492": {
        "lookahead": {
          "type": "Token",
          "name": "COLONCOLON"
        },
        "like": 478
      },
      "11550": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 481
      },
      "11551": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 481
      },
      "11552": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 481
      },
      "11553": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 481
      },
      "11554": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 481
      },
      "11555": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 481
      },
      "11556": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 481
      },
      "11557": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 481
      },
      "11558": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 481
      },
      "11559": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 481
      },
      "11560": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 481
      },
      "11561": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 481
      },
      "11562": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 481
      },
      "11563": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 481
      },
      "11564": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 481
      },
      "11565": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 481
      },
      "11566": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 481
      },
      "11567": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 481
      },
      "11568": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 481
      },
      "11569": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 481
      },
      "11570": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 481
      },
      "11571": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 481
      },
      "11572": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 481
      },
      "11573": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 481
      },
      "11574": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 481
      },
      "11575": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 481
      },
      "11576": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 481
      },
      "11577": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 481
      },
      "11578": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 481
      },
      "11579": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 481
      },
      "11580": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 481
      },
      "11581": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 481
      },
      "11582": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 481
      },
      "11583": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 481
      },
      "11584": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 481
      },
      "11585": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 481
      },
      "11586": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 481
      },
      "11587": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 481
      },
      "11588": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 481
      },
      "11589": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 481
      },
      "11590": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 481
      },
      "11591": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 481
      },
      "11592": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 481
      },
      "11593": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 481
      },
      "11594": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 481
      },
      "11595": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 481
      },
      "11596": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 481
      },
      "11597": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 481
      },
      "11598": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 481
      },
      "11599": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 481
      },
      "11600": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 481
      },
      "11601": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 481
      },
      "11602": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 481
      },
      "11603": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 481
      },
      "11604": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 481
      },
      "11609": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 484
      },
      "11610": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 484
      },
      "11611": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 484
      },
      "11612": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 485
      },
      "11613": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 485
      },
      "11614": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 485
      },
      "11640": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 490
      },
      "11641": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 490
      },
      "11642": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 490
      },
      "11643": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 490
      },
      "11644": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 490
      },
      "11645": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 490
      },
      "11646": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 490
      },
      "11647": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 490
      },
      "11648": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 490
      },
      "11649": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 490
      },
      "11650": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 490
      },
      "11651": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 490
      },
      "11652": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 490
      },
      "11653": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 490
      },
      "11654": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 490
      },
      "11655": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 490
      },
      "11656": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 490
      },
      "11712": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 492
      },
      "11714": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 494
      },
      "11715": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 494
      },
      "11716": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 494
      },
      "11717": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 494
      },
      "11718": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 494
      },
      "11719": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 494
      },
      "11720": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 494
      },
      "11721": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 494
      },
      "11722": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 494
      },
      "11723": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 494
      },
      "11724": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 494
      },
      "11725": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 494
      },
      "11726": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 494
      },
      "11727": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 494
      },
      "11728": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 494
      },
      "11729": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 494
      },
      "11730": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 494
      },
      "11786": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 496
      },
      "11787": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 496
      },
      "11788": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 496
      },
      "11789": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 496
      },
      "11790": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 497
      },
      "11791": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 497
      },
      "11792": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 497
      },
      "11793": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 497
      },
      "12039": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 503
      },
      "12040": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 503
      },
      "12041": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 503
      },
      "12042": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 503
      },
      "12043": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 503
      },
      "12044": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 503
      },
      "12045": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 503
      },
      "12046": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 503
      },
      "12047": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 503
      },
      "12048": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 503
      },
      "12049": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 503
      },
      "12061": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 505
      },
      "12062": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 505
      },
      "12063": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 505
      },
      "12064": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 505
      },
      "12071": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 507
      },
      "12072": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 507
      },
      "12073": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 507
      },
      "12074": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 507
      },
      "12075": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 507
      },
      "12076": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 508
      },
      "12077": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 508
      },
      "12078": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 508
      },
      "12079": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 508
      },
      "12080": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 508
      },
      "12101": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 511
      },
      "12102": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 511
      },
      "12103": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 511
      },
      "12104": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 511
      },
      "12105": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 511
      },
      "12106": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 511
      },
      "12107": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 511
      },
      "12108": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 511
      },
      "12109": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 511
      },
      "12110": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 511
      },
      "12111": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 512
      },
      "12112": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 513
      },
      "12113": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 513
      },
      "12114": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 513
      },
      "12115": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 513
      },
      "12116": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 513
      },
      "12117": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 513
      },
      "12118": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 513
      },
      "12119": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 513
      },
      "12120": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 513
      },
      "12121": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 513
      },
      "12122": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 514
      },
      "12123": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 514
      },
      "12124": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 514
      },
      "12125": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 514
      },
      "12126": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 514
      },
      "12127": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 514
      },
      "12128": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 514
      },
      "12129": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 514
      },
      "12130": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 514
      },
      "12131": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 514
      },
      "12132": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 515
      },
      "12133": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 515
      },
      "12134": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 515
      },
      "12135": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 515
      },
      "12136": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 515
      },
      "12137": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 515
      },
      "12138": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 515
      },
      "12139": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 515
      },
      "12140": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 515
      },
      "12141": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 515
      },
      "12142": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 516
      },
      "12143": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 516
      },
      "12144": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 516
      },
      "12145": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 516
      },
      "12146": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 516
      },
      "12147": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 516
      },
      "12148": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 516
      },
      "12149": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 516
      },
      "12150": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 516
      },
      "12151": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 516
      },
      "12152": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 517
      },
      "12153": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 517
      },
      "12154": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 517
      },
      "12155": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 517
      },
      "12156": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 517
      },
      "12157": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 517
      },
      "12158": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 517
      },
      "12159": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 517
      },
      "12160": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 517
      },
      "12161": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 517
      },
      "12162": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 518
      },
      "12183": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 521
      },
      "12184": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 521
      },
      "12185": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 521
      },
      "12186": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 521
      },
      "12187": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 521
      },
      "12188": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 521
      },
      "12189": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 521
      },
      "12190": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 521
      },
      "12191": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 521
      },
      "12192": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 521
      },
      "12193": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 521
      },
      "12194": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 521
      },
      "12195": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 521
      },
      "12196": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 521
      },
      "12197": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 521
      },
      "12198": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 521
      },
      "12199": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 521
      },
      "12200": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 521
      },
      "12201": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 521
      },
      "12202": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 521
      },
      "12203": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 521
      },
      "12204": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 521
      },
      "12205": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 521
      },
      "12206": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 521
      },
      "12207": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 521
      },
      "12208": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 521
      },
      "12209": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 521
      },
      "12210": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 521
      },
      "12211": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 521
      },
      "12212": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 521
      },
      "12213": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 521
      },
      "12214": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 522
      },
      "12215": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 522
      },
      "12216": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 522
      },
      "12217": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 522
      },
      "12218": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 522
      },
      "12219": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 522
      },
      "12220": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 522
      },
      "12221": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 522
      },
      "12222": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 522
      },
      "12223": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 522
      },
      "12224": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 522
      },
      "12225": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 522
      },
      "12226": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 522
      },
      "12227": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 522
      },
      "12228": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 522
      },
      "12229": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 522
      },
      "12230": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 522
      },
      "12231": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 522
      },
      "12232": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 522
      },
      "12233": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 522
      },
      "12234": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 522
      },
      "12235": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 522
      },
      "12236": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 522
      },
      "12237": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 522
      },
      "12238": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 522
      },
      "12239": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 523
      },
      "12240": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 523
      },
      "12241": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 523
      },
      "12242": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 523
      },
      "12243": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 523
      },
      "12244": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 523
      },
      "12245": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 523
      },
      "12246": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 523
      },
      "12247": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 523
      },
      "12248": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 523
      },
      "12249": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 523
      },
      "12250": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 523
      },
      "12251": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 523
      },
      "12252": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 523
      },
      "12253": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 523
      },
      "12254": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 523
      },
      "12255": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 523
      },
      "12256": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 523
      },
      "12257": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 523
      },
      "12258": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 523
      },
      "12259": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 523
      },
      "12260": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 523
      },
      "12261": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 523
      },
      "12262": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 523
      },
      "12263": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 523
      },
      "12264": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 523
      },
      "12265": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 523
      },
      "12266": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 523
      },
      "12267": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 523
      },
      "12268": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 523
      },
      "12269": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 523
      },
      "12270": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 523
      },
      "12271": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 523
      },
      "12272": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 523
      },
      "12273": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 523
      },
      "12274": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 523
      },
      "12275": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 523
      },
      "12276": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 523
      },
      "12277": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 523
      },
      "12278": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 523
      },
      "12279": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 523
      },
      "12280": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 523
      },
      "12281": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 523
      },
      "12282": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 523
      },
      "12283": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 523
      },
      "12284": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 523
      },
      "12285": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 523
      },
      "12286": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 523
      },
      "12287": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 523
      },
      "12288": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 523
      },
      "12289": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 523
      },
      "12290": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 523
      },
      "12291": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 523
      },
      "12292": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 523
      },
      "12293": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 523
      },
      "12294": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 523
      },
      "12295": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 524
      },
      "12296": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 524
      },
      "12297": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 524
      },
      "12298": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 524
      },
      "12299": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 524
      },
      "12300": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 524
      },
      "12301": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 524
      },
      "12302": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 524
      },
      "12303": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 524
      },
      "12304": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 524
      },
      "12305": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 524
      },
      "12306": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 524
      },
      "12307": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 524
      },
      "12308": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 524
      },
      "12309": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 524
      },
      "12310": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 524
      },
      "12311": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 524
      },
      "12312": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 524
      },
      "12313": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 524
      },
      "12314": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 524
      },
      "12315": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 524
      },
      "12316": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 524
      },
      "12317": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 524
      },
      "12318": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 524
      },
      "12319": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 524
      },
      "12320": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 524
      },
      "12321": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 524
      },
      "12322": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 524
      },
      "12323": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 524
      },
      "12324": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 524
      },
      "12325": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 524
      },
      "12326": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 524
      },
      "12327": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 524
      },
      "12328": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 524
      },
      "12329": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 524
      },
      "12330": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 524
      },
      "12331": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 524
      },
      "12332": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 524
      },
      "12333": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 524
      },
      "12334": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 524
      },
      "12335": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 524
      },
      "12336": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 524
      },
      "12337": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 524
      },
      "12338": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 524
      },
      "12339": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 524
      },
      "12340": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 524
      },
      "12341": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 524
      },
      "12342": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 524
      },
      "12343": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 524
      },
      "12344": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 524
      },
      "12345": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 524
      },
      "12346": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 524
      },
      "12347": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 524
      },
      "12348": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 524
      },
      "12349": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 524
      },
      "12350": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 524
      },
      "12351": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 525
      },
      "12352": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 525
      },
      "12353": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 525
      },
      "12354": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 525
      },
      "12355": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 525
      },
      "12356": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 525
      },
      "12357": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 525
      },
      "12358": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 525
      },
      "12359": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 525
      },
      "12360": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 525
      },
      "12361": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 525
      },
      "12362": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 525
      },
      "12363": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 525
      },
      "12364": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 525
      },
      "12365": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 525
      },
      "12366": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 525
      },
      "12367": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 525
      },
      "12368": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 525
      },
      "12369": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 525
      },
      "12370": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 525
      },
      "12371": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 525
      },
      "12372": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 525
      },
      "12373": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 525
      },
      "12374": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 525
      },
      "12375": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 525
      },
      "12376": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 525
      },
      "12377": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 525
      },
      "12378": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 525
      },
      "12379": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 525
      },
      "12380": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 525
      },
      "12381": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 525
      },
      "12382": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 526
      },
      "12383": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 526
      },
      "12384": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 526
      },
      "12385": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 526
      },
      "12386": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 526
      },
      "12387": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 526
      },
      "12388": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 526
      },
      "12389": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 526
      },
      "12390": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 526
      },
      "12391": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 526
      },
      "12392": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 526
      },
      "12393": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 526
      },
      "12394": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 526
      },
      "12395": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 526
      },
      "12396": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 526
      },
      "12397": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 526
      },
      "12398": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 526
      },
      "12399": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 526
      },
      "12400": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 526
      },
      "12401": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 526
      },
      "12402": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 526
      },
      "12403": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 526
      },
      "12404": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 526
      },
      "12405": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 526
      },
      "12406": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 526
      },
      "12407": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 526
      },
      "12408": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 526
      },
      "12409": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 526
      },
      "12410": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 526
      },
      "12411": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 526
      },
      "12412": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 526
      },
      "12445": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 529
      },
      "12446": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 529
      },
      "12502": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 531
      },
      "12503": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 531
      },
      "12511": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 534
      },
      "12512": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 535
      },
      "12513": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 536
      },
      "12514": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 536
      },
      "12515": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 536
      },
      "12516": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 536
      },
      "12517": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 536
      },
      "12518": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 536
      },
      "12519": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 536
      },
      "12520": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 536
      },
      "12521": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 536
      },
      "12522": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 536
      },
      "12523": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 536
      },
      "12524": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 536
      },
      "12525": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 536
      },
      "12526": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 536
      },
      "12527": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 536
      },
      "12528": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 536
      },
      "12529": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 536
      },
      "12530": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 536
      },
      "12531": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 536
      },
      "12532": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 536
      },
      "12533": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 536
      },
      "12534": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 536
      },
      "12535": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 536
      },
      "12536": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 536
      },
      "12537": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 536
      },
      "12538": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 536
      },
      "12539": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 536
      },
      "12540": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 536
      },
      "12541": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 536
      },
      "12542": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 536
      },
      "12543": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 536
      },
      "12544": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 536
      },
      "12545": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 536
      },
      "12546": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 536
      },
      "12547": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 536
      },
      "12548": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 536
      },
      "12549": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 536
      },
      "12550": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 536
      },
      "12551": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 536
      },
      "12552": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 536
      },
      "12553": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 536
      },
      "12554": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 536
      },
      "12555": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 536
      },
      "12556": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 536
      },
      "12557": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 536
      },
      "12558": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 536
      },
      "12559": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 536
      },
      "12560": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 536
      },
      "12561": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 536
      },
      "12562": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 536
      },
      "12563": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 536
      },
      "12564": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 536
      },
      "12565": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 536
      },
      "12566": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 536
      },
      "12567": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 536
      },
      "12573": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 537
      },
      "12574": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 537
      },
      "12575": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 537
      },
      "12576": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 537
      },
      "12577": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 537
      },
      "12578": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 537
      },
      "12579": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 537
      },
      "12580": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 537
      },
      "12581": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 537
      },
      "12582": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 537
      },
      "12583": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 537
      },
      "12584": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 537
      },
      "12585": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 537
      },
      "12586": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 537
      },
      "12587": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 537
      },
      "12588": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 537
      },
      "12589": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 537
      },
      "12590": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 537
      },
      "12591": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 537
      },
      "12592": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 537
      },
      "12593": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 537
      },
      "12594": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 537
      },
      "12595": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 537
      },
      "12596": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 537
      },
      "12597": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 537
      },
      "12598": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 537
      },
      "12599": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 537
      },
      "12600": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 537
      },
      "12601": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 537
      },
      "12602": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 537
      },
      "12603": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 537
      },
      "12697": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 541
      },
      "12698": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 541
      },
      "12699": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 541
      },
      "12700": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 541
      },
      "12701": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 541
      },
      "12702": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 541
      },
      "12703": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 541
      },
      "12704": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 541
      },
      "12705": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 541
      },
      "12706": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 541
      },
      "12707": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 541
      },
      "12708": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 541
      },
      "12709": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 541
      },
      "12710": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 541
      },
      "12711": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 541
      },
      "12712": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 541
      },
      "12713": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 541
      },
      "12714": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 541
      },
      "12715": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 541
      },
      "12716": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 541
      },
      "12717": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 541
      },
      "12718": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 541
      },
      "12719": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 541
      },
      "12720": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 541
      },
      "12721": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 541
      },
      "12722": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 541
      },
      "12723": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 541
      },
      "12724": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 541
      },
      "12725": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 541
      },
      "12726": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 541
      },
      "12727": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 541
      },
      "12728": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 542
      },
      "12729": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 542
      },
      "12761": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 544
      },
      "12762": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 544
      },
      "12763": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 544
      },
      "12764": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 544
      },
      "12765": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 544
      },
      "12766": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 544
      },
      "12767": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 544
      },
      "12768": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 544
      },
      "12769": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 544
      },
      "12770": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 544
      },
      "12771": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 544
      },
      "12772": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 544
      },
      "12773": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 544
      },
      "12774": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 544
      },
      "12775": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 544
      },
      "12776": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 544
      },
      "12777": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 544
      },
      "12778": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 544
      },
      "12779": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 544
      },
      "12780": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 544
      },
      "12781": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 544
      },
      "12782": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 544
      },
      "12783": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 544
      },
      "12784": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 544
      },
      "12785": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 544
      },
      "12786": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 544
      },
      "12787": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 544
      },
      "12788": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 544
      },
      "12789": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 544
      },
      "12790": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 544
      },
      "12791": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 544
      },
      "12792": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 544
      },
      "12793": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 544
      },
      "12794": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 544
      },
      "12795": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 544
      },
      "12796": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 544
      },
      "12797": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 544
      },
      "12798": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 544
      },
      "12799": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 544
      },
      "12800": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 544
      },
      "12801": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 545
      },
      "12802": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 545
      },
      "12803": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 545
      },
      "12804": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 545
      },
      "12805": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 545
      },
      "12806": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 545
      },
      "12807": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 545
      },
      "12808": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 545
      },
      "12809": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 545
      },
      "12810": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 545
      },
      "12811": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 545
      },
      "12812": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 545
      },
      "12813": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 545
      },
      "12814": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 545
      },
      "12815": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 545
      },
      "12816": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 545
      },
      "12817": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 545
      },
      "12818": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 545
      },
      "12819": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 545
      },
      "12820": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 545
      },
      "12821": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 545
      },
      "12822": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 545
      },
      "12823": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 545
      },
      "12824": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 545
      },
      "12825": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 545
      },
      "12826": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 545
      },
      "12827": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 545
      },
      "12828": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 545
      },
      "12829": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 545
      },
      "12830": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 545
      },
      "12831": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 545
      },
      "12832": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 545
      },
      "12833": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 545
      },
      "12834": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 545
      },
      "12835": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 545
      },
      "12836": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 545
      },
      "12837": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 545
      },
      "12838": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 545
      },
      "12839": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 545
      },
      "12840": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 545
      },
      "12841": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 545
      },
      "12842": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 545
      },
      "12843": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 545
      },
      "12844": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 545
      },
      "12845": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 545
      },
      "12846": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 545
      },
      "12847": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 545
      },
      "12848": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 545
      },
      "12849": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 545
      },
      "12850": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 545
      },
      "12851": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 545
      },
      "12852": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 546
      },
      "12853": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 546
      },
      "12854": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 546
      },
      "12855": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 546
      },
      "12856": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 546
      },
      "12857": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 546
      },
      "12858": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 546
      },
      "12859": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 546
      },
      "12860": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 546
      },
      "12861": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 546
      },
      "12862": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 546
      },
      "12863": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 546
      },
      "12864": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 546
      },
      "12865": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 546
      },
      "12866": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 546
      },
      "12867": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 546
      },
      "12868": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 546
      },
      "12869": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 546
      },
      "12870": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 546
      },
      "12871": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 546
      },
      "12872": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 546
      },
      "12873": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 546
      },
      "12874": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 546
      },
      "12875": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 546
      },
      "12876": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 546
      },
      "12877": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 546
      },
      "12878": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 546
      },
      "12879": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 546
      },
      "12880": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 546
      },
      "12881": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 546
      },
      "12882": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 546
      },
      "12883": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 546
      },
      "12884": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 546
      },
      "12885": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 546
      },
      "12886": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 546
      },
      "12887": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 546
      },
      "12888": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 546
      },
      "12889": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 546
      },
      "12890": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 546
      },
      "12891": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 546
      },
      "12892": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 546
      },
      "12893": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 546
      },
      "12894": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 546
      },
      "12895": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 546
      },
      "12896": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 546
      },
      "12897": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 546
      },
      "12898": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 546
      },
      "12899": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 546
      },
      "12900": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 546
      },
      "12901": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 546
      },
      "12902": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 546
      },
      "12903": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 546
      },
      "12904": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 546
      },
      "12905": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 546
      },
      "12906": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 546
      },
      "13034": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 550
      },
      "13036": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 552
      },
      "13037": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 552
      },
      "13038": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 552
      },
      "13039": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 552
      },
      "13040": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 552
      },
      "13041": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 552
      },
      "13042": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 552
      },
      "13043": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 552
      },
      "13044": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 552
      },
      "13045": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 552
      },
      "13046": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 552
      },
      "13047": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 552
      },
      "13048": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 552
      },
      "13049": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 552
      },
      "13050": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 552
      },
      "13051": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 552
      },
      "13052": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 552
      },
      "13053": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 553
      },
      "13054": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 553
      },
      "13055": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 553
      },
      "13056": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 553
      },
      "13057": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 553
      },
      "13058": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 553
      },
      "13059": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 553
      },
      "13060": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 553
      },
      "13061": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 553
      },
      "13062": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 553
      },
      "13063": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 553
      },
      "13064": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 553
      },
      "13065": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 553
      },
      "13066": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 553
      },
      "13067": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 553
      },
      "13068": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 553
      },
      "13069": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 553
      },
      "13070": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 554
      },
      "13127": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 557
      },
      "13128": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 557
      },
      "13129": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 557
      },
      "13130": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 557
      },
      "13131": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 557
      },
      "13132": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 557
      },
      "13133": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 557
      },
      "13134": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 557
      },
      "13135": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 557
      },
      "13136": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 557
      },
      "13137": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 557
      },
      "13138": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 557
      },
      "13139": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 557
      },
      "13140": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 557
      },
      "13141": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 557
      },
      "13142": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 557
      },
      "13143": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 557
      },
      "13144": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 557
      },
      "13145": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 557
      },
      "13146": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 557
      },
      "13147": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 557
      },
      "13148": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 557
      },
      "13149": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 557
      },
      "13150": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 557
      },
      "13151": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 557
      },
      "13152": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 557
      },
      "13153": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 557
      },
      "13154": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 557
      },
      "13155": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 557
      },
      "13156": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 557
      },
      "13157": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 557
      },
      "13158": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 557
      },
      "13159": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 557
      },
      "13160": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 557
      },
      "13161": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 557
      },
      "13162": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 557
      },
      "13163": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 557
      },
      "13164": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 557
      },
      "13165": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 557
      },
      "13166": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 557
      },
      "13167": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 557
      },
      "13168": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 557
      },
      "13169": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 557
      },
      "13170": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 557
      },
      "13171": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 557
      },
      "13172": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 557
      },
      "13173": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 557
      },
      "13174": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 557
      },
      "13175": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 557
      },
      "13176": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 557
      },
      "13177": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 557
      },
      "13178": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 557
      },
      "13179": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 557
      },
      "13180": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 557
      },
      "13181": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 557
      },
      "13292": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 560
      },
      "13293": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 560
      },
      "13294": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 560
      },
      "13295": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 560
      },
      "13296": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 560
      },
      "13297": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 560
      },
      "13298": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 560
      },
      "13299": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 560
      },
      "13300": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 560
      },
      "13301": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 560
      },
      "13302": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 560
      },
      "13303": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 560
      },
      "13304": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 560
      },
      "13305": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 560
      },
      "13306": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 560
      },
      "13307": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 560
      },
      "13308": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 560
      },
      "13309": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 560
      },
      "13310": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 560
      },
      "13311": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 560
      },
      "13312": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 560
      },
      "13313": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 560
      },
      "13314": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 560
      },
      "13315": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 560
      },
      "13316": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 560
      },
      "13317": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 560
      },
      "13318": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 560
      },
      "13319": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 560
      },
      "13320": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 560
      },
      "13321": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 560
      },
      "13322": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 560
      },
      "13323": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 560
      },
      "13324": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 560
      },
      "13325": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 560
      },
      "13326": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 560
      },
      "13327": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 560
      },
      "13328": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 560
      },
      "13329": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 560
      },
      "13330": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 560
      },
      "13331": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 560
      },
      "13332": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 560
      },
      "13333": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 560
      },
      "13334": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 560
      },
      "13335": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 560
      },
      "13336": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 560
      },
      "13337": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 560
      },
      "13338": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 560
      },
      "13339": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 560
      },
      "13340": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 560
      },
      "13341": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 560
      },
      "13342": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 560
      },
      "13343": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 560
      },
      "13344": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 560
      },
      "13345": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 560
      },
      "13346": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 560
      },
      "13457": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 563
      },
      "13458": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 564
      },
      "13464": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 568
      },
      "13466": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 570
      },
      "13467": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 571
      },
      "13470": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 573
      },
      "13471": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 573
      },
      "13472": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 573
      },
      "13473": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 573
      },
      "13474": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 573
      },
      "13475": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 573
      },
      "13476": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 573
      },
      "13477": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 573
      },
      "13478": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 573
      },
      "13479": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 573
      },
      "13480": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 573
      },
      "13481": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 573
      },
      "13482": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 573
      },
      "13483": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 573
      },
      "13484": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 573
      },
      "13485": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 573
      },
      "13486": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 573
      },
      "13487": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 573
      },
      "13488": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 573
      },
      "13489": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 573
      },
      "13490": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 573
      },
      "13491": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 573
      },
      "13492": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 573
      },
      "13493": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 573
      },
      "13494": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 573
      },
      "13495": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 573
      },
      "13496": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 573
      },
      "13497": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 573
      },
      "13498": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 573
      },
      "13499": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 573
      },
      "13500": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 573
      },
      "13501": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 573
      },
      "13502": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 573
      },
      "13503": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 573
      },
      "13504": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 573
      },
      "13505": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 573
      },
      "13506": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 573
      },
      "13507": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 573
      },
      "13508": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 573
      },
      "13509": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 573
      },
      "13510": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 573
      },
      "13511": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 573
      },
      "13512": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 573
      },
      "13513": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 573
      },
      "13514": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 573
      },
      "13515": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 573
      },
      "13516": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 573
      },
      "13517": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 573
      },
      "13518": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 573
      },
      "13519": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 573
      },
      "13520": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 573
      },
      "13521": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 573
      },
      "13522": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 573
      },
      "13523": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 573
      },
      "13524": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 573
      },
      "13525": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 574
      },
      "13526": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 575
      },
      "13527": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 575
      },
      "13528": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 575
      },
      "13529": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 576
      },
      "13530": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 576
      },
      "13531": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 576
      },
      "13540": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 580
      },
      "13541": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 580
      },
      "13542": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 580
      },
      "13543": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 580
      },
      "13544": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 580
      },
      "13545": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 580
      },
      "13546": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 580
      },
      "13547": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 580
      },
      "13548": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 580
      },
      "13549": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 580
      },
      "13550": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 580
      },
      "13551": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 580
      },
      "13552": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 580
      },
      "13553": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 580
      },
      "13554": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 580
      },
      "13555": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 580
      },
      "13556": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 580
      },
      "13557": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 581
      },
      "13558": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 581
      },
      "13559": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 581
      },
      "13560": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 581
      },
      "13561": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 581
      },
      "13562": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 581
      },
      "13563": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 581
      },
      "13564": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 581
      },
      "13565": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 581
      },
      "13566": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 581
      },
      "13567": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 581
      },
      "13568": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 581
      },
      "13569": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 581
      },
      "13570": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 581
      },
      "13571": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 581
      },
      "13572": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 581
      },
      "13573": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 581
      },
      "13574": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 581
      },
      "13575": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 581
      },
      "13576": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 581
      },
      "13577": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 581
      },
      "13578": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 581
      },
      "13579": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 581
      },
      "13580": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 581
      },
      "13581": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 581
      },
      "13582": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 581
      },
      "13583": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 581
      },
      "13584": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 581
      },
      "13585": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 581
      },
      "13586": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 581
      },
      "13587": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 581
      },
      "13588": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 581
      },
      "13589": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 581
      },
      "13590": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 581
      },
      "13591": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 581
      },
      "13592": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 581
      },
      "13593": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 581
      },
      "13594": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 581
      },
      "13595": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 581
      },
      "13596": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 581
      },
      "13597": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 581
      },
      "13598": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 581
      },
      "13599": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 581
      },
      "13600": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 581
      },
      "13601": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 581
      },
      "13602": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 581
      },
      "13603": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 581
      },
      "13604": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 581
      },
      "13605": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 581
      },
      "13606": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 581
      },
      "13607": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 581
      },
      "13608": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 581
      },
      "13609": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 581
      },
      "13610": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 581
      },
      "13611": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 581
      },
      "13612": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 582
      },
      "13613": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 583
      },
      "13614": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 583
      },
      "13615": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 583
      },
      "13616": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 583
      },
      "13617": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 583
      },
      "13618": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 583
      },
      "13619": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 583
      },
      "13620": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 583
      },
      "13621": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 583
      },
      "13622": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 583
      },
      "13623": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 583
      },
      "13624": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 583
      },
      "13625": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 583
      },
      "13626": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 583
      },
      "13627": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 583
      },
      "13628": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 583
      },
      "13629": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 583
      },
      "13850": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 588
      },
      "13851": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 588
      },
      "13852": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 588
      },
      "13853": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 588
      },
      "13854": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 588
      },
      "13855": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 588
      },
      "13856": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 588
      },
      "13857": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 588
      },
      "13858": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 588
      },
      "13859": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 588
      },
      "13860": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 588
      },
      "13861": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 588
      },
      "13862": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 588
      },
      "13863": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 588
      },
      "13864": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 588
      },
      "13865": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 588
      },
      "13866": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 588
      },
      "13867": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 588
      },
      "13868": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 588
      },
      "13869": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 588
      },
      "13870": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 588
      },
      "13871": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 588
      },
      "13872": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 588
      },
      "13873": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 588
      },
      "13874": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 588
      },
      "13875": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 588
      },
      "13876": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 588
      },
      "13877": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 588
      },
      "13878": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 588
      },
      "13879": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 588
      },
      "13880": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 588
      },
      "13881": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 588
      },
      "13882": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 588
      },
      "13883": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 588
      },
      "13884": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 588
      },
      "13885": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 588
      },
      "13886": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 588
      },
      "13887": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 588
      },
      "13888": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 588
      },
      "13889": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 588
      },
      "13890": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 588
      },
      "13891": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 588
      },
      "13892": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 588
      },
      "13893": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 588
      },
      "13894": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 588
      },
      "13895": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 588
      },
      "13896": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 588
      },
      "13897": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 588
      },
      "13898": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 588
      },
      "13899": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 588
      },
      "13900": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 588
      },
      "13901": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 588
      },
      "13902": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 588
      },
      "13903": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 588
      },
      "13904": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 588
      },
      "13905": {
        "lookahead": {
          "type": "Token",
          "name": "IMPORT"
        },
        "like": 589
      },
      "13906": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 589
      },
      "13907": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 589
      },
      "13908": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 589
      },
      "13909": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 589
      },
      "13910": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 589
      },
      "13911": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 589
      },
      "13912": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 589
      },
      "13913": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 589
      },
      "13914": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 589
      },
      "13915": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 589
      },
      "13916": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 589
      },
      "13917": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 589
      },
      "13918": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 589
      },
      "13919": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 589
      },
      "13920": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 589
      },
      "13921": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 589
      },
      "13922": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 589
      },
      "13923": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 589
      },
      "13924": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 589
      },
      "13925": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 589
      },
      "13926": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 589
      },
      "13927": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 589
      },
      "13928": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 589
      },
      "13929": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 589
      },
      "13941": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 591
      },
      "13942": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 591
      },
      "13943": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 591
      },
      "13944": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 591
      },
      "13945": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 591
      },
      "13946": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 591
      },
      "13947": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 591
      },
      "13948": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 591
      },
      "13949": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 591
      },
      "13950": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 591
      },
      "13951": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 591
      },
      "13952": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 591
      },
      "13953": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 591
      },
      "13954": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 591
      },
      "13955": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 591
      },
      "13956": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 591
      },
      "13957": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 591
      },
      "13958": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 592
      },
      "13959": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 592
      },
      "13960": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 592
      },
      "13961": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 592
      },
      "13962": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 592
      },
      "13963": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 592
      },
      "13964": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 592
      },
      "13965": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 592
      },
      "13966": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 592
      },
      "13967": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 592
      },
      "13968": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 592
      },
      "13969": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 592
      },
      "13970": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 592
      },
      "13971": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 592
      },
      "13972": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 592
      },
      "13973": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 592
      },
      "13974": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 592
      },
      "13989": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 596
      },
      "13990": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 596
      },
      "13991": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 596
      },
      "13992": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 596
      },
      "13993": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 596
      },
      "13994": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 596
      },
      "13995": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 596
      },
      "13996": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 596
      },
      "13997": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 596
      },
      "13998": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 596
      },
      "14009": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 598
      },
      "14012": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 601
      },
      "14013": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 602
      },
      "14028": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 605
      },
      "14030": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 607
      },
      "14031": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 607
      },
      "14032": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 607
      },
      "14033": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 607
      },
      "14034": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 608
      },
      "14035": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 608
      },
      "14036": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 608
      },
      "14037": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 608
      },
      "14079": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 611
      },
      "14080": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 611
      },
      "14081": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 611
      },
      "14082": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 611
      },
      "14083": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 611
      },
      "14084": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 611
      },
      "14085": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 611
      },
      "14086": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 611
      },
      "14087": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 611
      },
      "14088": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 611
      },
      "14089": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 611
      },
      "14090": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 611
      },
      "14091": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 611
      },
      "14092": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 611
      },
      "14093": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 611
      },
      "14094": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 611
      },
      "14095": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 611
      },
      "14096": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 611
      },
      "14097": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 611
      },
      "14098": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 611
      },
      "14099": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 611
      },
      "14100": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 611
      },
      "14101": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 611
      },
      "14102": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 611
      },
      "14103": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 611
      },
      "14104": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 611
      },
      "14105": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 612
      },
      "14106": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 612
      },
      "14107": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 612
      },
      "14108": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 612
      },
      "14109": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 612
      },
      "14110": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 612
      },
      "14111": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 612
      },
      "14112": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 612
      },
      "14113": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 612
      },
      "14114": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 612
      },
      "14115": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 612
      },
      "14116": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 612
      },
      "14117": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 612
      },
      "14118": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 612
      },
      "14119": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 612
      },
      "14120": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 612
      },
      "14121": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 612
      },
      "14122": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 612
      },
      "14123": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 612
      },
      "14124": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 612
      },
      "14125": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 612
      },
      "14126": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 612
      },
      "14127": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 612
      },
      "14128": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 612
      },
      "14129": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 612
      },
      "14130": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 612
      },
      "14157": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 614
      },
      "14213": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 616
      },
      "14220": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 618
      },
      "14252": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 620
      },
      "14253": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 621
      },
      "14286": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 624
      },
      "14287": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 624
      },
      "14288": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 624
      },
      "14289": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 624
      },
      "14290": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 624
      },
      "14291": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 624
      },
      "14292": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 624
      },
      "14293": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 624
      },
      "14294": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 624
      },
      "14295": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 624
      },
      "14296": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 624
      },
      "14297": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 624
      },
      "14298": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 624
      },
      "14299": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 624
      },
      "14300": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 624
      },
      "14301": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 624
      },
      "14302": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 624
      },
      "14303": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 624
      },
      "14304": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 624
      },
      "14305": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 624
      },
      "14306": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 624
      },
      "14307": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 624
      },
      "14308": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 624
      },
      "14309": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 624
      },
      "14310": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 624
      },
      "14311": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 624
      },
      "14312": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 624
      },
      "14313": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 624
      },
      "14314": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 624
      },
      "14315": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 624
      },
      "14316": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 624
      },
      "14403": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 627
      },
      "14404": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 627
      },
      "14405": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 627
      },
      "14406": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 627
      },
      "14407": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 627
      },
      "14408": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 627
      },
      "14409": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 627
      },
      "14410": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 627
      },
      "14411": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 627
      },
      "14412": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 627
      },
      "14413": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 627
      },
      "14414": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 627
      },
      "14415": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 627
      },
      "14416": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 627
      },
      "14417": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 627
      },
      "14418": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 627
      },
      "14419": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 627
      },
      "14420": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 628
      },
      "14421": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 628
      },
      "14422": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 628
      },
      "14423": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 628
      },
      "14424": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 628
      },
      "14425": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 628
      },
      "14426": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 628
      },
      "14427": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 628
      },
      "14428": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 628
      },
      "14429": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 628
      },
      "14430": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 628
      },
      "14431": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 628
      },
      "14432": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 628
      },
      "14433": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 628
      },
      "14434": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 628
      },
      "14435": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 628
      },
      "14436": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 628
      },
      "14437": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 628
      },
      "14438": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 628
      },
      "14439": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 628
      },
      "14440": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 628
      },
      "14441": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 628
      },
      "14442": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 628
      },
      "14443": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 628
      },
      "14444": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 628
      },
      "14445": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 628
      },
      "14446": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 628
      },
      "14447": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 628
      },
      "14448": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 628
      },
      "14449": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 628
      },
      "14450": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 628
      },
      "14451": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 628
      },
      "14452": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 628
      },
      "14453": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 628
      },
      "14454": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 628
      },
      "14455": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 628
      },
      "14456": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 628
      },
      "14457": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 628
      },
      "14458": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 628
      },
      "14459": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 628
      },
      "14460": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 628
      },
      "14461": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 628
      },
      "14462": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 628
      },
      "14463": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 628
      },
      "14464": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 628
      },
      "14465": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 628
      },
      "14466": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 628
      },
      "14467": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 628
      },
      "14468": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 628
      },
      "14469": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 628
      },
      "14470": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 628
      },
      "14471": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 628
      },
      "14472": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 628
      },
      "14473": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 628
      },
      "14474": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 628
      },
      "14475": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 629
      },
      "14476": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 630
      },
      "14477": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 630
      },
      "14478": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 630
      },
      "14479": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 630
      },
      "14480": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 630
      },
      "14481": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 630
      },
      "14482": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 630
      },
      "14483": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 630
      },
      "14484": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 630
      },
      "14485": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 630
      },
      "14486": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 630
      },
      "14487": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 630
      },
      "14488": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 630
      },
      "14489": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 630
      },
      "14490": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 630
      },
      "14491": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 630
      },
      "14492": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 630
      },
      "14494": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 632
      },
      "14495": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 632
      },
      "14496": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 632
      },
      "14497": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 632
      },
      "14498": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 632
      },
      "14499": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 632
      },
      "14500": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 632
      },
      "14501": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 632
      },
      "14502": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 632
      },
      "14503": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 632
      },
      "14504": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 632
      },
      "14505": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 632
      },
      "14506": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 632
      },
      "14507": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 632
      },
      "14508": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 632
      },
      "14509": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 632
      },
      "14510": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 632
      },
      "14511": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 632
      },
      "14512": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 632
      },
      "14513": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 632
      },
      "14514": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 632
      },
      "14515": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 632
      },
      "14516": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 632
      },
      "14517": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 632
      },
      "14518": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 632
      },
      "14519": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 632
      },
      "14520": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 632
      },
      "14521": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 632
      },
      "14522": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 632
      },
      "14523": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 632
      },
      "14524": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 632
      },
      "14525": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 632
      },
      "14526": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 632
      },
      "14527": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 632
      },
      "14528": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 632
      },
      "14529": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 632
      },
      "14530": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 632
      },
      "14531": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 632
      },
      "14532": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 632
      },
      "14533": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 632
      },
      "14534": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 632
      },
      "14535": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 632
      },
      "14536": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 632
      },
      "14537": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 632
      },
      "14538": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 632
      },
      "14539": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 632
      },
      "14540": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 632
      },
      "14541": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 632
      },
      "14542": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 632
      },
      "14543": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 632
      },
      "14544": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 632
      },
      "14545": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 632
      },
      "14546": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 632
      },
      "14547": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 632
      },
      "14548": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 632
      },
      "14626": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 638
      },
      "14627": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 638
      },
      "14628": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 639
      },
      "14629": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 639
      },
      "14795": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 643
      },
      "14797": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 644
      },
      "14798": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 645
      },
      "14799": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 645
      },
      "14800": {
        "lookahead": {
          "type": "Token",
          "name": "THICKARROW"
        },
        "like": 645
      },
      "14801": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 646
      },
      "14802": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 647
      },
      "14806": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 650
      },
      "14807": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 651
      },
      "14808": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 652
      },
      "14809": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 653
      },
      "14810": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 654
      },
      "14811": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 654
      },
      "14814": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 656
      },
      "14815": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 656
      },
      "14816": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 656
      },
      "14817": {
        "lookahead": {
          "type": "Token",
          "name": "COLONCOLON"
        },
        "like": 656
      },
      "14931": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 661
      },
      "14987": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 663
      },
      "14989": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 665
      },
      "14990": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 666
      },
      "14991": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 667
      },
      "15047": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 669
      },
      "15048": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 669
      },
      "15049": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 669
      },
      "15050": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 669
      },
      "15051": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 669
      },
      "15052": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 669
      },
      "15053": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 669
      },
      "15054": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 669
      },
      "15055": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 669
      },
      "15056": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 669
      },
      "15057": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 669
      },
      "15072": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 673
      },
      "15073": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 673
      },
      "15074": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 673
      },
      "15075": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 673
      },
      "15076": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 673
      },
      "15077": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 673
      },
      "15078": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 673
      },
      "15079": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 673
      },
      "15080": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 673
      },
      "15081": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 673
      },
      "15082": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 674
      },
      "15083": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 675
      },
      "15084": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 676
      },
      "15085": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 677
      },
      "15086": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 677
      },
      "15087": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 677
      },
      "15088": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 677
      },
      "15099": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 679
      },
      "15101": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 680
      },
      "15102": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 680
      },
      "15103": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 680
      },
      "15104": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 680
      },
      "15119": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 683
      },
      "15120": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 683
      },
      "15121": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 683
      },
      "15122": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 683
      },
      "15123": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 684
      },
      "15124": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 684
      },
      "15125": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 684
      },
      "15126": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 684
      },
      "15158": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 686
      },
      "15159": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 686
      },
      "15160": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 686
      },
      "15161": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 686
      },
      "15162": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 686
      },
      "15163": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 686
      },
      "15164": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 686
      },
      "15165": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 686
      },
      "15166": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 686
      },
      "15167": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 686
      },
      "15168": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 686
      },
      "15169": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 686
      },
      "15170": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 686
      },
      "15171": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 686
      },
      "15172": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 686
      },
      "15173": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 686
      },
      "15174": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 686
      },
      "15175": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 686
      },
      "15176": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 686
      },
      "15177": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 686
      },
      "15178": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 686
      },
      "15179": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 686
      },
      "15180": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 686
      },
      "15181": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 686
      },
      "15182": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 686
      },
      "15183": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 686
      },
      "15184": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 687
      },
      "15240": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 689
      },
      "15241": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 689
      },
      "15242": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 689
      },
      "15243": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 689
      },
      "15244": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 689
      },
      "15245": {
        "lookahead": {
          "type": "Token",
          "name": "DERIVING"
        },
        "like": 689
      },
      "15294": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 692
      },
      "15296": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 694
      },
      "15297": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 694
      },
      "15298": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 694
      },
      "15299": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 694
      },
      "15300": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 694
      },
      "15301": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 694
      },
      "15302": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 694
      },
      "15303": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 694
      },
      "15304": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 694
      },
      "15305": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 694
      },
      "15306": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 694
      },
      "15307": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 694
      },
      "15308": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 694
      },
      "15309": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 694
      },
      "15310": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 694
      },
      "15311": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 694
      },
      "15312": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 694
      },
      "15313": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 695
      },
      "15314": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 695
      },
      "15315": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 695
      },
      "15316": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 695
      },
      "15317": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 695
      },
      "15318": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 695
      },
      "15319": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 695
      },
      "15320": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 695
      },
      "15321": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 695
      },
      "15322": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 695
      },
      "15323": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 695
      },
      "15324": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 695
      },
      "15325": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 695
      },
      "15326": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 695
      },
      "15327": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 695
      },
      "15328": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 695
      },
      "15329": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 695
      },
      "15369": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 699
      },
      "15370": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 699
      },
      "15371": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 699
      },
      "15372": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 699
      },
      "15373": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 700
      },
      "15374": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 700
      },
      "15375": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 700
      },
      "15376": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 700
      },
      "15377": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 701
      },
      "15378": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 701
      },
      "15379": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 701
      },
      "15380": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 701
      },
      "15381": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 701
      },
      "15382": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 701
      },
      "15383": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 701
      },
      "15384": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 701
      },
      "15385": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 701
      },
      "15386": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 701
      },
      "15387": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 701
      },
      "15388": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 701
      },
      "15389": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 701
      },
      "15390": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 701
      },
      "15391": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 701
      },
      "15392": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 701
      },
      "15393": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 701
      },
      "15394": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 701
      },
      "15395": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 701
      },
      "15396": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 701
      },
      "15397": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 701
      },
      "15398": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 701
      },
      "15399": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 701
      },
      "15400": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 701
      },
      "15401": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 701
      },
      "15402": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 701
      },
      "15403": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 701
      },
      "15404": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 701
      },
      "15405": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 701
      },
      "15406": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 701
      },
      "15407": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 701
      },
      "15408": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 702
      },
      "15409": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 702
      },
      "15410": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 702
      },
      "15411": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 702
      },
      "15412": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 702
      },
      "15413": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 702
      },
      "15414": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 702
      },
      "15415": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 702
      },
      "15416": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 702
      },
      "15417": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 702
      },
      "15418": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 702
      },
      "15419": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 702
      },
      "15420": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 702
      },
      "15421": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 702
      },
      "15422": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 702
      },
      "15423": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 702
      },
      "15424": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 702
      },
      "15425": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 702
      },
      "15426": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 702
      },
      "15427": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 702
      },
      "15428": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 702
      },
      "15429": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 702
      },
      "15430": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 702
      },
      "15431": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 702
      },
      "15432": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 702
      },
      "15433": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 702
      },
      "15434": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 702
      },
      "15435": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 702
      },
      "15436": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 702
      },
      "15437": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 702
      },
      "15438": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 702
      },
      "15439": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 702
      },
      "15440": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 702
      },
      "15441": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 702
      },
      "15442": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 702
      },
      "15443": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 702
      },
      "15444": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 702
      },
      "15445": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 702
      },
      "15446": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 702
      },
      "15447": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 702
      },
      "15448": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 702
      },
      "15449": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 702
      },
      "15450": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 702
      },
      "15451": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 702
      },
      "15452": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 702
      },
      "15453": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 702
      },
      "15454": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 702
      },
      "15455": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 702
      },
      "15456": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 702
      },
      "15457": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 702
      },
      "15458": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 702
      },
      "15459": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 702
      },
      "15460": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 702
      },
      "15461": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 702
      },
      "15462": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 702
      },
      "15463": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 703
      },
      "15464": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 704
      },
      "15465": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 704
      },
      "15466": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 704
      },
      "15467": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 704
      },
      "15468": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 704
      },
      "15469": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 704
      },
      "15470": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 704
      },
      "15471": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 704
      },
      "15472": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 704
      },
      "15473": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 704
      },
      "15474": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 704
      },
      "15475": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 704
      },
      "15476": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 704
      },
      "15477": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 704
      },
      "15478": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 704
      },
      "15479": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 704
      },
      "15480": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 704
      },
      "15481": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 704
      },
      "15482": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 704
      },
      "15483": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 704
      },
      "15484": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 704
      },
      "15485": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 704
      },
      "15486": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 704
      },
      "15487": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 704
      },
      "15488": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 704
      },
      "15489": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 704
      },
      "15490": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 704
      },
      "15491": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 704
      },
      "15492": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 704
      },
      "15493": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 704
      },
      "15494": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 704
      },
      "15495": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 704
      },
      "15496": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 704
      },
      "15497": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 704
      },
      "15498": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 704
      },
      "15499": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 704
      },
      "15500": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 704
      },
      "15501": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 704
      },
      "15502": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 704
      },
      "15503": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 704
      },
      "15504": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 704
      },
      "15505": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 704
      },
      "15506": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 704
      },
      "15507": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 704
      },
      "15508": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 704
      },
      "15509": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 704
      },
      "15510": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 704
      },
      "15511": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 704
      },
      "15512": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 704
      },
      "15513": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 704
      },
      "15514": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 704
      },
      "15515": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 704
      },
      "15516": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 704
      },
      "15517": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 704
      },
      "15518": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 704
      },
      "15533": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 707
      },
      "15534": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 707
      },
      "15535": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 707
      },
      "15536": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 707
      },
      "15537": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 707
      },
      "15538": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 707
      },
      "15539": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 708
      },
      "15540": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 708
      },
      "15541": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 709
      },
      "15542": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 709
      },
      "15543": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 710
      },
      "15544": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 710
      },
      "15545": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 710
      },
      "15546": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 710
      },
      "15547": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 710
      },
      "15548": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 710
      },
      "15549": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 710
      },
      "15550": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 710
      },
      "15551": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 710
      },
      "15552": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 710
      },
      "15553": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 710
      },
      "15554": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 710
      },
      "15555": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 710
      },
      "15556": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 710
      },
      "15557": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 710
      },
      "15558": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 710
      },
      "15559": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 710
      },
      "15560": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 710
      },
      "15561": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 710
      },
      "15562": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 710
      },
      "15563": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 710
      },
      "15564": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 710
      },
      "15565": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 710
      },
      "15566": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 710
      },
      "15567": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 710
      },
      "15568": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 710
      },
      "15569": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 710
      },
      "15570": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 710
      },
      "15571": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 710
      },
      "15572": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 710
      },
      "15573": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 710
      },
      "15574": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 710
      },
      "15575": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 710
      },
      "15576": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 710
      },
      "15577": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 710
      },
      "15578": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 710
      },
      "15579": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 710
      },
      "15580": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 710
      },
      "15581": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 710
      },
      "15582": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 710
      },
      "15583": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 710
      },
      "15584": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 710
      },
      "15585": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 710
      },
      "15586": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 710
      },
      "15587": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 710
      },
      "15588": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 710
      },
      "15589": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 710
      },
      "15590": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 710
      },
      "15591": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 710
      },
      "15592": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 710
      },
      "15593": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 710
      },
      "15594": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 710
      },
      "15595": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 710
      },
      "15596": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 710
      },
      "15597": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 710
      },
      "15598": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 711
      },
      "15599": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 711
      },
      "15600": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 711
      },
      "15601": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 711
      },
      "15602": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 711
      },
      "15603": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 711
      },
      "15604": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 711
      },
      "15605": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 711
      },
      "15606": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 711
      },
      "15607": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 711
      },
      "15608": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 711
      },
      "15609": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 711
      },
      "15610": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 711
      },
      "15611": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 711
      },
      "15612": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 711
      },
      "15613": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 711
      },
      "15614": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 711
      },
      "15615": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 711
      },
      "15616": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 711
      },
      "15617": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 711
      },
      "15618": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 711
      },
      "15619": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 711
      },
      "15620": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 711
      },
      "15621": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 711
      },
      "15622": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 711
      },
      "15623": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 711
      },
      "15624": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 711
      },
      "15625": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 711
      },
      "15626": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 711
      },
      "15627": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 711
      },
      "15628": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 711
      },
      "15629": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 711
      },
      "15630": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 711
      },
      "15631": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 711
      },
      "15632": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 711
      },
      "15633": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 711
      },
      "15634": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 711
      },
      "15635": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 711
      },
      "15636": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 711
      },
      "15637": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 711
      },
      "15638": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 711
      },
      "15639": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 711
      },
      "15640": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 711
      },
      "15641": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 711
      },
      "15642": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 711
      },
      "15643": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 711
      },
      "15644": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 711
      },
      "15645": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 711
      },
      "15646": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 711
      },
      "15647": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 711
      },
      "15648": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 711
      },
      "15649": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 711
      },
      "15650": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 711
      },
      "15651": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 711
      },
      "15652": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 711
      },
      "15708": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 713
      },
      "15768": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 717
      },
      "15769": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 717
      },
      "15770": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 717
      },
      "15771": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 718
      },
      "15772": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 718
      },
      "15773": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 718
      },
      "15774": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 718
      },
      "15836": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 722
      },
      "15892": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 724
      },
      "15893": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 725
      },
      "15949": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 727
      },
      "15950": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 727
      },
      "15951": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 727
      },
      "15952": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 727
      },
      "15953": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 727
      },
      "15954": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 727
      },
      "15955": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 727
      },
      "15956": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 727
      },
      "15957": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 727
      },
      "15958": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 727
      },
      "15959": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 728
      },
      "15960": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 728
      },
      "15962": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 729
      },
      "15963": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 729
      },
      "15974": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 731
      },
      "15975": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 731
      },
      "15976": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 731
      },
      "15977": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 731
      },
      "15989": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 733
      },
      "15990": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 733
      },
      "15991": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 733
      },
      "15992": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 733
      },
      "16024": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 735
      },
      "16025": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 735
      },
      "16026": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 736
      },
      "16027": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 736
      },
      "16028": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 737
      },
      "16029": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 737
      },
      "16090": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 740
      },
      "16091": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 740
      },
      "16092": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 740
      },
      "16093": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 740
      },
      "16094": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 740
      },
      "16126": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 742
      },
      "16127": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 742
      },
      "16128": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 742
      },
      "16129": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 742
      },
      "16130": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 742
      },
      "16131": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 743
      },
      "16132": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 743
      },
      "16133": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 743
      },
      "16134": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 743
      },
      "16135": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 743
      },
      "16136": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 744
      },
      "16137": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 744
      },
      "16138": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 744
      },
      "16139": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 744
      },
      "16140": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 744
      },
      "16141": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 744
      },
      "16142": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 744
      },
      "16143": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 744
      },
      "16144": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 744
      },
      "16145": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 744
      },
      "16146": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 744
      },
      "16147": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 744
      },
      "16148": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 744
      },
      "16149": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 744
      },
      "16150": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 744
      },
      "16151": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 744
      },
      "16152": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 744
      },
      "16153": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 745
      },
      "16154": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 746
      },
      "16155": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 746
      },
      "16156": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 746
      },
      "16157": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 746
      },
      "16158": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 746
      },
      "16159": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 746
      },
      "16160": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 746
      },
      "16161": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 746
      },
      "16162": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 746
      },
      "16163": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 746
      },
      "16164": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 746
      },
      "16165": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 746
      },
      "16166": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 746
      },
      "16167": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 746
      },
      "16168": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 746
      },
      "16169": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 746
      },
      "16170": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 746
      },
      "16171": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 747
      },
      "16172": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 747
      },
      "16173": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 747
      },
      "16174": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 747
      },
      "16175": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 748
      },
      "16176": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 748
      },
      "16177": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 748
      },
      "16178": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 748
      },
      "16190": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 751
      },
      "16191": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 751
      },
      "16192": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 751
      },
      "16193": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 751
      },
      "16237": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 756
      },
      "16238": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 756
      },
      "16239": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 756
      },
      "16240": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 757
      },
      "16241": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 757
      },
      "16242": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 757
      },
      "16243": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 757
      },
      "16244": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 758
      },
      "16245": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 758
      },
      "16246": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 758
      },
      "16247": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 758
      },
      "16248": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 758
      },
      "16249": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 758
      },
      "16250": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 758
      },
      "16258": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 760
      },
      "16259": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 760
      },
      "16260": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 760
      },
      "16261": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 760
      },
      "16262": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 760
      },
      "16263": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 760
      },
      "16264": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 761
      },
      "16265": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 761
      },
      "16266": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 761
      },
      "16267": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 761
      },
      "16268": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 761
      },
      "16269": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 761
      },
      "16270": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 762
      },
      "16271": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 762
      },
      "16272": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 762
      },
      "16273": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 762
      },
      "16274": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 762
      },
      "16275": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 762
      },
      "16331": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 764
      },
      "16332": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 764
      },
      "16390": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 767
      },
      "16391": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 767
      },
      "16392": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 768
      },
      "16393": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 768
      },
      "16394": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 769
      },
      "16395": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 769
      },
      "16396": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 769
      },
      "16456": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 772
      },
      "16457": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 772
      },
      "16568": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 775
      },
      "16569": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 775
      },
      "16570": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 775
      },
      "16571": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 775
      },
      "16572": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 775
      },
      "16573": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 775
      },
      "16574": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 775
      },
      "16575": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 775
      },
      "16576": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 775
      },
      "16577": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 775
      },
      "16578": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 776
      },
      "16579": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 776
      },
      "16580": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 776
      },
      "16581": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 776
      },
      "16582": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 776
      },
      "16583": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 776
      },
      "16584": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 776
      },
      "16585": {
        "lookahead": {
          "type": "Token",
          "name": "THINARROW"
        },
        "like": 776
      },
      "16586": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 776
      },
      "16587": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 776
      },
      "16588": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 777
      },
      "16589": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 777
      },
      "16590": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 777
      },
      "16591": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 777
      },
      "16592": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 777
      },
      "16593": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 777
      },
      "16594": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 777
      },
      "16595": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 777
      },
      "16596": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 777
      },
      "16597": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 777
      },
      "16598": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 777
      },
      "16599": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 777
      },
      "16600": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 777
      },
      "16601": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 777
      },
      "16602": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 777
      },
      "16603": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 777
      },
      "16604": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 777
      },
      "16605": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 777
      },
      "16606": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 777
      },
      "16607": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 777
      },
      "16608": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 777
      },
      "16609": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 777
      },
      "16610": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 777
      },
      "16611": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 777
      },
      "16612": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 777
      },
      "16613": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 777
      },
      "16614": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 777
      },
      "16615": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 777
      },
      "16616": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 777
      },
      "16617": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 777
      },
      "16618": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 777
      },
      "16619": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 778
      },
      "16620": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 778
      },
      "16676": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 780
      },
      "16677": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 780
      },
      "16678": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 780
      },
      "16679": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 780
      },
      "16680": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 780
      },
      "16681": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 781
      },
      "16682": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 781
      },
      "16683": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 781
      },
      "16684": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 781
      },
      "16685": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 781
      },
      "16686": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 782
      },
      "16687": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 782
      },
      "16688": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 782
      },
      "16689": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 782
      },
      "16690": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 782
      },
      "16691": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 783
      },
      "16692": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 783
      },
      "16693": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 783
      },
      "16694": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 783
      },
      "16695": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 783
      },
      "16732": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 786
      },
      "16733": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 786
      },
      "16734": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 786
      },
      "16735": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 786
      },
      "16736": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 787
      },
      "16737": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 787
      },
      "16738": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 787
      },
      "16739": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 787
      },
      "16740": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 787
      },
      "16751": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 790
      },
      "16752": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 790
      },
      "16753": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 790
      },
      "16754": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 790
      },
      "16762": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 792
      },
      "16767": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 795
      },
      "16768": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 795
      },
      "16769": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 795
      },
      "16770": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 796
      },
      "16771": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 796
      },
      "16772": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 796
      },
      "16775": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 798
      },
      "16776": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 799
      },
      "16777": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 800
      },
      "16778": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 801
      },
      "16779": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 802
      },
      "16823": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 807
      },
      "16824": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 807
      },
      "16825": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 807
      },
      "16890": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 811
      },
      "16891": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 811
      },
      "16892": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 811
      },
      "16893": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 811
      },
      "16894": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 811
      },
      "16895": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 811
      },
      "16896": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 811
      },
      "16897": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 811
      },
      "16898": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 811
      },
      "16899": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 811
      },
      "16900": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 811
      },
      "16901": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 811
      },
      "16902": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 811
      },
      "16903": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 811
      },
      "16904": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 811
      },
      "16905": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 811
      },
      "16906": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 811
      },
      "16907": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 811
      },
      "16908": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 811
      },
      "16909": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 811
      },
      "16910": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 811
      },
      "16911": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 811
      },
      "16912": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 811
      },
      "16913": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 811
      },
      "16914": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 811
      },
      "16915": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 811
      },
      "16916": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 811
      },
      "16917": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 811
      },
      "16918": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 811
      },
      "16919": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 811
      },
      "16920": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 811
      },
      "16921": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 811
      },
      "16922": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 811
      },
      "16923": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 811
      },
      "16924": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 811
      },
      "16925": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 811
      },
      "16926": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 811
      },
      "16927": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 811
      },
      "16928": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 811
      },
      "16929": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 811
      },
      "16930": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 811
      },
      "16931": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 811
      },
      "16932": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 811
      },
      "16933": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 811
      },
      "16934": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 811
      },
      "16935": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 811
      },
      "16936": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 811
      },
      "16937": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 811
      },
      "16938": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 811
      },
      "16939": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 811
      },
      "16940": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 811
      },
      "16941": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 811
      },
      "16942": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 811
      },
      "16943": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 811
      },
      "16944": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 811
      },
      "16945": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 812
      },
      "16946": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 812
      },
      "16947": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 813
      },
      "16948": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 813
      },
      "16949": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 813
      },
      "16950": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 813
      },
      "17009": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 816
      },
      "17010": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 816
      },
      "17011": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 816
      },
      "17012": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 817
      },
      "17013": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 817
      },
      "17014": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 817
      },
      "17180": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 821
      },
      "17181": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 821
      },
      "17182": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 821
      },
      "17183": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 821
      },
      "17184": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 821
      },
      "17185": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 822
      },
      "17186": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 822
      },
      "17187": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 822
      },
      "17188": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 822
      },
      "17189": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 822
      },
      "17221": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 824
      },
      "17222": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 824
      },
      "17223": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 824
      },
      "17224": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 825
      },
      "17225": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 825
      },
      "17226": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 825
      },
      "17230": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 827
      },
      "17231": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 827
      },
      "17232": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 827
      },
      "17233": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 827
      },
      "17239": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 829
      },
      "17240": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 829
      },
      "17241": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 829
      },
      "17242": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 829
      },
      "17243": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 829
      },
      "17244": {
        "lookahead": {
          "type": "Token",
          "name": "WITHCONSTRUCTOR"
        },
        "like": 830
      },
      "17245": {
        "lookahead": {
          "type": "Token",
          "name": "WITH"
        },
        "like": 830
      },
      "17246": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 830
      },
      "17247": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 830
      },
      "17248": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 830
      },
      "17249": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 830
      },
      "17250": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 830
      },
      "17251": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 831
      },
      "17252": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 832
      },
      "17253": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 832
      },
      "17254": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 832
      },
      "17255": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 833
      },
      "17256": {
        "lookahead": {
          "type": "Token",
          "name": "MUTABLE"
        },
        "like": 833
      },
      "17257": {
        "lookahead": {
          "type": "Token",
          "name": "CYCLIC"
        },
        "like": 833
      },
      "17258": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 834
      },
      "17259": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 834
      },
      "17268": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 837
      },
      "17269": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 837
      },
      "17270": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 837
      },
      "17271": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 837
      },
      "17272": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 838
      },
      "17273": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 838
      },
      "17274": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 838
      },
      "17275": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 838
      },
      "17276": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 838
      },
      "17277": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 838
      },
      "17278": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 838
      },
      "17279": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 838
      },
      "17280": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 838
      },
      "17281": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 838
      },
      "17282": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 838
      },
      "17283": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 838
      },
      "17284": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 838
      },
      "17285": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 838
      },
      "17286": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 838
      },
      "17287": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 838
      },
      "17288": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 838
      },
      "17289": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 838
      },
      "17290": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 838
      },
      "17291": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 838
      },
      "17292": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 838
      },
      "17293": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 838
      },
      "17294": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 838
      },
      "17295": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 838
      },
      "17296": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 838
      },
      "17297": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 838
      },
      "17298": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 838
      },
      "17299": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 838
      },
      "17300": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 838
      },
      "17301": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 838
      },
      "17302": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 838
      },
      "17310": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 840
      },
      "17311": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 840
      },
      "17312": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 840
      },
      "17313": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 840
      },
      "17314": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 840
      },
      "17315": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 840
      },
      "17316": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 840
      },
      "17317": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 840
      },
      "17318": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 840
      },
      "17319": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 840
      },
      "17320": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 840
      },
      "17321": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 840
      },
      "17322": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 840
      },
      "17323": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 840
      },
      "17324": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 840
      },
      "17325": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 840
      },
      "17326": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 840
      },
      "17327": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 840
      },
      "17328": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 840
      },
      "17329": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 840
      },
      "17330": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 840
      },
      "17331": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 840
      },
      "17332": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 840
      },
      "17333": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 840
      },
      "17334": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 840
      },
      "17335": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 840
      },
      "17336": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 840
      },
      "17337": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 840
      },
      "17338": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 840
      },
      "17339": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 840
      },
      "17340": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 840
      },
      "17341": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 840
      },
      "17342": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 840
      },
      "17343": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 840
      },
      "17344": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 840
      },
      "17345": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 840
      },
      "17346": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 840
      },
      "17347": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 840
      },
      "17348": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 840
      },
      "17349": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 840
      },
      "17350": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 840
      },
      "17351": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 840
      },
      "17352": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 840
      },
      "17353": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 840
      },
      "17354": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 840
      },
      "17355": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 840
      },
      "17356": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 840
      },
      "17357": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 840
      },
      "17358": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 840
      },
      "17359": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 840
      },
      "17360": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 840
      },
      "17361": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 840
      },
      "17362": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 840
      },
      "17363": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 840
      },
      "17364": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 840
      },
      "17367": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 842
      },
      "17368": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 842
      },
      "17369": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 842
      },
      "17370": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 842
      },
      "17431": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 846
      },
      "17432": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 846
      },
      "17433": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 847
      },
      "17434": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 847
      },
      "17435": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 847
      },
      "17546": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 850
      },
      "17547": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 850
      },
      "17548": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 850
      },
      "17549": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 850
      },
      "17550": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 850
      },
      "17551": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 850
      },
      "17552": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 850
      },
      "17553": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 850
      },
      "17554": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 850
      },
      "17555": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 850
      },
      "17556": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 850
      },
      "17557": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 850
      },
      "17558": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 850
      },
      "17559": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 850
      },
      "17560": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 850
      },
      "17561": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 850
      },
      "17562": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 850
      },
      "17563": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 850
      },
      "17564": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 850
      },
      "17565": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 850
      },
      "17566": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 850
      },
      "17567": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 850
      },
      "17568": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 850
      },
      "17569": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 850
      },
      "17570": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 850
      },
      "17571": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 850
      },
      "17572": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 850
      },
      "17573": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 850
      },
      "17574": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 850
      },
      "17575": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 850
      },
      "17576": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 850
      },
      "17577": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 850
      },
      "17578": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 850
      },
      "17579": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 850
      },
      "17580": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 850
      },
      "17581": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 850
      },
      "17582": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 850
      },
      "17583": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 850
      },
      "17584": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 850
      },
      "17585": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 850
      },
      "17586": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 850
      },
      "17587": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 850
      },
      "17588": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 850
      },
      "17589": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 850
      },
      "17590": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 850
      },
      "17591": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 850
      },
      "17592": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 850
      },
      "17593": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 850
      },
      "17594": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 850
      },
      "17595": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 850
      },
      "17596": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 850
      },
      "17597": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 850
      },
      "17598": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 850
      },
      "17599": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 850
      },
      "17600": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 850
      },
      "17632": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 852
      },
      "17633": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 852
      },
      "17634": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 852
      },
      "17635": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 853
      },
      "17636": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 853
      },
      "17637": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 853
      },
      "17638": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 853
      },
      "17639": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 853
      },
      "17640": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 854
      },
      "17641": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 854
      },
      "17642": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 854
      },
      "17643": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 854
      },
      "17644": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 854
      },
      "17649": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 856
      },
      "17650": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 856
      },
      "17651": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 856
      },
      "17652": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 856
      },
      "17660": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 858
      },
      "17661": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 858
      },
      "17662": {
        "lookahead": {
          "type": "Token",
          "name": "THICKARROW"
        },
        "like": 859
      },
      "17666": {
        "lookahead": {
          "type": "Token",
          "name": "THICKARROW"
        },
        "like": 861
      },
      "17669": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 863
      },
      "17670": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 863
      },
      "17671": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 863
      },
      "17672": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 863
      },
      "17673": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 863
      },
      "17674": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 863
      },
      "17675": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 863
      },
      "17676": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 863
      },
      "17677": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 863
      },
      "17678": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 863
      },
      "17679": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 863
      },
      "17680": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 863
      },
      "17681": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 863
      },
      "17682": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 863
      },
      "17683": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 863
      },
      "17684": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 863
      },
      "17685": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 863
      },
      "17686": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 863
      },
      "17687": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 863
      },
      "17688": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 863
      },
      "17689": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 863
      },
      "17690": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 863
      },
      "17691": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 863
      },
      "17692": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 863
      },
      "17693": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 863
      },
      "17694": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 863
      },
      "17695": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 863
      },
      "17696": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 863
      },
      "17697": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 863
      },
      "17698": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 863
      },
      "17699": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 863
      },
      "17700": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 863
      },
      "17701": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 863
      },
      "17702": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 863
      },
      "17703": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 863
      },
      "17704": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 863
      },
      "17705": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 863
      },
      "17706": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 863
      },
      "17707": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 863
      },
      "17708": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 863
      },
      "17709": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 863
      },
      "17710": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 863
      },
      "17711": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 863
      },
      "17712": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 863
      },
      "17713": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 863
      },
      "17714": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 863
      },
      "17715": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 863
      },
      "17716": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 863
      },
      "17717": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 863
      },
      "17718": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 863
      },
      "17719": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 863
      },
      "17720": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 863
      },
      "17721": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 863
      },
      "17722": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 863
      },
      "17723": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 863
      },
      "17724": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 864
      },
      "17725": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 864
      },
      "17726": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 864
      },
      "17727": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 864
      },
      "17728": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 864
      },
      "17729": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 864
      },
      "17730": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 864
      },
      "17731": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 864
      },
      "17732": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 864
      },
      "17733": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 864
      },
      "17734": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 864
      },
      "17735": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 864
      },
      "17736": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 864
      },
      "17737": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 864
      },
      "17738": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 864
      },
      "17739": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 864
      },
      "17740": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 864
      },
      "17741": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 864
      },
      "17742": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 864
      },
      "17743": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 864
      },
      "17744": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 864
      },
      "17745": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 864
      },
      "17746": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 864
      },
      "17747": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 864
      },
      "17748": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 864
      },
      "17749": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 864
      },
      "17750": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 864
      },
      "17751": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 864
      },
      "17752": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 864
      },
      "17753": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 864
      },
      "17754": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 864
      },
      "17755": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 864
      },
      "17756": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 864
      },
      "17757": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 864
      },
      "17758": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 864
      },
      "17759": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 864
      },
      "17760": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 864
      },
      "17761": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 864
      },
      "17762": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 864
      },
      "17763": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 864
      },
      "17764": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 864
      },
      "17765": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 864
      },
      "17766": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 864
      },
      "17767": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 864
      },
      "17768": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 864
      },
      "17769": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 864
      },
      "17770": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 864
      },
      "17771": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 864
      },
      "17772": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 864
      },
      "17773": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 864
      },
      "17774": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 864
      },
      "17775": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 864
      },
      "17776": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 864
      },
      "17777": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 864
      },
      "17778": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 864
      },
      "17779": {
        "lookahead": {
          "type": "Token",
          "name": "PLUS"
        },
        "like": 865
      },
      "17780": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 865
      },
      "17781": {
        "lookahead": {
          "type": "Token",
          "name": "STAR"
        },
        "like": 865
      },
      "17782": {
        "lookahead": {
          "type": "Token",
          "name": "SLASH"
        },
        "like": 865
      },
      "17783": {
        "lookahead": {
          "type": "Token",
          "name": "LEQ"
        },
        "like": 865
      },
      "17784": {
        "lookahead": {
          "type": "Token",
          "name": "GEQ"
        },
        "like": 865
      },
      "17785": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALEQUAL"
        },
        "like": 865
      },
      "17786": {
        "lookahead": {
          "type": "Token",
          "name": "NEQ"
        },
        "like": 865
      },
      "17787": {
        "lookahead": {
          "type": "Token",
          "name": "LT"
        },
        "like": 865
      },
      "17788": {
        "lookahead": {
          "type": "Token",
          "name": "GT"
        },
        "like": 865
      },
      "17789": {
        "lookahead": {
          "type": "Token",
          "name": "AND"
        },
        "like": 865
      },
      "17790": {
        "lookahead": {
          "type": "Token",
          "name": "OR"
        },
        "like": 865
      },
      "17791": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 865
      },
      "17792": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 865
      },
      "17793": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 865
      },
      "17794": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 865
      },
      "17795": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 865
      },
      "17796": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 865
      },
      "17797": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 865
      },
      "17798": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 865
      },
      "17799": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 865
      },
      "17800": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 865
      },
      "17801": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 865
      },
      "17802": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 865
      },
      "17803": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 865
      },
      "17804": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 865
      },
      "17805": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 865
      },
      "17806": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 865
      },
      "17807": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 865
      },
      "17808": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 865
      },
      "17809": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 865
      },
      "17810": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 865
      },
      "17811": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 865
      },
      "17812": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 865
      },
      "17813": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 865
      },
      "17814": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 865
      },
      "17815": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 865
      },
      "17816": {
        "lookahead": {
          "type": "Token",
          "name": "PARENNOSPACE"
        },
        "like": 865
      },
      "17817": {
        "lookahead": {
          "type": "Token",
          "name": "CARET"
        },
        "like": 865
      },
      "17818": {
        "lookahead": {
          "type": "Token",
          "name": "DOT"
        },
        "like": 865
      },
      "17819": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 865
      },
      "17820": {
        "lookahead": {
          "type": "Token",
          "name": "BANG"
        },
        "like": 865
      },
      "17821": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 865
      },
      "17822": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 865
      },
      "17823": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 865
      },
      "17824": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 865
      },
      "17825": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 865
      },
      "17826": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 865
      },
      "17827": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 865
      },
      "17828": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 865
      },
      "17829": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 865
      },
      "17830": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 865
      },
      "17831": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 865
      },
      "17832": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 865
      },
      "17833": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 865
      },
      "17834": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 866
      },
      "17835": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 866
      },
      "17836": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 866
      },
      "17837": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 866
      },
      "17838": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 866
      },
      "17839": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 866
      },
      "17840": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 866
      },
      "17841": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 866
      },
      "17842": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 866
      },
      "17843": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 866
      },
      "17844": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 866
      },
      "17845": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 866
      },
      "17846": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 866
      },
      "17847": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 866
      },
      "17848": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 866
      },
      "17849": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 866
      },
      "17850": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 866
      },
      "17851": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 866
      },
      "17852": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 866
      },
      "17853": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 866
      },
      "17854": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 866
      },
      "17855": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 866
      },
      "17856": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 866
      },
      "17857": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 866
      },
      "17858": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 866
      },
      "17859": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 866
      },
      "17860": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 866
      },
      "17861": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 866
      },
      "17862": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 866
      },
      "17863": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 866
      },
      "17864": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 866
      },
      "17865": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 867
      },
      "17866": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 867
      },
      "17867": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 867
      },
      "17868": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 867
      },
      "17869": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 867
      },
      "17881": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 870
      },
      "17882": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 870
      },
      "17883": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 870
      },
      "17884": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 871
      },
      "17885": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 871
      },
      "17890": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 873
      },
      "17891": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 873
      },
      "17892": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 873
      },
      "17893": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 873
      },
      "17894": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 873
      },
      "17895": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 873
      },
      "17896": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 873
      },
      "17897": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 874
      },
      "17898": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 874
      },
      "17899": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 874
      },
      "17900": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 875
      },
      "17901": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 875
      },
      "17902": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 876
      },
      "17903": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 876
      },
      "17904": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 876
      },
      "17905": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 876
      },
      "17906": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 0
      },
      "17907": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 3
      },
      "17908": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 4
      },
      "17913": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 17
      },
      "17914": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 18
      },
      "17992": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 7
      },
      "17994": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 7
      },
      "17996": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 7
      },
      "17998": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 7
      },
      "18000": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 7
      },
      "18002": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 7
      },
      "18004": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 7
      },
      "18006": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 7
      },
      "18008": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 7
      },
      "18010": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 7
      },
      "18012": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 7
      },
      "18014": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 7
      },
      "18016": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 7
      },
      "18018": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 7
      },
      "18020": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 7
      },
      "18022": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 7
      },
      "18024": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 7
      },
      "18026": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 7
      },
      "18028": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 7
      },
      "18030": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 7
      },
      "18032": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 7
      },
      "18034": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 7
      },
      "18036": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 7
      },
      "18038": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 7
      },
      "18088": {
        "lookahead": {
          "type": "Token",
          "name": "EQUALS"
        },
        "like": 33
      },
      "18091": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 18
      },
      "18169": {
        "lookahead": {
          "type": "Token",
          "name": "IS"
        },
        "like": 136
      },
      "18171": {
        "lookahead": {
          "type": "Token",
          "name": "RAISES"
        },
        "like": 136
      },
      "18173": {
        "lookahead": {
          "type": "Token",
          "name": "SATISFIES"
        },
        "like": 136
      },
      "18175": {
        "lookahead": {
          "type": "Token",
          "name": "FUN"
        },
        "like": 136
      },
      "18177": {
        "lookahead": {
          "type": "Token",
          "name": "DATA"
        },
        "like": 136
      },
      "18179": {
        "lookahead": {
          "type": "Token",
          "name": "DATATYPE"
        },
        "like": 136
      },
      "18181": {
        "lookahead": {
          "type": "Token",
          "name": "WHEN"
        },
        "like": 136
      },
      "18183": {
        "lookahead": {
          "type": "Token",
          "name": "VAR"
        },
        "like": 136
      },
      "18185": {
        "lookahead": {
          "type": "Token",
          "name": "NAME"
        },
        "like": 136
      },
      "18187": {
        "lookahead": {
          "type": "Token",
          "name": "CHECK"
        },
        "like": 136
      },
      "18189": {
        "lookahead": {
          "type": "Token",
          "name": "GRAPH"
        },
        "like": 136
      },
      "18191": {
        "lookahead": {
          "type": "Token",
          "name": "NOT"
        },
        "like": 136
      },
      "18193": {
        "lookahead": {
          "type": "Token",
          "name": "PARENSPACE"
        },
        "like": 136
      },
      "18195": {
        "lookahead": {
          "type": "Token",
          "name": "METHOD"
        },
        "like": 136
      },
      "18197": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACE"
        },
        "like": 136
      },
      "18199": {
        "lookahead": {
          "type": "Token",
          "name": "LBRACK"
        },
        "like": 136
      },
      "18201": {
        "lookahead": {
          "type": "Token",
          "name": "IF"
        },
        "like": 136
      },
      "18203": {
        "lookahead": {
          "type": "Token",
          "name": "CASES"
        },
        "like": 136
      },
      "18205": {
        "lookahead": {
          "type": "Token",
          "name": "FOR"
        },
        "like": 136
      },
      "18207": {
        "lookahead": {
          "type": "Token",
          "name": "TRY"
        },
        "like": 136
      },
      "18209": {
        "lookahead": {
          "type": "Token",
          "name": "BLOCK"
        },
        "like": 136
      },
      "18211": {
        "lookahead": {
          "type": "Token",
          "name": "NUMBER"
        },
        "like": 136
      },
      "18213": {
        "lookahead": {
          "type": "Token",
          "name": "DASH"
        },
        "like": 136
      },
      "18215": {
        "lookahead": {
          "type": "Token",
          "name": "TRUE"
        },
        "like": 136
      },
      "18217": {
        "lookahead": {
          "type": "Token",
          "name": "FALSE"
        },
        "like": 136
      },
      "18219": {
        "lookahead": {
          "type": "Token",
          "name": "STRING"
        },
        "like": 136
      },
      "18221": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 136
      },
      "18223": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 136
      },
      "18225": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 136
      },
      "18227": {
        "lookahead": {
          "type": "Token",
          "name": "COLON"
        },
        "like": 136
      },
      "18229": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 136
      },
      "18231": {
        "lookahead": {
          "type": "Token",
          "name": "EXCEPT"
        },
        "like": 136
      },
      "18233": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACK"
        },
        "like": 136
      },
      "18235": {
        "lookahead": {
          "type": "Token",
          "name": "ELSEIF"
        },
        "like": 136
      },
      "18237": {
        "lookahead": {
          "type": "Token",
          "name": "ELSE"
        },
        "like": 136
      },
      "18239": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 136
      },
      "18241": {
        "lookahead": {
          "type": "EOF"
        },
        "like": 136
      },
      "18243": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 136
      },
      "18245": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 136
      },
      "18247": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 136
      },
      "18533": {
        "lookahead": {
          "type": "Token",
          "name": "COMMA"
        },
        "like": 33
      },
      "18535": {
        "lookahead": {
          "type": "Token",
          "name": "FROM"
        },
        "like": 33
      },
      "18537": {
        "lookahead": {
          "type": "Token",
          "name": "RPAREN"
        },
        "like": 33
      },
      "18612": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 211
      },
      "18663": {
        "lookahead": {
          "type": "Token",
          "name": "RBRACE"
        },
        "like": 225
      },
      "18665": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 225
      },
      "18667": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 225
      },
      "18669": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 225
      },
      "18671": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 225
      },
      "18673": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 225
      },
      "18686": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 101
      },
      "18687": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 101
      },
      "18688": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 101
      },
      "18689": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 101
      },
      "18690": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 101
      },
      "18691": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 102
      },
      "18693": {
        "lookahead": {
          "type": "Token",
          "name": "SHARING"
        },
        "like": 102
      },
      "18695": {
        "lookahead": {
          "type": "Token",
          "name": "WHERE"
        },
        "like": 102
      },
      "18697": {
        "lookahead": {
          "type": "Token",
          "name": "END"
        },
        "like": 102
      },
      "18699": {
        "lookahead": {
          "type": "Token",
          "name": "SEMI"
        },
        "like": 102
      },
      "18712": {
        "lookahead": {
          "type": "Token",
          "name": "BAR"
        },
        "like": 17
      }
    },
    "rules": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      220,
      217,
      218,
      219,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246,
      247,
      248,
      249,
      250,
      251,
      252,
      253,
      254,
      255,
      256,
      257,
      258,
      259,
      260,
      261,
      262,
      263,
      264,
      265,
      266,
      267,
      268,
      269,
      270,
      271,
      272,
      273,
      274,
      275,
      276,
      277,
      278,
      279,
      280,
      281,
      282,
      283,
      284,
      285,
      286,
      287,
      288,
      289,
      296,
      290,
      291,
      292,
      293,
      294,
      295,
      297,
      298,
      299,
      300,
      301,
      302,
      303,
      304,
      305,
      306,
      307,
      308,
      309,
      310,
      311,
      312,
      313,
      314,
      315,
      316,
      317,
      318,
      319,
      320,
      321
    ],
    "rnTable": [
      {
        "program": {
          "push": 1
        },
        "prelude": {
          "push": 2
        },
        "prelude_I0_opt": {
          "push": 3
        },
        "prelude_I0": {
          "push": 4
        },
        "provide-stmt": {
          "push": 5
        },
        "prelude_I1": {
          "push": 6
        },
        "import-stmt": {
          "push": 7
        },
        "'IMPORT": {
          "push": 8,
          "reductions": [
            [
              903,
              3
            ]
          ]
        },
        "'NAME": {
          "push": 9,
          "reductions": [
            [
              884,
              2
            ],
            [
              915,
              3
            ]
          ]
        },
        "'STRING": {
          "push": 10,
          "reductions": [
            [
              901,
              2
            ],
            [
              949,
              3
            ]
          ]
        },
        "'PROVIDE": {
          "push": 11
        },
        "stmt": {
          "push": 12
        },
        "block_I0_star": {
          "push": 13
        },
        "block_I0": {
          "push": 14
        },
        "let-expr": {
          "push": 15
        },
        "fun-expr": {
          "push": 16
        },
        "data-expr": {
          "push": 17
        },
        "datatype-expr": {
          "push": 18
        },
        "when-expr": {
          "push": 19
        },
        "var-expr": {
          "push": 20
        },
        "assign-expr": {
          "push": 21
        },
        "check-test": {
          "push": 22
        },
        "check-expr": {
          "push": 23
        },
        "graph-expr": {
          "push": 24
        },
        "binding": {
          "push": 25
        },
        "binop-expr": {
          "push": 26
        },
        "'FUN": {
          "push": 27,
          "reductions": [
            [
              879,
              2
            ],
            [
              904,
              3
            ]
          ]
        },
        "'PARENSPACE": {
          "push": 28,
          "reductions": [
            [
              888,
              2
            ],
            [
              923,
              3
            ]
          ]
        },
        "'CHECK": {
          "push": 29,
          "reductions": [
            [
              885,
              2
            ],
            [
              917,
              3
            ]
          ]
        },
        "'DATA": {
          "push": 30,
          "reductions": [
            [
              880,
              2
            ],
            [
              907,
              3
            ]
          ]
        },
        "'DATATYPE": {
          "push": 31,
          "reductions": [
            [
              881,
              2
            ],
            [
              909,
              3
            ]
          ]
        },
        "'VAR": {
          "push": 32,
          "reductions": [
            [
              883,
              2
            ],
            [
              913,
              3
            ]
          ]
        },
        "'GRAPH": {
          "push": 33,
          "reductions": [
            [
              886,
              2
            ],
            [
              919,
              3
            ]
          ]
        },
        "'WHEN": {
          "push": 34,
          "reductions": [
            [
              882,
              2
            ],
            [
              911,
              3
            ]
          ]
        },
        "binop-clause": {
          "push": 35
        },
        "not-expr": {
          "push": 36
        },
        "expr": {
          "push": 37
        },
        "'NOT": {
          "push": 38,
          "reductions": [
            [
              887,
              2
            ],
            [
              921,
              3
            ]
          ]
        },
        "'DASH": {
          "push": 39,
          "reductions": [
            [
              898,
              2
            ],
            [
              943,
              3
            ]
          ]
        },
        "paren-expr": {
          "push": 40
        },
        "id-expr": {
          "push": 41
        },
        "prim-expr": {
          "push": 42
        },
        "lambda-expr": {
          "push": 43
        },
        "method-expr": {
          "push": 44
        },
        "app-expr": {
          "push": 45
        },
        "left-app-expr": {
          "push": 46
        },
        "obj-expr": {
          "push": 47
        },
        "list-expr": {
          "push": 48
        },
        "dot-expr": {
          "push": 49
        },
        "bracket-expr": {
          "push": 50
        },
        "colon-expr": {
          "push": 51
        },
        "colon-bracket-expr": {
          "push": 52
        },
        "get-bang-expr": {
          "push": 53
        },
        "update-expr": {
          "push": 54
        },
        "extend-expr": {
          "push": 55
        },
        "if-expr": {
          "push": 56
        },
        "cases-expr": {
          "push": 57
        },
        "for-expr": {
          "push": 58
        },
        "try-expr": {
          "push": 59
        },
        "user-block-expr": {
          "push": 60
        },
        "num-expr": {
          "push": 61
        },
        "bool-expr": {
          "push": 62
        },
        "string-expr": {
          "push": 63
        },
        "'NUMBER": {
          "push": 64,
          "reductions": [
            [
              897,
              2
            ],
            [
              941,
              3
            ]
          ]
        },
        "'TRUE": {
          "push": 65,
          "reductions": [
            [
              899,
              2
            ],
            [
              945,
              3
            ]
          ]
        },
        "'FALSE": {
          "push": 66,
          "reductions": [
            [
              900,
              2
            ],
            [
              947,
              3
            ]
          ]
        },
        "'METHOD": {
          "push": 67,
          "reductions": [
            [
              889,
              2
            ],
            [
              925,
              3
            ]
          ]
        },
        "'LBRACE": {
          "push": 68,
          "reductions": [
            [
              890,
              2
            ],
            [
              927,
              3
            ]
          ]
        },
        "'LBRACK": {
          "push": 69,
          "reductions": [
            [
              891,
              2
            ],
            [
              929,
              3
            ]
          ]
        },
        "'IF": {
          "push": 70,
          "reductions": [
            [
              892,
              2
            ],
            [
              931,
              3
            ]
          ]
        },
        "'CASES": {
          "push": 71,
          "reductions": [
            [
              893,
              2
            ],
            [
              933,
              3
            ]
          ]
        },
        "'FOR": {
          "push": 72,
          "reductions": [
            [
              894,
              2
            ],
            [
              935,
              3
            ]
          ]
        },
        "'TRY": {
          "push": 73,
          "reductions": [
            [
              895,
              2
            ],
            [
              937,
              3
            ]
          ]
        },
        "'BLOCK": {
          "push": 74,
          "reductions": [
            [
              896,
              2
            ],
            [
              939,
              3
            ]
          ]
        },
        "$": {
          "accept": true,
          "reductions": [
            [
              17906,
              1
            ],
            [
              17907,
              2
            ],
            [
              17908,
              3
            ]
          ]
        }
      },
      {
        "$": {
          "accept": true
        }
      },
      {
        "block": {
          "push": 75
        },
        "'NAME": {
          "push": 9
        },
        "'STRING": {
          "push": 10
        },
        "stmt": {
          "push": 12
        },
        "block_I0_star": {
          "push": 13
        },
        "block_I0": {
          "push": 14
        },
        "let-expr": {
          "push": 15
        },
        "fun-expr": {
          "push": 16
        },
        "data-expr": {
          "push": 17
        },
        "datatype-expr": {
          "push": 18
        },
        "when-expr": {
          "push": 19
        },
        "var-expr": {
          "push": 20
        },
        "assign-expr": {
          "push": 21
        },
        "check-test": {
          "push": 22
        },
        "check-expr": {
          "push": 23
        },
        "graph-expr": {
          "push": 24
        },
        "binding": {
          "push": 25
        },
        "binop-expr": {
          "push": 26
        },
        "'FUN": {
          "push": 27
        },
        "'PARENSPACE": {
          "push": 28
        },
        "'CHECK": {
          "push": 29
        },
        "'DATA": {
          "push": 30
        },
        "'DATATYPE": {
          "push": 31
        },
        "'VAR": {
          "push": 32
        },
        "'GRAPH": {
          "push": 33
        },
        "'WHEN": {
          "push": 34
        },
        "binop-clause": {
          "push": 35
        },
        "not-expr": {
          "push": 36
        },
        "expr": {
          "push": 37
        },
        "'NOT": {
          "push": 38
        },
        "'DASH": {
          "push": 39
        },
        "paren-expr": {
          "push": 40
        },
        "id-expr": {
          "push": 41
        },
        "prim-expr": {
          "push": 42
        },
        "lambda-expr": {
          "push": 43
        },
        "method-expr": {
          "push": 44
        },
        "app-expr": {
          "push": 45
        },
        "left-app-expr": {
          "push": 46
        },
        "obj-expr": {
          "push": 47
        },
        "list-expr": {
          "push": 48
        },
        "dot-expr": {
          "push": 49
        },
        "bracket-expr": {
          "push": 50
        },
        "colon-expr": {
          "push": 51
        },
        "colon-bracket-expr": {
          "push": 52
        },
        "get-bang-expr": {
          "push": 53
        },
        "update-expr": {
          "push": 54
        },
        "extend-expr": {
          "push": 55
        },
        "if-expr": {
          "push": 56
        },
        "cases-expr": {
          "push": 57
        },
        "for-expr": {
          "push": 58
        },
        "try-expr": {
          "push": 59
        },
        "user-block-expr": {
          "push": 60
        },
        "num-expr": {
          "push": 61
        },
        "bool-expr": {
          "push": 62
        },
        "string-expr": {
          "push": 63
        },
        "'NUMBER": {
          "push": 64
        },
        "'TRUE": {
          "push": 65
        },
        "'FALSE": {
          "push": 66
        },
        "'METHOD": {
          "push": 67
        },
        "'LBRACE": {
          "push": 68
        },
        "'LBRACK": {
          "push": 69
        },
        "'IF": {
          "push": 70
        },
        "'CASES": {
          "push": 71
        },
        "'FOR": {
          "push": 72
        },
        "'TRY": {
          "push": 73
        },
        "'BLOCK": {
          "push": 74
        },
        "$": {
          "reductions": [
            [
              5954,
              5
            ],
            [
              17913,
              5
            ],
            [
              17914,
              6
            ]
          ]
        }
      },
      {
        "prelude_I1_star": {
          "push": 76
        },
        "prelude_I1": {
          "push": 6
        },
        "import-stmt": {
          "push": 7
        },
        "'IMPORT": {
          "push": 8
        },
        "$": {
          "reductions": [
            [
              5978,
              4
            ],
            [
              18038,
              4
            ]
          ]
        },
        "'NAME": {
          "reductions": [
            [
              5960,
              4
            ],
            [
              18002,
              4
            ]
          ]
        },
        "'STRING": {
          "reductions": [
            [
              5977,
              4
            ],
            [
              18036,
              4
            ]
          ]
        },
        "'FUN": {
          "reductions": [
            [
              5955,
              4
            ],
            [
              17992,
              4
            ]
          ]
        },
        "'PARENSPACE": {
          "reductions": [
            [
              5964,
              4
            ],
            [
              18010,
              4
            ]
          ]
        },
        "'CHECK": {
          "reductions": [
            [
              5961,
              4
            ],
            [
              18004,
              4
            ]
          ]
        },
        "'DATA": {
          "reductions": [
            [
              5956,
              4
            ],
            [
              17994,
              4
            ]
          ]
        },
        "'DATATYPE": {
          "reductions": [
            [
              5957,
              4
            ],
            [
              17996,
              4
            ]
          ]
        },
        "'VAR": {
          "reductions": [
            [
              5959,
              4
            ],
            [
              18000,
              4
            ]
          ]
        },
        "'GRAPH": {
          "reductions": [
            [
              5962,
              4
            ],
            [
              18006,
              4
            ]
          ]
        },
        "'WHEN": {
          "reductions": [
            [
              5958,
              4
            ],
            [
              17998,
              4
            ]
          ]
        },
        "'NOT": {
          "reductions": [
            [
              5963,
              4
            ],
            [
              18008,
              4
            ]
          ]
        },
        "'DASH": {
          "reductions": [
            [
              5974,
              4
            ],
            [
              18030,
              4
            ]
          ]
        },
        "'NUMBER": {
          "reductions": [
            [
              5973,
              4
            ],
            [
              18028,
              4
            ]
          ]
        },
        "'TRUE": {
          "reductions": [
            [
              5975,
              4
            ],
            [
              18032,
              4
            ]
          ]
        },
        "'FALSE": {
          "reductions": [
            [
              5976,
              4
            ],
            [
              18034,
              4
            ]
          ]
        },
        "'METHOD": {
          "reductions": [
            [
              5965,
              4
            ],
            [
              18012,
              4
            ]
          ]
        },
        "'LBRACE": {
          "reductions": [
            [
              5966,
              4
            ],
            [
              18014,
              4
            ]
          ]
        },
        "'LBRACK": {
          "reductions": [
            [
              5967,
              4
            ],
            [
              18016,
              4
            ]
          ]
        },
        "'IF": {
          "reductions": [
            [
              5968,
              4
            ],
            [
              18018,
              4
            ]
          ]
        },
        "'CASES": {
          "reductions": [
            [
              5969,
              4
            ],
            [
              18020,
              4
            ]
          ]
        },
        "'FOR": {
          "reductions": [
            [
              5970,
              4
            ],
            [
              18022,
              4
            ]
          ]
        },
        "'TRY": {
          "reductions": [
            [
              5971,
              4
            ],
            [
              18024,
              4
            ]
          ]
        },
        "'BLOCK": {
          "reductions": [
            [
              5972,
              4
            ],
            [
              18026,
              4
            ]
          ]
        }
      },
      {
        "$": {
          "reductions": [
            [
              6003,
              0
            ]
          ]
        },
        "'IMPORT": {
          "reductions": [
            [
              5979,
              0
            ]
          ]
        },
        "'NAME": {
          "reductions": [
            [
              5985,
              0
            ]
          ]
        },
        "'STRING": {
          "reductions": [
            [
              6002,
              0
            ]
          ]
        },
        "'FUN": {
          "reductions": [
            [
              5980,
              0
            ]
          ]
        },
        "'PARENSPACE": {
            [
        }