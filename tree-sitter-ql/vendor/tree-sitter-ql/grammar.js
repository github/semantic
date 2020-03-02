module.exports = grammar({
  name: 'ql',
  conflicts: $ => [
    [$.simpleId, $.className],
    [$.simpleId, $.literalId],
    [$.varDecl, $.returnType],
  ],
  word: $ => $._lower_id,
  extras: $ => [
    /[ \t\r\n]/,
    $.line_comment,
    $.block_comment,
  ],


  rules: {

    ql: $ => repeat($.moduleMember),

    module: $ => seq('module', field('name', $.moduleName), choice(seq("{", repeat($.moduleMember), "}"), $.moduleAliasBody)),

    moduleMember: $ => choice(
      seq(
        repeat($.annotation),
        choice($.imprt, $.classlessPredicate, $.dataclass, $.datatype, $.select, $.module)
      ),
      $.qldoc
    ),

    imprt: $ => seq($.import, $.importModuleExpr, optional(seq($.as, field('name', $.moduleName)))),

    moduleAliasBody: $ => seq($.eq, $.moduleExpr, ";"),
    predicateAliasBody: $ => seq($.eq, $.predicateExpr, ";"),
    typeAliasBody: $ => seq($.eq, $.typeExpr, ";"),

    classlessPredicate: $ => seq(
      $.returnType,
      field('name', $.predicateName),
      choice(
        seq("(", sep($.varDecl, ","), ")", $._optbody),
        $.predicateAliasBody
      )
    ),

    datatype: $ => seq($.newtype, field('name', $.className), $.eq, $.datatypeBranches),
    datatypeBranches: $ => sep1($.datatypeBranch, "or"),

    datatypeBranch: $ => seq(
      optional($.qldoc),
      optional($.annotation),
      field('name', $.className),
      "(",
      sep($.varDecl, ","),
      ")",
      optional($.body)
    ),

    select: $ => seq(
      optional(seq("from", sep($.varDecl, ","))),
      optional(seq("where", $._exprOrTerm)),
      seq('select', $.asExprs, optional($.orderBys))
    ),

    dataclass: $ => seq(
      $.class, field('name', $.className),
      choice(
        seq($.extends, sep1($.typeExpr, ","), "{", repeat($.classMember), "}"),
        $.typeAliasBody
      )
    ),

    classMember: $ => choice(
      seq(
        repeat($.annotation),
        choice($.charpred, $.memberPredicate, $.field)
      ),
      $.qldoc
    ),

    charpred: $ => seq($.className, "(", ")", "{", $._exprOrTerm, "}"),

    memberPredicate: $ => seq($.returnType, field('name', $.predicateName), "(", sep($.varDecl, ","), ")", $._optbody),

    field: $ => seq($.varDecl, ";"),

    _optbody: $ => choice(
      $.empty,
      $.body,
      $.higherOrderTerm
    ),

    empty: $ => ";",

    body: $ => seq("{", $._exprOrTerm, "}"),

    higherOrderTerm: $ => seq(
      $.eq,
      field('name', $.literalId),
      "(",
      sep($.predicateExpr, ","),
      ")",
      "(",
      sep($._call_arg, ","),
      ")"
    ),

    special_call: $ => seq($.specialId, "(", ")"),
    prefix_cast: $ => prec.dynamic(10, seq("(", $.typeExpr, ")", $._exprOrTerm)),
    unary_expr: $ => seq($.unop, $._exprOrTerm),
    mul_expr: $ => prec.left(9, seq(
      field('left', $._exprOrTerm),
      $.mulop,
      field('right', $._exprOrTerm)
    )),
    add_expr: $ => prec.left(8, seq(
      field('left', $._exprOrTerm),
      $.addop,
      field('right', $._exprOrTerm)
    )),
    in_expr: $ => prec.left(7, seq(
      field('left', $._exprOrTerm),
      $.in,
      field('right', $.range)
    )),
    comp_term: $ => prec.left(6, seq(
      field('left', $._exprOrTerm),
      $.compop,
      field('right', $._exprOrTerm)
    )),
    instance_of: $ => prec.left(5, seq($._exprOrTerm, $.instanceof, $.typeExpr)),
    negation: $ => prec.left(4, seq($.not, $._exprOrTerm)),
    if_term: $ => prec.left(3, seq(
      "if", field('cond', $._exprOrTerm),
      "then", field('first', $._exprOrTerm),
      "else", field('second', $._exprOrTerm)
    )),
    conjunction: $ => prec.left(3, seq(
      field('left', $._exprOrTerm),
      "and",
      field('right', $._exprOrTerm)
    )),
    disjunction: $ => prec.left(2, seq(
      field('left', $._exprOrTerm),
      "or",
      field('right', $._exprOrTerm)
    )),
    implication: $ => prec.left(1, seq(
      field('left', $._exprOrTerm),
      "implies",
      field('right', $._exprOrTerm)
    )),

    quantified: $ => seq($.quantifier, "(",
      choice(
        seq(
          sep($.varDecl, ","),
          optional(seq("|", $._exprOrTerm, optional(seq("|", $._exprOrTerm))))
        ),
        $._exprOrTerm
      ),
      ")"),

    specialId: $ => $.none,

    quantifier: $ => choice($.exists, $.forall, $.forex),

    _call_arg: $ => choice(
      $._exprOrTerm,  // ExprArg
      $.underscore  // DontCare
    ),

    memberCall: $ => seq(field('name', $.predicateName), optional($.closure), "(", sep($._call_arg, ","), ")"), //QualCall

    postfixCast: $ => seq("(", $.typeExpr, ")"),                                  // QualCast

    classless_predicate_call: $ => prec.dynamic(10, seq($.aritylessPredicateExpr, optional($.closure), "(", sep($._call_arg, ","), ")")),
    qualified_expr: $ => seq($._primary, ".", choice($.memberCall, $.postfixCast)),
    super_ref: $ => seq(optional(seq($.typeExpr, ".")), $.super),


    // The split here is to ensure that the node is non-empty
    full_aggregate_body: $ => choice(
      seq(sep($.varDecl, ","),
        seq(
          "|",
          optional($._exprOrTerm),
          optional(seq("|", $.asExprs, optional($.orderBys)))
        )
      ),
      sep1($.varDecl, ","),
      ),

    expr_aggregate_body: $ => seq($.asExprs, optional($.orderBys)),

    aggregate: $ => seq($.aggId,                                                                // Agg
      optional(
        seq("[", sep1($._exprOrTerm, ","), "]")
      ),
      "(",
      optional(
        choice($.full_aggregate_body, $.expr_aggregate_body)
      ),
      ")"
    ),
    range: $ => seq(                                                                        // Range
      "[",
      field('lower', $._exprOrTerm), "..", field('upper', $._exprOrTerm),
      "]"
    ),

    par_expr: $ => seq("(", $._exprOrTerm, ")"),


    _exprOrTerm: $ => choice(
      $.special_call,
      $.prefix_cast,
      $._primary,
      $.unary_expr,
      $.mul_expr,
      $.add_expr,
      $.in_expr,
      $.comp_term,
      $.instance_of,
      $.negation,
      $.if_term,
      $.conjunction,
      $.disjunction,
      $.implication,
      $.quantified,                         // QuantifiedTerm
    ),

    _primary: $ => choice(
      $.classless_predicate_call, // PredicateAtomExpr
      $.qualified_expr,                                        // QualifiedExpr
      $.literal,                                                                  // Lit
      $.variable,                                                                 // Var
      $.super_ref,
      $.aggregate,
      $.range,
      $.par_expr                                                 // ParExpr
    ),

    literal: $ => choice(
      $.integer,     // IntLit
      $.float,       // FloatLit
      $.bool,        // BoolLit
      $.string       // StringLit
    ),


    bool: $ => choice($.true, $.false),

    variable: $ => choice($.this, $.result, $.varName),

    compop: $ => choice($.eq, $.ne, $.lt, $.gt, $.le, $.ge),

    unop: $ => choice($.plus, $.minus),

    mulop: $ => choice($.star, $.slash, $.mod),

    addop: $ => choice($.plus, $.minus),

    closure: $ => choice($.star, $.plus),

    direction: $ => choice($.asc, $.desc),

    varDecl: $ => seq($.typeExpr, $.varName),

    asExprs: $ => sep1($.asExpr, ","),

    asExpr: $ => seq($._exprOrTerm, optional(seq($.as, $.varName))),

    orderBys: $ => seq("order", "by", sep1($.orderBy, ",")),

    orderBy: $ => seq($._exprOrTerm, optional($.direction)),

    qldoc: $ => /\/\*\*[^*]*\*+([^/*][^*]*\*+)*\//,

    literalId: $ => choice($._lower_id, $._upper_id),

    annotation: $ => choice(
      field('name', $.annotName),                                  // SimpleAnnotation
      seq(                                                       // ArgsAnnotation
        field('name', $.annotName),
        "[",
        field('args', sep1($.annotArg, ",")),
        "]"
      )
    ),


    annotName: $ => $._lower_id,

    annotArg: $ => choice($.simpleId, $.this, $.result),

    moduleName: $ => field('name', $.simpleId),

    qualModuleExpr: $ => sep1($.simpleId, "."),

    importModuleExpr: $ => seq($.qualModuleExpr, repeat(seq("::", $.simpleId))),

    moduleExpr: $ => choice($.simpleId, seq($.moduleExpr, "::", $.simpleId)),

    typeLiteral: $ => choice($.dbtype, $.boolean, $.date, 'float', 'int', 'string'),

    simpleId: $ => choice($._lower_id, $._upper_id),

    className: $ => $._upper_id,

    dbtype: $ => $._at_lower_id,

    returnType: $ => choice($.predicate, $.typeExpr),

    typeExpr: $ => choice(
      seq(optional(seq($.moduleExpr, "::")), $.className),
      $.typeLiteral
    ),

    predicateName: $ => $._lower_id,

    aritylessPredicateExpr: $ => seq(optional(seq($.moduleExpr, "::")), field('name', $.literalId)),

    predicateExpr: $ => seq($.aritylessPredicateExpr, $.slash, $.integer),

    varName: $ => $.simpleId,

    aggId: $ => choice($.avg, $.concat, $.strictconcat, $.count, $.max, $.min, $.rank, $.strictcount, $.strictsum, $.sum, $.any),

    _upper_id: $ => /[A-Z][A-Za-z0-9_]*/,
    _lower_id: $ => /[a-z][A-Za-z0-9_]*/,
    _at_lower_id: $ => /@[a-z][A-Za-z0-9_]*/,
    integer: $ => /[0-9]+/,
    float: $ => /[0-9]+\.[0-9]+/,
    string: $ => /"([^"\\\r\n\t]|\\["\\nrt])*"/,
    line_comment: $ => /\/\/[^\r\n]*/,
    block_comment: $ => /\/\*([^*]+\*+([^/*][^*]*\*+)*|\*)\//,

    any: $ => 'any',
    as: $ => 'as',
    asc: $ => 'asc',
    avg: $ => 'avg',
    boolean: $ => 'boolean',
    class: $ => 'class',
    newtype: $ => 'newtype',
    count: $ => 'count',
    date: $ => 'date',
    desc: $ => 'desc',
    exists: $ => 'exists',
    extends: $ => 'extends',
    false: $ => 'false',
    forall: $ => 'forall',
    forex: $ => 'forex',
    import: $ => 'import',
    in: $ => 'in',
    instanceof: $ => 'instanceof',
    max: $ => 'max',
    min: $ => 'min',
    not: $ => 'not',
    none: $ => 'none',
    predicate: $ => 'predicate',
    rank: $ => 'rank',
    result: $ => 'result',
    strictcount: $ => 'strictcount',
    strictsum: $ => 'strictsum',
    strictconcat: $ => 'strictconcat',
    concat: $ => 'concat',
    sum: $ => 'sum',
    super: $ => 'super',
    this: $ => 'this',
    true: $ => 'true',

    // symbols
    lt: $ => '<',
    le: $ => '<=',
    eq: $ => '=',
    gt: $ => '>',
    ge: $ => '>=',
    underscore: $ => '_',
    minus: $ => '-',
    ne: $ => '!=',
    slash: $ => '/',
    star: $ => '*',
    mod: $ => '%',
    plus: $ => '+',
  }
});

function sep(rule, s) {
  return optional(sep1(rule, s))
}

function sep1(rule, s) {
  return seq(rule, repeat(seq(s, rule)))
}
