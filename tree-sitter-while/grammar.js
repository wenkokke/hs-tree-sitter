/**
 * @file Parser for the WHILE programming language.
 * @author Wen Kokke <wenkokke@users.noreply.github.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "while",

  precedences: (_) => [
    [
      "multiplication",
      "addition",
      "negation",
      "conjunction",
      "disjunction",
      "sequence",
    ],
  ],

  supertypes: ($) => [
    $.statement,
    $.predicate,
    $.expression,
    $.comparison_operator,
  ],

  rules: {
    source_file: ($) => $.statements,

    statements: ($) => repeat1($.statement),

    statement: ($) =>
      choice(
        $.statement_assignment,
        $.statement_skip,
        $.statement_sequence,
        $.statement_if_then_else,
        $.statement_while_do,
        $.statement_block,
        $.expression,
      ),

    statement_assignment: ($) => seq($.expression_variable, ":=", $.expression),

    statement_skip: (_) => "skip",

    statement_sequence: ($) =>
      prec.right("sequence", seq($.statement, ";", $.statement)),

    statement_if_then_else: ($) =>
      prec.right(
        "sequence",
        seq("if", $.predicate, "then", $.statement, "else", $.statement),
      ),

    statement_while_do: ($) =>
      prec.right("sequence", seq("while", $.predicate, "do", $.statement)),

    statement_block: ($) => seq("{", $.statements, "}"),

    predicate: ($) =>
      choice(
        $.predicate_true,
        $.predicate_false,
        $.predicate_negation,
        $.predicate_conjunction,
        $.predicate_disjunction,
        $.predicate_comparison,
        $.parenthesized_predicate,
      ),

    predicate_true: ($) => "true",

    predicate_false: ($) => "false",

    predicate_negation: ($) => prec("negation", seq("not", $.predicate)),

    predicate_conjunction: ($) =>
      prec.left(
        "conjunction",
        seq(field("left", $.predicate), "and", field("right", $.predicate)),
      ),

    predicate_disjunction: ($) =>
      prec.left(
        "disjunction",
        seq(field("left", $.predicate), "or", field("right", $.predicate)),
      ),

    predicate_comparison: ($) =>
      seq(
        field("left", $.expression),
        $.comparison_operator,
        field("right", $.expression),
      ),

    comparison_operator: ($) => choice($.lt, $.le, $.eq, $.gt, $.ge),

    lt: (_) => "<",
    le: (_) => "<=",
    eq: (_) => "=",
    gt: (_) => ">",
    ge: (_) => ">=",

    parenthesized_predicate: ($) => seq("(", $.predicate, ")"),

    expression: ($) =>
      choice(
        $.expression_variable,
        $.expression_number,
        $.expression_addition,
        $.expression_subtraction,
        $.expression_multiplication,
        $.expression_division,
        $.parenthesized_expression,
      ),

    expression_variable: (_) => /[a-z]+/,

    expression_number: (_) => /[0-9]+/,

    expression_addition: ($) =>
      prec.left(
        "addition",
        seq(field("left", $.expression), "+", field("right", $.expression)),
      ),

    expression_subtraction: ($) =>
      prec.left(
        "addition",
        seq(field("left", $.expression), "-", field("right", $.expression)),
      ),

    expression_multiplication: ($) =>
      prec.left(
        "multiplication",
        seq(field("left", $.expression), "*", field("right", $.expression)),
      ),

    expression_division: ($) =>
      prec.left(
        "multiplication",
        seq(field("left", $.expression), "/", field("right", $.expression)),
      ),

    parenthesized_expression: ($) => seq("(", $.expression, ")"),
  },
});
